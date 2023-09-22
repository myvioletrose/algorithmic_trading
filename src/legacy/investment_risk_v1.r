### 30/100

##################################################################
################################################################################

# date 
# symbol
# risk_free_rate
# 
# market_return_n
# actual_return_n
# 
# sharpe_ratio_n: Average(pct - risk free rate) / sd (pct - risk free rate)
# beta_n: covariance(x.pct, spy.pct) / variance(x.pct)
# nlcor_n: nlcor(x.pct, spy.pct)
# nlcor_pvalue_n
# r_n
# r_pvalue_n
# 
# r_squared_n
# expected_return_n: risk_free_rate + beta_n * (market_return_n - risk_free_rate)
# alpha_n: actual_return_n - expected_return_n

###########################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

#############################################################################

# get vector of symbols
get_symbols_query = glue::glue("
with sub
as (
	select symbol, count(1)
	, min(date) as min_date
	, max(date) as max_date
	from stock.stock_fact
	group by symbol
)

select symbol
from sub
where extract(year from min_date) <=2013
and extract(year from max_date) = 2022
and symbol != 'HLT'
order by symbol
")

get_symbols_df = dbGetQuery(con, get_symbols_query)

########################################################################################

source_tbl = "stock.stock_fact"
n = 30  # number of look back days
    
tic()

dfRisk_a = dbGetQuery(con, statement = glue::glue("
with spy
as (
	select date, 
	symbol,
	adjusted,
	lag(adjusted, 1) over(partition by symbol order by date) as adjusted_lag_1,
	lag(adjusted, {n}-1) over(partition by symbol order by date) as adjusted_lag_n
	from {source_tbl}
	where symbol = 'SPY'
), 

symbols
as (
	select date, 
	symbol,
	adjusted,
	lag(adjusted, 1) over(partition by symbol order by date) as adjusted_lag_1,
	lag(adjusted, {n}-1) over(partition by symbol order by date) as adjusted_lag_n
	from {source_tbl}
	--where symbol != 'SPY'
)

select s.date 
, s.symbol 

, s.adjusted 
, (s.adjusted - s.adjusted_lag_1) / s.adjusted_lag_1 as pct
, s.adjusted_lag_n
, (s.adjusted - s.adjusted_lag_n) / s.adjusted_lag_n as actual_return_n

, sp.adjusted as market_adjusted
, (sp.adjusted - sp.adjusted_lag_1) / sp.adjusted_lag_1 as market_pct
, sp.adjusted_lag_n as market_adjusted_lag_n
, (sp.adjusted - sp.adjusted_lag_n) / sp.adjusted_lag_n as market_return_n

from symbols s
left join spy sp on s.date = sp.date
order by symbol, date
"))

toc()

# disconnect db
#dbDisconnect(con) 

dfRisk_a %>% dim()

# backup
dfRisk_a_backup = dfRisk_a
names(dfRisk_a_backup) = gsub(pattern = "*_n", replacement = paste0("_n_", n), names(dfRisk_a_backup))

# nested df
dfRisk_a_nested <- dfRisk_a %>%
    group_by(symbol) %>%
    tidyr::nest() %>%
    dplyr::inner_join(., get_symbols_df %>%
                          dplyr::arrange(symbol) %>%
                          dplyr::mutate(i = row_number()), 
                      by = "symbol") %>%
    arrange(symbol)

###########################################################################################
tic()

# detect, use multicores
numCores <- parallel::detectCores()

# create a simple cluster on the local machine using all available threads
cl <- parallel::makeCluster(detectCores(), methods = FALSE)

# register our cluster
doParallel::registerDoParallel(cl)

trial = "30_v2"
length_of_trial = 259
start = 201
end = 459
risk_free_rate = 0.02

dfRisk_b_temp <- vector(mode = "list", length_of_trial)
symbols <- dfRisk_a_nested %>%
    dplyr::filter(i >= start & i <= end) %>%
    dplyr::select(symbol) %>%
    .$symbol

#writeLines(c(""), "investment_risk_log.txt")

# set up progress bar
doSNOW::registerDoSNOW(cl)
pb <- txtProgressBar(max = length_of_trial, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)

# initiate parallel processing
#nrow(dfRisk_a_nested)  # [1] 459
dfRisk_b_temp = foreach::foreach(j = 1:length_of_trial, 
                                 .combine = rbind,
                                 .packages = c('dplyr', 'tidyr'),
                                 .options.snow = opts,
                                 .verbose = TRUE) %dopar% {
    
    #sink("investment_risk_log.txt", append = TRUE)
    
    rolling_window_risk(dfRisk_a_nested %>%
                            dplyr::filter(symbol == symbols[j]) %>%
                            unnest(cols = c(data)) %>%
                            ungroup(),
                        rolling_windows = n,
                        dataPartition = 2000,
                        risk_free_rate = risk_free_rate,
                        col_a = "pct",
                        col_b = "market_pct",
                        col_c = "market_return_n",
                        col_d = "actual_return_n")

}

# consolidate the list into a df
assign(paste0("dfRisk_b_", trial), value = dfRisk_b_temp, envir = .GlobalEnv)

cat(paste0(rep("#", 100), collapse = ""))
cat(paste0("Finished <<<<< ", trial, " >>>>> ", Sys.time()))
cat(paste0("dfRisk_b_", trial))
eval(sym(paste0("dfRisk_b_", trial))) %>% dim()

# output as csv
dfRisk_output <- dplyr::inner_join(dfRisk_a_backup, 
                                   dfRisk_b_temp %>% dplyr::select(-market_return_n, -actual_return_n), 
                                   by = c("date", "symbol"))

readr::write_csv(dfRisk_output, file = paste0("dfRisk_b_", trial, ".csv"))

# stop the cluster
parallel::stopCluster(cl)

toc()

##########################
# save output

# lookback 100 day
#dfRisk_b_v1, i between 1 and 30
#dfRisk_b_v2, i between 31 and 70
#dfRisk_b_v3, i between 101 and 200
#dfRisk_b_v4, i between 201 and 300
#dfRisk_b_v5, i between 301 and 459

# lookback 30 day
#dfRisk_b_30_v1, i between 1 and 200
#dfRisk_b_30_v2, i between 201 and 459

############################
# combine tables together

# start timer
tic()

# read data - lookback period = 100
dfRisk_b_v1 = readr::read_csv("dfRisk_b_v1.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 100)

dfRisk_b_v2 = readr::read_csv("dfRisk_b_v2.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 100)

dfRisk_b_v3 = readr::read_csv("dfRisk_b_v3.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 100)

dfRisk_b_v4 = readr::read_csv("dfRisk_b_v4.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 100)

dfRisk_b_v5 = readr::read_csv("dfRisk_b_v5.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 100)

df_invest_risk_a = dplyr::bind_rows(dfRisk_b_v1,
                                    dfRisk_b_v2,
                                    dfRisk_b_v3,
                                    dfRisk_b_v4,
                                    dfRisk_b_v5)

# change column names, i.e., replace *_n_100 to lookback
names(df_invest_risk_a) = gsub(pattern = "n_100|n_30", replacement = "lookback", names(df_invest_risk_a))

dim(df_invest_risk_a)

################################################

# read data - lookback period = 30
dfRisk_b_30_v1 = readr::read_csv("dfRisk_b_30_v1.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 30)

dfRisk_b_30_v2 = readr::read_csv("dfRisk_b_30_v2.csv") %>%
    dplyr::mutate(risk_free_rate = 0.02,
                  lookback_period = 30)

df_invest_risk_b = dplyr::bind_rows(dfRisk_b_30_v1,
                                    dfRisk_b_30_v2)

# change column names, i.e., replace *_n_100 to lookback
names(df_invest_risk_b) = gsub(pattern = "n_100|n_30", replacement = "lookback", names(df_invest_risk_b))

dim(df_invest_risk_b)

####################

# stack up the two combined dfs
df_invest_risk = dplyr::bind_rows(df_invest_risk_a,
                                  df_invest_risk_b)

dim(df_invest_risk)

# sanity check
dim(df_invest_risk_a) == dim(df_invest_risk_b)  # [1] TRUE TRUE

colSums(is.na(df_invest_risk))

sqldf("select date, symbol, lookback_period, count(1)
      from df_invest_risk
      group by 1, 2, 3
      having count(1)>1
      limit 5")

# end timer
toc()

#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

# write table
tbl_name = "investment_risk"
tbl = paste0(schema, ".", tbl_name)
DBI::dbWriteTable(con, DBI::SQL(tbl), df_invest_risk, overwrite = TRUE)

# data validation
query = glue::glue("
select count(1)
from {tbl}
")

dbGetQuery(con, query)

query = glue::glue("
select *
from {tbl}
where symbol = 'SPY'
order by lookback_period, symbol, date
")

dfSpy = dbGetQuery(con, query)

write.table(dfSpy, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)

# disconnect db
dbDisconnect(con) 












