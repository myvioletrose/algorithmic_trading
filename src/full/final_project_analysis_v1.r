# creation of df6_analysis
# df for final project analysis

############################################
#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

# tables
sf = "stock_fact"
sd = "stock_dim"
bt = "strategy_final_project_df5_backtest_simplified"
si = "stock_indicator"

tbl_list = sapply(list(sf, sd, bt, si), function(x) paste0(schema, ".", x))
names(tbl_list) = wrapr::qc(sf, sd, bt, si)
sapply(1:length(tbl_list), function(x) assign(names(tbl_list)[x], value = tbl_list[[x]], envir = .GlobalEnv))

##############################################
df6a <- dbGetQuery(con, glue::glue("
select d.company_name
, d.sector
, b.*
from {sd} d
join {bt} b on d.symbol = b.symbol 
"
)) %>%
    dplyr::mutate(cci_comparison = dplyr::case_when(cci_net_value > buy_and_hold_net_value ~1, TRUE ~0),
                  ha_comparison = dplyr::case_when(ha_net_value > buy_and_hold_net_value ~1, TRUE ~0),
                  macd_comparison = dplyr::case_when(macd_net_value > buy_and_hold_net_value ~1, TRUE ~0),
                  rsi_comparison = dplyr::case_when(rsi_net_value > buy_and_hold_net_value ~1, TRUE ~0),
                  rf_v1_comparison = dplyr::case_when(rf_v1_net_value > buy_and_hold_net_value ~1, TRUE ~0),
                  rf_v2_comparison = dplyr::case_when(rf_v2_net_value > buy_and_hold_net_value ~1, TRUE ~0)) %>%
    group_by(symbol) %>%
    dplyr::mutate(sum_better_strategy = sum(c_across(contains("comparison"))),
                  any_better_yn = dplyr::case_when(sum_better_strategy >0 ~1, TRUE ~0)) %>%
    ungroup() %>%
    dplyr::select(symbol, company_name, sector, everything()) %>%
    arrange(symbol)

######################################
# beta_n: covariance(x.pct, spy.pct) / variance(x.pct)
# expected_return_n: risk_free_rate + beta_n * (market_return_n - risk_free_rate)
# alpha_n: actual_return_n - expected_return_n

start_date <- "2018-01-02"
end_date <- "2022-03-30"
risk_free_rate <- 0.02
market_return <- df6a %>%
    dplyr::filter(symbol == 'SPY') %>%
    dplyr::select(min_date_adjusted, max_date_adjusted) %>%
    dplyr::mutate(market_return = (max_date_adjusted - min_date_adjusted)/min_date_adjusted) %>%
    .$market_return
# [1] 0.835852

# market_return <- dbGetQuery(con, 
# glue::glue("
# select (y.max_date_adjusted - x.min_date_adjusted)/x.min_date_adjusted as market_return
# from (
#     select symbol, adjusted as min_date_adjusted 
#     from {sf} 
#     where symbol = 'SPY' 
#     and date = (select min(date) from {sf} where year = 2014)
# ) x
# join (
#     select symbol, adjusted as max_date_adjusted 
#     from {sf} 
#     where symbol = 'SPY' 
#     and date = (select max(date) from {sf} where year = 2017)
# ) y on x.symbol = y.symbol
# ")) %>%
#     .$market_return
# [1] 0.5810762                            

invest_fund = 10000

risk_eval <- dbGetQuery(con, 
glue::glue("
select x.symbol
, x.date 
, x.adjusted
, x.pct
, y.adjusted as adjusted_spy
, y.pct as pct_spy
from {si} x
join (select symbol, date, adjusted, pct from {si} where symbol = 'SPY') y on x.date = y.date 
where x.date between '{start_date}' and '{end_date}'
order by x.symbol, x.date
")
) %>%
    group_by(symbol) %>%
    summarise(beta = stats::cov(pct, pct_spy) / stats::var(pct)) %>%
    ungroup() %>%
    dplyr::mutate(expected_return = risk_free_rate + beta * (market_return - risk_free_rate)) %>%
    dplyr::inner_join(., df6a %>% 
                          dplyr::select(symbol, contains("net_value")) %>%
                          dplyr::mutate(cci_return = cci_net_value / invest_fund,
                                        ha_return = ha_net_value / invest_fund,
                                        macd_return = macd_net_value / invest_fund,
                                        rsi_return = rsi_net_value / invest_fund,
                                        rf_v1_return = rf_v1_net_value / invest_fund,
                                        rf_v2_return = rf_v2_net_value / invest_fund,
                                        buy_and_hold_return = buy_and_hold_net_value / invest_fund) %>%
                          dplyr::select(symbol, contains("return")),
                      by = "symbol") %>%
    dplyr::mutate(alpha_cci = cci_return - expected_return,
                  alpha_ha = ha_return - expected_return,
                  alpha_macd = macd_return - expected_return,
                  alpha_rsi = rsi_return - expected_return,
                  alpha_rf_v1 = rf_v1_return - expected_return,
                  alpha_rf_v2 = rf_v2_return - expected_return,
                  alpha_buy_and_hold = buy_and_hold_return - expected_return) %>%
    arrange(symbol)

df6 <- dplyr::inner_join(df6a, risk_eval, by = "symbol") %>% arrange(symbol)
dim(df6)  # [1] 459 39
str(df6)
# tibble [459 x 39] (S3: tbl_df/tbl/data.frame)
# $ symbol                : chr [1:459] "A" "AAL" "AAP" "AAPL" ...
# $ company_name          : chr [1:459] "Agilent Technologies Inc" "American Airlines Group" "Advance Auto Parts" "Apple Inc." ...
# $ sector                : chr [1:459] "Health Care" "Industrials" "Consumer Discretionary" "Information Technology" ...
# $ min_date              : Date[1:459], format: "2018-01-02" "2018-01-02" "2018-01-02" "2018-01-02" ...
# $ max_date              : Date[1:459], format: "2022-03-30" "2022-03-30" "2022-03-30" "2022-03-30" ...
# $ min_date_adjusted     : num [1:459] 65.4 51.6 102.6 41.1 80.3 ...
# $ max_date_adjusted     : num [1:459] 135.2 18.1 211.8 177.8 163.8 ...
# $ cci_net_value         : num [1:459] 1217.6 -966.2 1759.7 33.5 1719.7 ...
# $ ha_net_value          : num [1:459] 3039 -4504 -1950 6539 4461 ...
# $ macd_net_value        : num [1:459] 2830 -1204 343 2915 3440 ...
# $ rsi_net_value         : num [1:459] 6946 -4350 4299 0 1269 ...
# $ rf_v1_net_value       : num [1:459] 11236 -3315 7886 7487 12763 ...
# $ rf_v2_net_value       : num [1:459] 7200 -3507 3080 4836 10941 ...
# $ avg_net_value         : num [1:459] 5411 -2974 2570 3635 5766 ...
# $ buy_and_hold_net_value: num [1:459] 10670 -6505 10653 33218 10394 ...
# $ cci_comparison        : num [1:459] 0 1 0 0 0 0 0 0 0 0 ...
# $ ha_comparison         : num [1:459] 0 1 0 0 0 0 0 0 0 0 ...
# $ macd_comparison       : num [1:459] 0 1 0 0 0 0 0 0 0 0 ...
# $ rsi_comparison        : num [1:459] 0 1 0 0 0 0 0 0 0 0 ...
# $ rf_v1_comparison      : num [1:459] 1 1 0 0 1 0 0 0 0 0 ...
# $ rf_v2_comparison      : num [1:459] 0 1 0 0 1 0 0 0 0 0 ...
# $ sum_better_strategy   : num [1:459] 1 6 0 0 2 0 0 0 0 0 ...
# $ any_better_yn         : num [1:459] 1 1 0 0 1 0 0 0 0 0 ...
# $ beta                  : num [1:459] 0.546 0.155 0.351 0.498 0.39 ...
# $ expected_return       : num [1:459] 0.465 0.146 0.306 0.426 0.338 ...
# $ cci_return            : num [1:459] 0.12176 -0.09662 0.17597 0.00335 0.17197 ...
# $ ha_return             : num [1:459] 0.304 -0.45 -0.195 0.654 0.446 ...
# $ macd_return           : num [1:459] 0.283 -0.1204 0.0343 0.2915 0.344 ...
# $ rsi_return            : num [1:459] 0.695 -0.435 0.43 0 0.127 ...
# $ rf_v1_return          : num [1:459] 1.124 -0.332 0.789 0.749 1.276 ...
# $ rf_v2_return          : num [1:459] 0.72 -0.351 0.308 0.484 1.094 ...
# $ buy_and_hold_return   : num [1:459] 1.067 -0.651 1.065 3.322 1.039 ...
# $ alpha_cci             : num [1:459] -0.344 -0.243 -0.13 -0.423 -0.166 ...
# $ alpha_ha              : num [1:459] -0.161 -0.597 -0.501 0.228 0.108 ...
# $ alpha_macd            : num [1:459] -0.18234 -0.26677 -0.27187 -0.13453 0.00593 ...
# $ alpha_rsi             : num [1:459] 0.229 -0.581 0.124 -0.426 -0.211 ...
# $ alpha_rf_v1           : num [1:459] 0.658 -0.478 0.482 0.323 0.938 ...
# $ alpha_rf_v2           : num [1:459] 0.25468 -0.49708 0.00182 0.05764 0.75607 ...
# $ alpha_buy_and_hold    : num [1:459] 0.602 -0.797 0.759 2.896 0.701 ...

###### write table
# df6 - final project analysis
tbl_name = "strategy_final_project_df6_analysis"
tbl = paste0(schema, ".", tbl_name)
tbl

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl), df6, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl}"))

# disconnect db
dbDisconnect(con) 

###################################################################################################
###################### analysis, visualization begins here ######################################################

######################################
# what is the best strategy?
# what is the best strategy by sector?
######################################

df6 <- df6 %>% dplyr::filter(symbol != 'SPY')
windows()

##############################
# how's alpha by strategy?
dfGather <- df6 %>%
    dplyr::select(contains("alpha")) %>%
    tidyr::gather() %>%
    dplyr::mutate(key = gsub("alpha_", "", key)) %>%
    dplyr::select(strategy = key, alpha = value) 

# density plot - alpha
dfGather %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(alpha, color = strategy)) +
    geom_density() +
    theme_bw() +
    theme(legend.position = "top") +
    geom_vline(data = aggregate(alpha ~ strategy, 
                                dfGather %>% dplyr::filter(strategy != 'ha'), 
                                mean), 
               aes(xintercept = alpha,
                   color = strategy),
               linetype = "dashed") +
    facet_wrap(~ strategy, 
               nrow = 2) +
    ggtitle("alpha")

# how's return by strategy?
dfGather2 <- df6 %>%
    dplyr::select(contains("_return")) %>%
    dplyr::select(-expected_return) %>%
    tidyr::gather() %>%
    dplyr::mutate(key = gsub("_return", "", key)) %>%
    dplyr::select(strategy = key, return = value) 

# density plot - return
dfGather2 %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(return, color = strategy)) +
    geom_density() +
    theme_minimal() +
    theme(legend.position = "top") +
    geom_vline(data = aggregate(return ~ strategy, 
                                dfGather2 %>% dplyr::filter(strategy != 'ha'), 
                                mean), 
               aes(xintercept = return,
                   color = strategy),
               linetype = "dashed") +
    facet_wrap(~ strategy,
               nrow = 6) +
    ggtitle("return")

##############################
# correlation alpha, beta, expected return, return
df6 %>%
    #dplyr::select(beta:alpha_buy_and_hold) %>%
    dplyr::select(beta, contains("return")) %>%
    cor() %>%
    corrplot(method = 'number', type = 'upper')

##############################
# alpha - by sector
sectorGather <- df6 %>%
    dplyr::select(sector, contains("alpha")) %>%
    tidyr::gather(key, value, -sector) %>%
    dplyr::mutate(key = gsub("alpha_", "", key)) %>%
    dplyr::select(sector, strategy = key, alpha = value) 

# return - by sector
sectorGather2 <- df6 %>%
    dplyr::select(sector, contains("_return")) %>%
    dplyr::select(-expected_return) %>%
    tidyr::gather(key, value, -sector) %>%
    dplyr::mutate(key = gsub("_return", "", key)) %>%
    dplyr::select(sector, strategy = key, return = value) 

##############################
# boxplot by strategy - alpha
dfGather %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(x = strategy, y = alpha)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 2, notch = FALSE) +
    #coord_flip() +
    theme_bw() +
    ggtitle("alpha") +
    xlab("") +
    ylab("")

# boxplot by strategy - return
dfGather2 %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(x = strategy, y = return)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 2, notch = FALSE) +
    coord_flip() +
    theme_bw() +
    ggtitle("return") +
    xlab("") +
    ylab("")

# boxplot by sector - various alpha
sectorGather %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(x = sector, y = alpha)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 1, notch = FALSE) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sectorGather$sector %>% as.factor))) +
    facet_wrap(~strategy) +
    theme_bw() +
    ggtitle("alpha") +
    xlab("") +
    ylab("")

# boxplot by sector - various return
sectorGather2 %>%
    dplyr::filter(strategy != 'ha') %>%
    ggplot(aes(x = sector, y = return)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 1, notch = FALSE) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sectorGather$sector %>% as.factor))) +
    facet_wrap(~strategy) +
    theme_bw() +
    ggtitle("return") +
    xlab("") +
    ylab("")

##############################
### beta vs alph | return
betaGather_alpha <- df6 %>%
    dplyr::select(sector, beta, contains("alpha")) %>%
    tidyr::gather(key, value, -sector, -beta) %>%
    dplyr::mutate(key = gsub("alpha_", "", key)) %>%
    dplyr::mutate(type = "alpha") %>%
    dplyr::select(sector, strategy = key, type, beta, value)

betaGather_return <- df6 %>%
    dplyr::select(sector, beta, contains("return"), -expected_return) %>%
    tidyr::gather(key, value, -sector, -beta) %>%
    dplyr::mutate(key = gsub("_return", "", key)) %>%
    dplyr::mutate(type = "return") %>%
    dplyr::select(sector, strategy = key, type, beta, value)

betaGather <- dplyr::bind_rows(betaGather_alpha, betaGather_return)

# scatterplot: beta vs alpha - by strategy
betaGather %>%
    dplyr::filter(type == 'alpha' & strategy != 'ha') %>%
    dplyr::select(sector, strategy, beta, alpha = value) %>%
    ggplot(aes(x = beta, y = alpha,
               color = sector)) +
    geom_point() +
    geom_smooth(method = lm,  linetype = "dashed",
                color = "darkred", fill = "blue") +
    facet_wrap(~strategy) +
    ggtitle("alpha")

# scatterplot: beta vs return - by strategy 
betaGather %>%
    dplyr::filter(type == 'return' & strategy != 'ha') %>%
    dplyr::select(sector, strategy, beta, return = value) %>%
    ggplot(aes(x = beta, y = return,
               color = sector)) +
    geom_smooth(method = lm,  linetype = "dashed",
                color = "darkred", fill = "blue") +
    geom_point() +
    facet_wrap(~strategy) +
    ggtitle("return")

##############################
# performance table - count comparing to buy_and_hold strategy
performance <- df6 %>% dplyr::select(sector, contains("comparison")) %>%
    tidyr::gather(key, value, -sector) %>%
    dplyr::mutate(key = gsub("_comparison", "", key)) %>%
    dplyr::select(sector, strategy = key, comparison = value) %>%
    group_by(sector, strategy) %>%
    summarise(count = n(),
              out_perform = sum(comparison)) %>%
    dplyr::mutate(pct = round(out_perform / count, 4))

performance

performance2 <- performance %>%
    dplyr::select(sector, strategy, num_of_stock = count, pct) %>%
    tidyr::spread(strategy, pct) %>%
    ungroup()

performance2

############################################################
############################################################
############################################################
# descriptive stat

# by strategy - alpha
aggregate(alpha ~ strategy, dfGather, mean) %>% arrange(desc(alpha))

# by strategy - return
aggregate(return ~ strategy, dfGather2, mean) %>% arrange(desc(return))

# by sector + strategy - alpha
aggregate(alpha ~ sector + strategy, sectorGather, mean) %>%
    as.data.frame() %>%
    tidyr::spread(strategy, alpha)

# by sector + strategy - return
aggregate(return ~ sector + strategy, sectorGather2, mean) %>%
    as.data.frame() %>%
    tidyr::spread(strategy, return)

##############################
# ANOVA - strategy
one.way.alpha <- aov(alpha ~ strategy, data = dfGather)
summary(one.way.alpha)
tukey.test.alpha <- TukeyHSD(one.way.alpha)
broom::tidy(tukey.test.alpha)

one.way.return <- aov(return ~ strategy, data = dfGather2)
summary(one.way.return)
tukey.test.return <- TukeyHSD(one.way.return)
broom::tidy(tukey.test.return)

##############################
# ANOVA - strategy + sector
list_of_sectors = unique(df6$sector)
tidyResult_list_alpha <- vector(mode = "list", length = length(list_of_sectors))
tidyResult_list_return <- vector(mode = "list", length = length(list_of_sectors))

for(i in 1:length(list_of_sectors)){
    aov.alpha <- aov(alpha ~ strategy, data = sectorGather %>% 
                         dplyr::filter(sector == list_of_sectors[i]))
    
    tidyResult_list_alpha[[i]] <- broom::tidy(TukeyHSD(aov.alpha)) %>% dplyr::mutate(sector = list_of_sectors[i])
    
    aov.return <- aov(return ~ strategy, data = sectorGather2 %>% 
                          dplyr::filter(sector == list_of_sectors[i]))
    
    tidyResult_list_return[[i]] <- broom::tidy(TukeyHSD(aov.return)) %>% dplyr::mutate(sector = list_of_sectors[i])
}

tidyResult_df <- rbind(tidyResult_list_alpha %>%
                           dplyr::bind_rows() %>%
                           dplyr::mutate(measure = "alpha"),
                       tidyResult_list_return %>%
                           dplyr::bind_rows() %>%
                           dplyr::mutate(measure = "return")) %>%
    tidyr::separate(col = "contrast", into = c("s1", "s2"), sep = "-") %>%
    dplyr::mutate(sig_flag = dplyr::case_when(adj.p.value < .05 ~1, TRUE~0),
                  is_s1_better = dplyr::case_when(estimate>0 & sig_flag ==1 ~1, 
                                                  estimate<0 & sig_flag ==1 ~-1,
                                                  TRUE~0)) %>%
    dplyr::select(-term, -null.value)

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
################################################## what to report, show in the paper! ##################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

# mapper - only report these strategies
strategy = c("cci", "macd", "rf_v1", "rsi")
strategy_name = c("CCI", "MACD", "Random Forest", "RSI")
mapper = data.frame(strategy, strategy_name) %>%
    dplyr::mutate(strategy_name = factor(strategy_name, levels = c("CCI", "MACD", "RSI", "Random Forest")))

# mapper2 - only report these strategies
strategy2 = c("cci", "macd", "rf_v1", "rsi", "buy_and_hold")
strategy_name2 = c("CCI", "MACD", "Random Forest", "RSI", "Buy-and-Hold")
mapper2 = data.frame(strategy = strategy2, strategy_name = strategy_name2) %>%
    dplyr::mutate(strategy_name = factor(strategy_name2, levels = c("CCI", "MACD", "RSI", "Random Forest", "Buy-and-Hold")))

#######################################################
# boxplot by strategy - return [y]
dfGather2 %>%
    dplyr::inner_join(., mapper2, by = "strategy") %>%
    #dplyr::select(strategy, strategy_name) %>% distinct()
    ggplot(aes(x = strategy_name, y = return, fill = strategy_name)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 2, notch = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "red", size = 1) +
    #coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    #ggtitle("Actual Return (%) by Strategy") +
    xlab("") +
    ylab("") ->b1

ggsave(filename = "boxplot_actual_return_by_strategy.png", plot = b1, width = 12, height = 8)

# boxplot by sector - various return [y]
sectorGather2 %>%
    dplyr::inner_join(., mapper2, by = "strategy") %>%
    ggplot(aes(x = strategy_name, y = return, fill = strategy_name)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 1, notch = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "red", size = 1) +
    #coord_flip() +
    #scale_x_discrete(limits = rev(levels(sectorGather$sector %>% as.factor))) +
    #facet_wrap(~strategy_name, nrow = 1) +
    facet_wrap(~sector, ncol = 11) +
    theme_bw() +
    theme(legend.position = "none", 
          strip.text.x = element_text(size = 7.5),
          #plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(hjust = 1, angle = 45),
          legend.background = element_rect(colour = "black"),
          legend.title = element_blank()) +
    #ggtitle("Actual Return (%) by Sector, Strategy") +
    xlab("") +
    ylab("") -> b2

ggsave(filename = "boxplot_actual_return_by_sector_strategy.png", plot = b2, width = 15, height = 8)

# boxplot by sector - various return
sectorGather2 %>%
    dplyr::inner_join(., mapper, by = "strategy") %>%
    ggplot(aes(x = sector, y = return)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 1, notch = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "red", size = 1) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sectorGather$sector %>% as.factor))) +
    facet_wrap(~strategy_name) +
    theme_bw() +
    ggtitle("Actual Return (%) by Sector, Strategy") +
    xlab("") +
    ylab("")

############################################################################################
# quick stat performance vs buy_and_hold
performance3 <- performance %>%
    dplyr::select(sector, strategy, num_of_stock = count, pct) %>%
    dplyr::inner_join(., mapper, by = "strategy") %>%
    dplyr::select(-strategy) %>%
    tidyr::spread(strategy_name, pct) %>%
    ungroup()

performance3 %>% mutate_if(is.numeric, function(x) round(x, 2))

# chi.sq test?
# chi.sq.test = df6 %>% dplyr::select(sector, contains("comparison")) %>%
#     tidyr::gather(key, value, -sector) %>%
#     dplyr::mutate(key = gsub("_comparison", "", key)) %>%
#     dplyr::select(sector, strategy = key, comparison = value)
# 
# with(chi.sq.test, table(strategy, comparison))

############################################################################################
# descriptive stat

# return by sector [y]
actual_return_by_sector <- sectorGather2 %>%
    dplyr::filter(strategy == "buy_and_hold") %>%
    group_by(sector) %>%
    summarise(n = n(),
              mean = mean(return),
              sd = sd(return),
              se = sd / sqrt(n),
              lower.bound = mean - 1.96*se,
              upper.bound = mean + 1.96*se,
              minimum = min(return),
              maximum = max(return))%>%
    ungroup()

actual_return_by_sector %>% mutate_if(is.numeric, function(x) round(x, 2))   

# return by strategy [y]
return_by_strategy <- sectorGather2 %>%
    dplyr::inner_join(., mapper2, by = "strategy") %>%
    dplyr::select(-strategy, -sector) %>%
    group_by(strategy_name) %>%
    summarise(n = n(),
              mean = mean(return),
              sd = sd(return),
              se = sd / sqrt(n),
              lower.bound = mean - 1.96*se,
              upper.bound = mean + 1.96*se,
              minimum = min(return),
              maximum = max(return))%>%
    ungroup()

return_by_strategy %>% mutate_if(is.numeric, function(x) round(x, 2))

# return by strategy + sector [y]
return_by_strategy2 <- sectorGather2 %>%
    dplyr::inner_join(., mapper2, by = "strategy") %>%
    dplyr::select(-strategy) %>%
    group_by(strategy_name, sector) %>%
    summarise(n = n(),
              mean = mean(return),
              sd = sd(return),
              se = sd / sqrt(n),
              lower.bound = mean - 1.96*se,
              upper.bound = mean + 1.96*se,
              minimum = min(return),
              maximum = max(return))%>%
    ungroup()

return_by_strategy2 %>% mutate_if(is.numeric, function(x) round(x, 2))

#######################################################
# ANOVA - strategy
anova_strategy <- aov(return ~ strategy_name, data = dfGather2 %>% 
                          dplyr::inner_join(., mapper2, by = "strategy"))

summary(anova_strategy)

postHoc_strategy <- TukeyHSD(anova_strategy)

# anova result
broom::tidy(postHoc_strategy) %>% select(-term, -null.value) %>% 
    dplyr::mutate(sig_flag = dplyr::case_when(adj.p.value < .05 ~1, TRUE~0)) %>%
    arrange(desc(sig_flag), desc(estimate))

#######################################################
# ANOVA - strategy by sector
tic()
list_of_sectors = unique(df6$sector)
tidyResult_list_alpha2 <- vector(mode = "list", length = length(list_of_sectors))
tidyResult_list_return2 <- vector(mode = "list", length = length(list_of_sectors))

for(i in 1:length(list_of_sectors)){
    aov.alpha <- aov(alpha ~ strategy_name, data = sectorGather %>%
                         dplyr::inner_join(., mapper2, by = "strategy") %>%
                         dplyr::select(-strategy) %>%
                         dplyr::filter(sector == list_of_sectors[i]))
    
    tidyResult_list_alpha2[[i]] <- broom::tidy(TukeyHSD(aov.alpha)) %>% dplyr::mutate(sector = list_of_sectors[i])
    
    aov.return <- aov(return ~ strategy_name, data = sectorGather2 %>%
                          dplyr::inner_join(., mapper2, by = "strategy") %>%
                          dplyr::select(-strategy) %>%
                          dplyr::filter(sector == list_of_sectors[i]))
    
    tidyResult_list_return2[[i]] <- broom::tidy(TukeyHSD(aov.return)) %>% dplyr::mutate(sector = list_of_sectors[i])
}

tidyResult_df2 <- rbind(tidyResult_list_alpha2 %>%
                            dplyr::bind_rows() %>%
                            dplyr::mutate(measure = "alpha"),
                        tidyResult_list_return2 %>%
                            dplyr::bind_rows() %>%
                            dplyr::mutate(measure = "return")) %>%
    tidyr::separate(col = "contrast", into = c("s1", "s2"), sep = "-") %>%
    dplyr::mutate(sig_flag = dplyr::case_when(adj.p.value < .05 ~1, TRUE~0),
                  marginal_flag = dplyr::case_when(adj.p.value < .1 ~1, TRUE~0),
                  is_s1_sig_better = dplyr::case_when(estimate>0 & sig_flag ==1 ~1, 
                                                      estimate<0 & sig_flag ==1 ~-1,
                                                      TRUE~0)) %>%
    dplyr::select(-term, -null.value)
toc()

# anova result
tidyResult_df2 %>% 
    #dplyr::filter(sig_flag == 1) %>%
    #dplyr::filter(marginal_flag == 1) %>% 
    dplyr::filter(measure == "return") %>%
    dplyr::arrange(desc(estimate))
































