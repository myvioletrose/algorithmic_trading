# creation of df1

#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

# disconnect db
#dbDisconnect(con) 

# tables
sf = "stock_fact"
sd = "stock_dim"
si = "stock_indicator"
haf = "heikin_ashi_fact"
ir = "investment_risk"

tbl_list = sapply(list(sf, sd, si, haf, ir), function(x) paste0(schema, ".", x))
names(tbl_list) = wrapr::qc(sf, sd, si, haf, ir)
sapply(1:length(tbl_list), function(x) assign(names(tbl_list)[x], value = tbl_list[[x]], envir = .GlobalEnv))

query = glue::glue("
select * from {si}
limit 5
")

df = dbGetQuery(con, query)
x = names(df) %>% as.data.frame

#write.table(x, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)

##########################################################################################################################
query1 = glue::glue("
with sf
as (
	select symbol, 
	date, 
	year, 
	quarter, 
	month, 
	weekday, 
	day, 
	open, 
	high, 
	low, 
	close, 
	volume, 
	adjusted 
	from {sf}
), 

sd 
as (
	select symbol, 
	company_name, 
	sector
	from {sd}
),

si 
as (
	select symbol, 
	date, 
	adjusted, 
	adj_lag_1, 
	adj_lag_2, 
	intraday_volatility, 
	cci, 
	rsi, 
	macd, 
	macd_signal, 
	obv, 
	pct2, 
	sma_5, 
	ema_10, 
	ema_20, 
	ema_30, 
	ema_40, 
	ema_50, 
	ema_100, 
	ema_200, 
	target_flag, 
	target_var, 
	temp_flag, 
	pct, 
	pct_mu_100, 
	pct_sd_100, 
	pct_percentile_100
	from {si}
),

haf 
as (
	select symbol, 
	cast(date as date) as date, 
	open_ha, 
	high_ha, 
	low_ha, 
	close_ha, 
	intraday_volatility_ha, 
	intraday_volatility_ha_mu_100, 
	intraday_volatility_ha_sd_100, 
	intraday_volatility_ha_percentile_100
	from {haf}
),

ir30
as (
	select date, 
	symbol, 
	adjusted_lag_lookback as adjusted_lag_n30, 
	actual_return_lookback as actual_return_n30, 	
	market_adjusted_lag_lookback as market_adjusted_lag_n30, 
	market_return_lookback as market_return_n30, 
	sharpe_ratio_lookback as sharpe_ratio_n30, 
	pct_sd_lookback as pct_sd_n30, 
	beta_lookback as beta_n30, 
	nlcor_lookback as nlcor_n30, 
	nlcor_pvalue_lookback as nlcor_pvalue_n30, 
	r_lookback as r_n30, 
	r_pvalue_lookback as r_pvalue_n30, 
	r_squared_lookback as r_squared_n30, 
	expected_return_lookback as expected_return_n30, 
	alpha_lookback as alpha_n30
	from {ir}
	where lookback_period = 30
),

ir100
as (
	select date, 
	symbol, 
	adjusted_lag_lookback as adjusted_lag_n100, 
	actual_return_lookback as actual_return_n100, 	
	market_adjusted_lag_lookback as market_adjusted_lag_n100, 
	market_return_lookback as market_return_n100, 
	sharpe_ratio_lookback as sharpe_ratio_n100, 
	pct_sd_lookback as pct_sd_n100, 
	beta_lookback as beta_n100, 
	nlcor_lookback as nlcor_n100, 
	nlcor_pvalue_lookback as nlcor_pvalue_n100, 
	r_lookback as r_n100, 
	r_pvalue_lookback as r_pvalue_n100, 
	r_squared_lookback as r_squared_n100, 
	expected_return_lookback as expected_return_n100, 
	alpha_lookback as alpha_n100
	from {ir}
	where lookback_period = 100
),

part1
as (
	select sf.symbol, 
	sd.company_name, 
	sd.sector,
	sf.date, 
	sf.year, 
	sf.quarter, 
	sf.month, 
	sf.weekday, 
	sf.day, 

	sf.open, 
	sf.high, 
	sf.low, 
	sf.close, 
	sf.volume, 
	sf.adjusted, 

	s.open as market_open, 
	s.high as market_high, 
	s.low as market_low, 
	s.close as market_close, 
	s.volume as market_volume, 
	s.adjusted as market_adjusted,

	si.adj_lag_1, 
	si.adj_lag_2, 
	si.cci, 
	si.rsi, 
	si.macd, 
	si.macd_signal, 
	si.obv, 	 
	si.sma_5, 
	si.ema_10, 
	si.ema_20, 
	si.ema_30, 
	si.ema_40, 
	si.ema_50, 
	si.ema_100, 
	si.ema_200, 	
	si.target_flag, 
	si.target_var, 
	si.temp_flag, 
	si.pct, 
	si.pct_mu_100, 
	si.pct_sd_100, 
	si.pct_percentile_100,
	si.pct2,
	si.intraday_volatility,

	si2.adj_lag_1 as market_adj_lag_1, 
	si2.adj_lag_2 as market_adj_lag_2, 
	si2.cci as market_cci, 
	si2.rsi as market_rsi, 
	si2.macd as market_macd, 
	si2.macd_signal as market_macd_signal, 
	si2.obv as market_obv, 	 
	si2.sma_5 as market_sma_5, 
	si2.ema_10 as market_ema_10, 
	si2.ema_20 as market_ema_20, 
	si2.ema_30 as market_ema_30, 
	si2.ema_40 as market_ema_40, 
	si2.ema_50 as market_ema_50, 
	si2.ema_100 as market_ema_100, 
	si2.ema_200 as market_ema_200, 	
	si2.target_flag as market_target_flag, 
	si2.target_var as market_target_var, 
	si2.temp_flag as market_temp_flag, 
	si2.pct as market_pct, 
	si2.pct_mu_100 as market_pct_mu_100, 
	si2.pct_sd_100 as market_pct_sd_100, 
	si2.pct_percentile_100 as market_pct_percentile_100,
	si2.pct2 as market_pct2,
	si2.intraday_volatility as market_intraday_volatility,

	haf.open_ha, 
	haf.high_ha, 
	haf.low_ha, 
	haf.close_ha, 
	haf.intraday_volatility_ha, 
	haf.intraday_volatility_ha_mu_100, 
	haf.intraday_volatility_ha_sd_100, 
	haf.intraday_volatility_ha_percentile_100,

	haf2.open_ha as market_open_ha, 
	haf2.high_ha as market_high_ha, 
	haf2.low_ha as market_low_ha, 
	haf2.close_ha as market_close_ha, 
	haf2.intraday_volatility_ha as market_intraday_volatility_ha, 
	haf2.intraday_volatility_ha_mu_100 as market_intraday_volatility_ha_mu_100, 
	haf2.intraday_volatility_ha_sd_100 as market_intraday_volatility_ha_sd_100,
	haf2.intraday_volatility_ha_percentile_100 as market_intraday_volatility_ha_percentile_100

	from sf 
	join (select * from sf where symbol = 'SPY') s on sf.date = s.date
	join si on sf.symbol = si.symbol and sf.date = si.date
	join (select * from si where symbol = 'SPY') si2 on si.date = si2.date
	join haf on sf.symbol = haf.symbol and sf.date = haf.date
	join (select * from haf where symbol = 'SPY') haf2 on haf.date = haf2.date
	join sd on sf.symbol = sd.symbol
),

part2
as (
	select a.date, 
	a.symbol, 

	a.adjusted_lag_n30, 
	a.actual_return_n30, 
	a.market_adjusted_lag_n30, 
	a.market_return_n30, 
	a.sharpe_ratio_n30, 
	a.pct_sd_n30, 
	a.beta_n30, 
	a.nlcor_n30, 
	a.nlcor_pvalue_n30, 
	a.r_n30, 
	a.r_pvalue_n30, 
	a.r_squared_n30, 
	a.expected_return_n30, 
	a.alpha_n30,
	
	b.adjusted_lag_n100, 
	b.actual_return_n100, 
	b.market_adjusted_lag_n100, 
	b.market_return_n100, 
	b.sharpe_ratio_n100, 
	b.pct_sd_n100, 
	b.beta_n100, 
	b.nlcor_n100, 
	b.nlcor_pvalue_n100, 
	b.r_n100, 
	b.r_pvalue_n100, 
	b.r_squared_n100, 
	b.expected_return_n100, 
	b.alpha_n100	
	from ir30 a 
	join ir100 b on a.symbol = b.symbol and a.date = b.date
)

select p1.symbol, 
p1.company_name, 
case when p1.symbol = 'SPY' then 'SPDR S&P 500 ETF Trust' else p1.sector end as sector,
p1.date, 
p1.year, 
p1.quarter, 
p1.month, 
p1.weekday, 
p1.day, 

p1.open, 
p1.high, 
p1.low, 
p1.close, 
p1.volume, 
p1.adjusted, 

p1.market_open, 
p1.market_high, 
p1.market_low, 
p1.market_close, 
p1.market_volume, 
p1.market_adjusted,

p1.adj_lag_1, 
p1.adj_lag_2, 
p1.cci, 
p1.rsi, 
p1.macd, 
p1.macd_signal, 
p1.obv, 	 
p1.sma_5, 
p1.ema_10, 
p1.ema_20, 
p1.ema_30, 
p1.ema_40, 
p1.ema_50, 
p1.ema_100, 
p1.ema_200, 	
p1.target_flag, 
p1.target_var, 
p1.temp_flag, 
p1.pct, 
p1.pct_mu_100, 
p1.pct_sd_100, 
p1.pct_percentile_100,
p1.pct2,
p1.intraday_volatility,

p1.market_adj_lag_1, 
p1.market_adj_lag_2, 
p1.market_cci, 
p1.market_rsi, 
p1.market_macd, 
p1.market_macd_signal, 
p1.market_obv, 	 
p1.market_sma_5, 
p1.market_ema_10, 
p1.market_ema_20, 
p1.market_ema_30, 
p1.market_ema_40, 
p1.market_ema_50, 
p1.market_ema_100, 
p1.market_ema_200, 	
p1.market_target_flag, 
p1.market_target_var, 
p1.market_temp_flag, 
p1.market_pct, 
p1.market_pct_mu_100, 
p1.market_pct_sd_100, 
p1.market_pct_percentile_100,
p1.market_pct2,
p1.market_intraday_volatility,

p1.open_ha, 
p1.high_ha, 
p1.low_ha, 
p1.close_ha, 
p1.intraday_volatility_ha, 
p1.intraday_volatility_ha_mu_100, 
p1.intraday_volatility_ha_sd_100, 
p1.intraday_volatility_ha_percentile_100,

p1.market_open_ha, 
p1.market_high_ha, 
p1.market_low_ha, 
p1.market_close_ha, 
p1.market_intraday_volatility_ha, 
p1.market_intraday_volatility_ha_mu_100, 
p1.market_intraday_volatility_ha_sd_100,
p1.market_intraday_volatility_ha_percentile_100,

p2.adjusted_lag_n30, 
p2.actual_return_n30, 
p2.market_adjusted_lag_n30, 
p2.market_return_n30, 
p2.sharpe_ratio_n30, 
p2.pct_sd_n30, 
p2.beta_n30, 
p2.nlcor_n30, 
p2.nlcor_pvalue_n30, 
p2.r_n30, 
p2.r_pvalue_n30, 
p2.r_squared_n30, 
p2.expected_return_n30, 
p2.alpha_n30,

p2.adjusted_lag_n100, 
p2.actual_return_n100, 
p2.market_adjusted_lag_n100, 
p2.market_return_n100, 
p2.sharpe_ratio_n100, 
p2.pct_sd_n100, 
p2.beta_n100, 
p2.nlcor_n100, 
p2.nlcor_pvalue_n100, 
p2.r_n100, 
p2.r_pvalue_n100, 
p2.r_squared_n100, 
p2.expected_return_n100, 
p2.alpha_n100

from part1 p1
join part2 p2 on p1.symbol = p2.symbol and p1.date = p2.date
order by p1.symbol, p1.date
")

######################################################################
# extract data from various tables
tic()

df1 = dbGetQuery(con, query1)

toc()

# sanity check
dim(df1)  # [1] 917964    113

colSums(is.na(df1))

sqldf("select date, symbol, count(1)
      from df1
      group by 1, 2
      having count(1)>1
      limit 5")

##################################################################################################

# write table
tbl_name = "strategy_final_project_df1"
tbl = paste0(schema, ".", tbl_name)
tbl

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl), df1, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl}"))

##################################################################################################
tic()
df1 <- dbGetQuery(con, glue::glue("select * from {tbl} order by symbol, date"))
toc()

df1b <- df1 %>% 
    dplyr::mutate(intraday_volatility = dplyr::case_when(is.na(intraday_volatility) ~0,
                                                         TRUE ~ intraday_volatility)) %>%
    filter(date >= '2014-09-29' & 
               !is.na(target_flag))

#df1 %>% dplyr::filter(is.na(intraday_volatility))

colSums(is.na(df1b)) %>% as.data.frame()
dim(df1b)  # [1] 867474    113

# disconnect db
dbDisconnect(con) 















