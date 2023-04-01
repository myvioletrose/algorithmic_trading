# creation of df2, df3

columns <- wrapr::qc(symbol, date,     
                     target_var, target_flag,
                     open, close, adjusted, 
                     cci, rsi, macd, macd_signal,
                     open_ha, high_ha, low_ha, close_ha, intraday_volatility_ha,
                     obv, sma_5, ema_10, ema_30,
                     sharpe_ratio_n30, nlcor_n30
)

df2_temp <- df1b %>%
    dplyr::select(all_of(columns)) %>%
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    # dplyr::mutate(ema_10_ha = pracma::movavg(close_ha, n = 10, type = "e"),
    #               ema_30_ha = pracma::movavg(close_ha, n = 30, type = "e")) %>%
    dplyr::mutate(obv_lag_1 = lag(obv),
                  close_lag_1 = lag(close),
                  obv_flag = dplyr::case_when(obv > obv_lag_1 ~1, TRUE ~-1),
                  overnight_flag = dplyr::case_when((open > close_lag_1) & (close > open) ~1,
                                                    (open < close_lag_1) & (close < open) ~-1,
                                                    TRUE ~0),
                  ema_flag = dplyr::case_when(ema_10 > ema_30 ~1, TRUE ~-1),
                  sma_flag = dplyr::case_when(adjusted > sma_5 ~1, TRUE ~-1)) %>%
    ungroup()

##################################################################
# reminder: %>% filter(date >= '2014-10-01')
# ebz: enter buy zone
# esz: enter sell zone
# cz: comfort zone
# bz: buy zone
# sz: sell zone

tic()

df2 <- sqldf("
with sub
as (
    select symbol, 
    date, 
    target_var, 
    target_flag, 
    adjusted,     
    cci, 
    rsi, 
    macd, 
    macd_signal, 
    case when macd > macd_signal then abs(macd - macd_signal) else abs(macd - macd_signal) * -1 end as macd_signal_diff,
    intraday_volatility_ha, 
    nlcor_n30, 
    obv_flag, 
    overnight_flag, 
    ema_flag, 
    sma_flag
    from df2_temp
), 

lag 
as (
    select symbol, 
    date, 
    lag(adjusted, 1) over(partition by symbol order by date) as adjusted_lag_1,    
    lag(cci, 1) over(partition by symbol order by date) as cci_lag_1,    
    lag(rsi, 1) over(partition by symbol order by date) as rsi_lag_1,
    lag(macd_signal_diff, 1) over(partition by symbol order by date) as macd_signal_diff_lag_1,
    lag(intraday_volatility_ha, 1) over(partition by symbol order by date) as intraday_volatility_ha_lag_1,    

    lag(adjusted, 2) over(partition by symbol order by date) as adjusted_lag_2,    
    lag(cci, 2) over(partition by symbol order by date) as cci_lag_2,    
    lag(rsi, 2) over(partition by symbol order by date) as rsi_lag_2,
    lag(macd, 2) over(partition by symbol order by date) as macd_lag_2,
    lag(macd_signal_diff, 2) over(partition by symbol order by date) as macd_signal_diff_lag_2,
    lag(intraday_volatility_ha, 2) over(partition by symbol order by date) as intraday_volatility_ha_lag_2   
    from sub
),

macd
as (
    select symbol, 
    date, 
    zone_in_out,
    zone,
    case when (macd_signal_diff > macd_signal_diff_lag_1) and (macd_signal_diff_lag_1 > macd_signal_diff_lag_2) and zone = 'bz' and zone_in_out = 'cz' then 'up - bz'
        when (macd_signal_diff > macd_signal_diff_lag_1) and (macd_signal_diff_lag_1 > macd_signal_diff_lag_2) and zone = 'sz' and zone_in_out = 'cz' then 'up - sz'
        when (macd_signal_diff < macd_signal_diff_lag_1) and (macd_signal_diff_lag_1 < macd_signal_diff_lag_2) and zone = 'bz' and zone_in_out = 'cz' then 'down - bz'
        when (macd_signal_diff < macd_signal_diff_lag_1) and (macd_signal_diff_lag_1 < macd_signal_diff_lag_2) and zone = 'sz' and zone_in_out = 'cz' then 'down - sz'
        else 'stay'
        end as zone_direction
    from (
        select l.symbol,
        l.date,
        s.macd_signal_diff,
        l.macd_signal_diff_lag_1,
        l.macd_signal_diff_lag_2,
        case when (l.macd_signal_diff_lag_1 > l.macd_signal_diff_lag_2) and (s.macd_signal_diff > l.macd_signal_diff_lag_1) and (l.macd_signal_diff_lag_2 <0) and (s.macd_signal_diff >0) then 'ebz'        
            when (l.macd_signal_diff_lag_1 < l.macd_signal_diff_lag_2) and (s.macd_signal_diff < l.macd_signal_diff_lag_1) and (l.macd_signal_diff_lag_2 >0) and (s.macd_signal_diff <0) then 'esz'        
            else 'cz' end as zone_in_out,
        case when s.macd_signal_diff >0 then 'bz' else 'sz' end as zone
        from sub s
        join lag l on s.symbol = l.symbol and s.date = l.date
    ) m
), 

rsi 
as (
    select symbol, 
    date, 
    zone_in_out,
    zone,
    case when (rsi > rsi_lag_1) and zone = 'bz' and zone_in_out = 'cz' then 'up - bz' 
        when (rsi > rsi_lag_1) and zone = 'sz' and zone_in_out = 'cz' then 'up - sz'    
        when (rsi < rsi_lag_1) and zone = 'bz' and zone_in_out = 'cz' then 'down - bz'    
        when (rsi < rsi_lag_1) and zone = 'sz' and zone_in_out = 'cz' then 'down - sz'
        else 'stay'
        end as zone_direction
    from (
        select l.symbol,
        l.date,
        s.rsi,
        l.rsi_lag_1,
        l.rsi_lag_2,
        case when (l.rsi_lag_1 < l.rsi_lag_2) and (s.rsi < l.rsi_lag_1) and (l.rsi_lag_2 >30) and (s.rsi <30) then 'ebz'        
            when (l.rsi_lag_1 > l.rsi_lag_2) and (s.rsi > l.rsi_lag_1) and (l.rsi_lag_2 <70) and (s.rsi >70) then 'esz'        
            else 'cz' end as zone_in_out,
        case when s.rsi <30 then 'bz' 
            when s.rsi >70 then 'sz'
            else 'cz' end as zone
        from sub s
        join lag l on s.symbol = l.symbol and s.date = l.date
    ) r
),

cci 
as (
    select symbol, 
    date, 
    zone_in_out,
    zone,
    case when (cci > cci_lag_1) and (cci_lag_1 > cci_lag_2) and zone = 'bz' and zone_in_out = 'cz' then 'up - bz' 
        when (cci > cci_lag_1) and (cci_lag_1 > cci_lag_2) and zone = 'sz' and zone_in_out = 'cz' then 'up - sz'    
        when (cci < cci_lag_1) and (cci_lag_1 < cci_lag_2) and zone = 'bz' and zone_in_out = 'cz' then 'down - bz'    
        when (cci < cci_lag_1) and (cci_lag_1 < cci_lag_2) and zone = 'sz' and zone_in_out = 'cz' then 'down - sz'
        else 'stay'
        end as zone_direction
    from (
        select l.symbol,
        l.date,
        s.cci,
        l.cci_lag_1,
        l.cci_lag_2,
        case when (l.cci_lag_1 < l.cci_lag_2) and (s.cci < l.cci_lag_1) and (l.cci_lag_2 >-100) and (s.cci <-100) then 'ebz'        
            when (l.cci_lag_1 > l.cci_lag_2) and (s.cci > l.cci_lag_1) and (l.cci_lag_2 <100) and (s.cci >100) then 'esz'        
            else 'cz' end as zone_in_out,
        case when s.cci <-100 then 'bz' 
            when s.cci >100 then 'sz'
            else 'cz' end as zone
        from sub s
        join lag l on s.symbol = l.symbol and s.date = l.date
    ) c    
), 

ha 
as (
    select symbol, 
    date, 
    zone_in_out,
    zone,
    case when (intraday_volatility_ha >0) and (intraday_volatility_ha_lag_1 >0) and zone_in_out = 'cz' then 'up - bz' 
        when (intraday_volatility_ha <0) and (intraday_volatility_ha_lag_1 <0) and zone_in_out = 'cz' then 'down - sz'
        else 'stay'
        end as zone_direction
    from (
        select l.symbol,
        l.date,
        s.intraday_volatility_ha,
        l.intraday_volatility_ha_lag_1,
        l.intraday_volatility_ha_lag_2,
        case when l.intraday_volatility_ha_lag_2 <0 and 
            l.intraday_volatility_ha_lag_1 <0 and
            s.intraday_volatility_ha >0 then 'ebz'        
            when l.intraday_volatility_ha_lag_2 >0 and 
            l.intraday_volatility_ha_lag_1 >0 and
            s.intraday_volatility_ha <0 then 'esz'        
            else 'cz' end as zone_in_out,
        case when s.intraday_volatility_ha >0 then 'bz' 
            else 'sz'
            end as zone
        from sub s
        join lag l on s.symbol = l.symbol and s.date = l.date
    ) i
), 

interim
as (
    select s.symbol, 
    s.date, 

    s.target_var, 
    s.target_flag, 

    s.adjusted,     

    s.cci, 
    s.rsi, 
    s.macd, 
    s.macd_signal, 
    s.macd_signal_diff,
    s.intraday_volatility_ha, 

    s.nlcor_n30, 

    s.obv_flag, 
    s.overnight_flag, 
    s.ema_flag, 
    s.sma_flag,

    l.adjusted_lag_1,    
    l.cci_lag_1,    
    l.rsi_lag_1,
    l.macd_signal_diff_lag_1,
    l.intraday_volatility_ha_lag_1,    
    l.adjusted_lag_2,    
    l.cci_lag_2,    
    l.rsi_lag_2,
    l.macd_lag_2,
    l.macd_signal_diff_lag_2,
    l.intraday_volatility_ha_lag_2,

    m.zone_in_out as macd_zone_in_out,
    m.zone as macd_zone,
    m.zone_direction as macd_zone_direction,

    r.zone_in_out as rsi_zone_in_out,
    r.zone as rsi_zone,
    r.zone_direction as rsi_zone_direction,

    c.zone_in_out as cci_zone_in_out,
    c.zone as cci_zone,
    c.zone_direction as cci_zone_direction,

    h.zone_in_out as ha_zone_in_out,
    h.zone as ha_zone,
    h.zone_direction as ha_zone_direction

    from sub s 
    join lag l on s.symbol = l.symbol and s.date = l.date
    join macd m on s.symbol = m.symbol and s.date = m.date 
    join rsi r on s.symbol = r.symbol and s.date = r.date 
    join cci c on s.symbol = c.symbol and s.date = c.date 
    join ha h on s.symbol = h.symbol and s.date = h.date
),

strategy
as (
    select symbol, 
    date,
    case when macd_zone_in_out = 'ebz' then 1 
        when macd_zone_in_out = 'esz' then -1
        else 0 
        end as macd_zone_in_out_flag,

    case when rsi_zone_in_out = 'ebz' then 1 
        when rsi_zone_in_out = 'esz' then -1
        else 0 
        end as rsi_zone_in_out_flag,

    case when cci_zone_in_out = 'ebz' then 1 
        when cci_zone_in_out = 'esz' then -1
        else 0 
        end as cci_zone_in_out_flag,

    case when ha_zone_in_out = 'ebz' then 1 
        when ha_zone_in_out = 'esz' then -1
        else 0 
        end as ha_zone_in_out_flag,

    case when macd_zone_direction in ('up - bz', 'up - sz') then 1 
        when macd_zone_direction in ('down - bz', 'down - sz') then -1
        else 0 
        end as macd_zone_direction_flag,

    case when cci_zone_direction in ('up - bz', 'up - sz') then 1 
        when cci_zone_direction in ('down - bz', 'down - sz') then -1
        else 0 
        end as cci_zone_direction_flag,

    case when rsi_zone_direction in ('up - bz') then 1 
        when rsi_zone_direction in ('down - sz') then -1
        else 0 
        end as rsi_zone_direction_flag,

    case when ha_zone_direction in ('up - bz') then 1 
        when ha_zone_direction in ('down - sz') then -1
        else 0 
        end as ha_zone_direction_flag

    from interim
)

select i.*, 
-- A columns: entry/exit signal
s.macd_zone_in_out_flag,
s.rsi_zone_in_out_flag,
s.cci_zone_in_out_flag,
s.ha_zone_in_out_flag,

-- B columns: indicate buy/sell zone, e.g., macd_zone, rsi_zone

-- C columns: direction inside the buy/sell zone
s.macd_zone_direction_flag,
s.cci_zone_direction_flag,
s.rsi_zone_direction_flag,
s.ha_zone_direction_flag

from interim i 
join strategy s on i.symbol = s.symbol and i.date = s.date
order by symbol, date
") %>%
    dplyr::filter(date >= "2014-10-01")  # 'date' not recognized as date type inside the query, cast(date as date) not working either

toc()

dim(df2)
colSums(is.na(df2)) %>% as.data.frame()

#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

###### write table
tbl_name = "strategy_final_project_df2"
tbl = paste0(schema, ".", tbl_name)
tbl

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl), df2, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl}"))

##################################################################################################################
##################################################################################################################
# "adjusted"
# "target_var"
# "nlcor_n30"

rsi_threshold = 2
cci_threshold = 2
macd_threshold = 4
ha_threshold = 4

df3_temp <- df2 %>%
    dplyr::select(c("symbol", "date"), contains("_flag")) %>%
    arrange(symbol, date)
dim(df3_temp)  # [1] 866556     15

list_of_symbols = unique(df3_temp$symbol)
temp_list <- vector(mode = "list", length = length(list_of_symbols))

###################################
tic()

for(i in 1:length(list_of_symbols)){

    temp <- df3_temp %>%
        dplyr::filter(symbol == list_of_symbols[i])
    
    part1 <- temp %>%
        tidyr::gather("flag", "value",
                      wrapr::qc(macd_zone_in_out_flag, 
                                rsi_zone_in_out_flag, 
                                cci_zone_in_out_flag, 
                                ha_zone_in_out_flag, 
                                
                                macd_zone_direction_flag, 
                                cci_zone_direction_flag, 
                                rsi_zone_direction_flag, 
                                ha_zone_direction_flag, 
                                
                                obv_flag, 
                                overnight_flag, 
                                ema_flag, 
                                sma_flag))
    
    part2 <- temp %>%
        dplyr::select(symbol, date, 
                      macd_zone_in_out_flag, 
                      rsi_zone_in_out_flag, 
                      cci_zone_in_out_flag, 
                      ha_zone_in_out_flag) %>%
        dplyr::inner_join(., part1, by = c("symbol", "date"))
    
    part3 = sqldf(glue::glue("
    with sub
    as (
        select symbol,
        date,
        macd_zone_in_out_flag,
        rsi_zone_in_out_flag,
        cci_zone_in_out_flag,
        ha_zone_in_out_flag,
        sum(case when value = 1 then 1 else 0 end) as up_count,
        sum(case when value = -1 then 1 else 0 end) as down_count
        from part2
        group by 1, 2, 3, 4, 5, 6
    )
    
    select x.symbol,
    x.date,
    
    -- A columns
    x.macd_zone_in_out_flag,
    x.rsi_zone_in_out_flag,
    x.cci_zone_in_out_flag,
    x.ha_zone_in_out_flag,
    
    -- C columns
    x.macd_zone_direction_flag,
    x.cci_zone_direction_flag,
    x.rsi_zone_direction_flag,
    x.ha_zone_direction_flag,
    
    -- D columns
    x.obv_flag,
    x.overnight_flag,
    x.ema_flag,
    x.sma_flag,
    
    -- count flags
    y.up_count,
    y.down_count,
    
    -- E columns
    case when x.macd_zone_in_out_flag = 1 and y.up_count >= {macd_threshold} then 1
        when x.macd_zone_in_out_flag = -1 and y.down_count >= {macd_threshold} then -1
        else 0
        end as macd_sig_flag,
        
    case when x.rsi_zone_in_out_flag = 1 and y.up_count >= {rsi_threshold} then 1
        when x.rsi_zone_in_out_flag = -1 and y.down_count >= {rsi_threshold} then -1
        else 0
        end as rsi_sig_flag,
    
    case when x.cci_zone_in_out_flag = 1 and y.up_count >= {cci_threshold} then 1
        when x.cci_zone_in_out_flag = -1 and y.down_count >= {cci_threshold} then -1
        else 0
        end as cci_sig_flag,
    
    case when x.ha_zone_in_out_flag = 1 and y.up_count >= {ha_threshold} then 1
        when x.ha_zone_in_out_flag = -1 and y.down_count >= {ha_threshold} then -1
        else 0
        end as ha_sig_flag
        
    from temp x
    join sub y on x.symbol = y.symbol and x.date = y.date
    order by x.symbol, x.date
    "))

temp_list[[i]] = part3

print(paste0(i, ". ", list_of_symbols[i], " fimished at ", Sys.time()))    
    
}

toc()

tempDf <- temp_list %>% 
    dplyr::bind_rows() %>% 
    dplyr::inner_join(., df1b %>%
                          dplyr::select(symbol, date,
                                        open, high, low, close, adjusted, volume,
                                        target_var, target_flag, 
                                        nlcor_n30),
                      by = c("symbol", "date")) %>%
    dplyr::select(symbol, date,
                  open, high, low, close, adjusted, volume,
                  target_var, target_flag, 
                  nlcor_n30, everything()) %>%
    arrange(symbol, date)

dim(tempDf)
colSums(is.na(tempDf)) %>% as.data.frame()

tempDf_market <- tempDf %>%
    dplyr::filter(symbol == 'SPY') %>%
    dplyr::select(c("symbol", "date"), contains("_sig_flag")) %>%
    tidyr::gather("flag", "value", -c(symbol:date))

tempDf_market2 <- sqldf("
select symbol,
date,
case when neg_count >0 then -1
    when pos_count >0 and neg_count = 0 then 1
    else 0
    end as market_sig_flag
from (
    select symbol,
    date,
    sum(case when value = -1 then 1 else 0 end) as neg_count,
    sum(case when value = 1 then 1 else 0 end) as pos_count 
    from tempDf_market
    group by 1, 2
) m          
")

#################################################################################
df3 <- sqldf("
with sub 
as (
    select t.*
    , tm.market_sig_flag
    from tempDf t
    left join tempDf_market2 tm on t.date = tm.date 
)

select symbol, 
date, 
open, 
high, 
low, 
close, 
adjusted, 
volume, 
target_var, 
target_flag, 
nlcor_n30, 
macd_zone_in_out_flag, 
rsi_zone_in_out_flag, 
cci_zone_in_out_flag, 
ha_zone_in_out_flag, 
macd_zone_direction_flag, 
cci_zone_direction_flag, 
rsi_zone_direction_flag, 
ha_zone_direction_flag, 
obv_flag, 
overnight_flag, 
ema_flag, 
sma_flag, 
up_count, 
down_count, 
market_sig_flag,
case when macd_sig_flag = 1 and
    nlcor_n30 > .5 and 
    market_sig_flag != 1
    then 0
    else macd_sig_flag
    end as macd_sig_flag, 

case when rsi_sig_flag = 1 and
    nlcor_n30 > .5 and 
    market_sig_flag != 1
    then 0
    else rsi_sig_flag
    end as rsi_sig_flag,

case when cci_sig_flag = 1 and
    nlcor_n30 > .5 and 
    market_sig_flag != 1
    then 0
    else cci_sig_flag
    end as cci_sig_flag,

case when ha_sig_flag = 1 and
    nlcor_n30 > .5 and 
    market_sig_flag != 1
    then 0
    else ha_sig_flag
    end as ha_sig_flag
from sub
order by symbol, date
")

str(df3)
dim(df3)
colSums(is.na(df3)) %>% as.data.frame()

##################################################
###### write table
tbl_name = "strategy_final_project_df3"
tbl = paste0(schema, ".", tbl_name)
tbl

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl), df3, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl}"))

# disconnect db
dbDisconnect(con) 









