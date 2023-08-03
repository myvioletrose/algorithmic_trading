output2_transformed <- output2 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%        
        dplyr::mutate(
                # message
                message = "hold",
                
                # candle stick pattern
                #candle_stick_pattern = case_when( csp_up_trend == 1 & (csp_dragonfly_doji == 1 | csp_hammer == 1 | csp_bullish_engulfing == 1 | csp_bullish_harami == 1 | csp_piercing_line == 1 | csp_kick_up == 1 | csp_three_white_soliders == 1 | csp_morning_star == 1 | csp_rising_three == 1) ~ 1,
                #                                  csp_down_trend == 1 & (csp_gravestone_doji == 1 | csp_inverted_harmer == 1 | csp_bearish_engulfing == 1 | csp_bearish_harami == 1 | csp_dark_cloud_cover == 1 | csp_kick_down == 1 | csp_three_black_crows == 1 | csp_evening_star == 1 | csp_failling_three == 1) ~ -1,
                #                                  TRUE ~ 0 ),
                candle_stick_pattern = case_when( csp_up_trend == 1 & csp_candle_stick_signal == 1 ~ 1,
                                                  csp_down_trend == 1 & csp_candle_stick_signal == -1 ~ -1,
                                                  TRUE ~ 0 ),
                candle_stick_pattern_lag1 = lag(candle_stick_pattern, 1),
                
                # signal buy if,
                message_b = case_when(macd_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - macd",
                                      macd_ha_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - macd_ha",
                                      #ha_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - ha",
                                      evwma_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - evwma",
                                      TRUE ~ message),
                
                # close lag, percent change, message lag
                close_lag1 = lag(close, 1),
                percent_change_lag1_day = (close - close_lag1)/ close_lag1,
                
                #close_lag2 = lag(close, 2),
                #percent_change_lag2_day = (close - close_lag2)/ close_lag2,
                
                close_lag3 = lag(close, 3),
                percent_change_lag3_day = (close - close_lag3)/ close_lag3,
                
                # close_lag4 = lag(close, 4),
                # percent_change_lag4_day = (close - close_lag4)/ close_lag4,
                
                close_lag5 = lag(close, 5),
                percent_change_lag5_day = (close - close_lag5)/ close_lag5,  
                
                #close_lag7 = lag(close, 7),
                #percent_change_lag7_day = (close - close_lag7)/ close_lag7, 
                
                percent_change_lag5_day_lag1 = lag(percent_change_lag5_day, 1),
                percent_change_lag5_day_lag2 = lag(percent_change_lag5_day, 2),
                percent_change_lag5_day_lag3 = lag(percent_change_lag5_day, 3),
                percent_change_lag5_day_lag4 = lag(percent_change_lag5_day, 4),
                
                # message_lag_3 = lag(message_b, 3),
                # message_lag_5 = lag(message_b, 5),
                # message_lag_7 = lag(message_b, 7),
                
                # sell if,
                message_s1 = case_when(macd_flag == -1 & close < zlema & candle_stick_pattern == -1 ~ "sell - macd",
                                       #ha_flag == -1 & close < zlema & candle_stick_pattern == -1 ~ "sell - ha",
                                       macd_ha_flag == -1 & close < zlema & candle_stick_pattern == -1 ~ "sell - macd_ha",
                                       evwma_flag == -1 & close < zlema & candle_stick_pattern == -1 ~ "sell - evwma",
                                       TRUE ~ message_b),                                                               
                
                message_s2 = case_when(candle_stick_pattern == -1 & close < zlema & (close < evwma | close < chanExit_long) ~ "sell - evwma/ce_long",                                       
                                       TRUE ~ message_b),
                
                # message_s3 = case_when((message_lag_7 %in% c("buy - macd", "buy - ha", "buy - evwma") & percent_change_lag7_day <0.01) ~ "sell - 7-day rule",
                #                        (message_lag_3 %in% c("buy - macd", "buy - ha", "buy - evwma") & percent_change_lag3_day >=0.01) ~ "sell - 3-day rule",
                #                        TRUE ~ message_b)          

                message_s3 = case_when(percent_change_lag5_day <0 &
                                               percent_change_lag5_day_lag1 <0 &
                                               percent_change_lag5_day_lag2 <0 &
                                               percent_change_lag5_day_lag3 <0 &
                                               percent_change_lag5_day_lag4 <0 &
                                               close < zlema ~ "sell - consecutive lost",
                                       TRUE ~ message_b)

        ) %>%
        ungroup() %>%
        dplyr::mutate(sma5_lower_yn = case_when(close < sma5 ~ 1, TRUE ~ 0),                      
                      zlema_lower_yn = case_when(close < zlema ~ 1, TRUE ~ 0),
                      evwma_lower_yn = case_when(close < evwma ~ 1, TRUE ~ 0),
                      ce_long_break_yn = case_when(close < chanExit_long ~ 1, TRUE ~ 0),
                      ce_short_break_yn = case_when(close > chanExit_short ~ 1, TRUE ~ 0)) %>%
        select(symbol, 
               date,
               year,
               volume,
               open,
               high,
               low,
               close,               
               trailing_stop_loss_yesterday,
               atr,
               
               percent_change_lag1_day,
               percent_change_lag3_day,
               percent_change_lag5_day,               
               
               sma5,               
               zlema,
               evwma,
               
               chanExit_short,
               chanExit_long,
               
               sma5_lower_yn,
               
               zlema_lower_yn,
               evwma_lower_yn,
               
               ce_short_break_yn,
               ce_long_break_yn,
               
               cci,
               rsi,
               macd_trend_dir,
               is_intraday_green_yn_ha,
               
               situation,
               
               contains("csp_"),
               matches("message_[bs]")
        ) %>%
        arrange(symbol, date)

##################################################################
################## <<<< s4 >>>> ########################################
tic()

s4 = output2_transformed %>% 
        select(symbol, date, close, message_b) %>%
        dplyr::mutate(message_s4 = "hold")

hard_stop_down_by = 0.1
hard_stop_up_by = 0.2
down_by = 0.02
up_by = 0.04

s4_df = sqldf(glue::glue("
with sub 
as (
        select symbol,
        date,
        close,
        message_b,
        message_s4
        from s4
        where message_b like '%buy%'
),

sub2
as (
        select x.symbol,
        x.date,
        max(sub.date) as last_buy_date
        from s4 x
        left join sub on x.symbol = sub.symbol and x.date > sub.date
        group by 1, 2
),

sub3
as (
        select s2.symbol,
        s2.date,
        s2.last_buy_date,
        sub.close as close_flagged_by_last_buy
        from sub2 s2
        left join sub on s2.symbol = sub.symbol and s2.last_buy_date = sub.date
),

fillna
as (
        select s3.symbol,
        s3.date,
        s3.last_buy_date,
        s4.message_b,
        s4.close,
        case when s3.close_flagged_by_last_buy is null then s4.close else s3.close_flagged_by_last_buy end as close_flagged_by_last_buy
        from sub3 s3
        join s4 on s3.symbol = s4.symbol and s3.date = s4.date
),

up_and_down
as (
        select symbol,
        date,
        last_buy_date,
        message_b,
        close,
        close_flagged_by_last_buy,        
        close_flagged_by_last_buy * (1 - {down_by}) as down_limit,
        close_flagged_by_last_buy * (1 + {up_by}) as up_limit,
        close_flagged_by_last_buy * (1 - {hard_stop_down_by}) as hard_stop_down_limit,
        close_flagged_by_last_buy * (1 + {hard_stop_up_by}) as hard_stop_up_limit
        from fillna
),

msg_s4
as (
        select symbol,
        date,
        last_buy_date,
        message_b,
        close,
        close_flagged_by_last_buy,        
        down_limit,
        up_limit,
        hard_stop_down_limit,
        hard_stop_up_limit,
        case when close < down_limit then 'sell - s4 (down)'
                when close > up_limit then 'sell - s4 (up)'
                else message_b
                end as message_s4
        from up_and_down
),

first_sell
as (
        select symbol,
        last_buy_date,
        min(date) as first_sell_date
        from msg_s4
        where message_s4 like '%sell%'
        group by 1, 2
),

interim
as (
        select x.symbol,
        x.date,
        x.last_buy_date,
        x.message_b,
        x.close,
        x.close_flagged_by_last_buy,
        x.down_limit,
        x.up_limit,
        x.hard_stop_down_limit,
        x.hard_stop_up_limit,
        case when y.first_sell_date = x.date then x.message_s4 else x.message_b end as message_s4
        from msg_s4 x
        left join first_sell y on x.symbol = y.symbol and x.last_buy_date = y.last_buy_date
)

select *
from interim
order by symbol, date
"))

toc()

#####################################
hard_stop_limit_df <- s4_df %>%
        dplyr::select(symbol, date, close, close_flagged_by_last_buy, message_b, hard_stop_down_limit, hard_stop_up_limit) %>%
        arrange(symbol, date)

poc <- output2_transformed %>%
        dplyr::inner_join(s4_df %>% select(symbol, date, message_s4), by = c("symbol", "date")) %>%
        dplyr::inner_join(hard_stop_limit_df %>% select(symbol, date, hard_stop_down_limit, hard_stop_up_limit), by = c("symbol", "date")) %>%
        dplyr::mutate(
                # profit_protection - sell immediately if,
                message_s1 = case_when(close < trailing_stop_loss_yesterday ~ "sell - profit protect", 
                                       TRUE ~ message_s1),
                message_s2 = case_when(close < trailing_stop_loss_yesterday ~ "sell - profit protect", 
                                       TRUE ~ message_s2),
                message_s3 = case_when(close < trailing_stop_loss_yesterday ~ "sell - profit protect", 
                                       TRUE ~ message_s3), 
                message_s4 = case_when(close < trailing_stop_loss_yesterday ~ "sell - profit protect", 
                                       TRUE ~ message_s4),
                # stop-loss
                message_s1 = case_when(close > hard_stop_up_limit ~ "sell - stop loss (hard)", TRUE ~ message_s1),
                message_s2 = case_when(close > hard_stop_up_limit ~ "sell - stop loss (hard)", TRUE ~ message_s2),
                message_s3 = case_when(close > hard_stop_up_limit ~ "sell - stop loss (hard)", TRUE ~ message_s3)
                #message_s4 = case_when(close > hard_stop_up_limit ~ "sell - stop loss (hard)", TRUE ~ message_s4)                
        ) %>%
        arrange(symbol, date)

############################ <<< save poc >>> #############################
# # save tbl
# schema = "adhoc"
# tbl_name = "poc"
# tbl = paste0(schema, ".", tbl_name)
# DB = "stg"
# 
# # create connection
# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)
# 
# # save_tbl_action
# dim(poc)
# tic()
# save_tbl_action(tbl, poc, overwrite = TRUE)
# toc()
# 
# # check table
# d(glue::glue("select count(1) from {tbl}"))
# #poc <- d(glue::glue("select * from {tbl} order by symbol, date"))
# 
# # disconnect db
# dbDisconnect(con)

################################################################################
###################### <<<<<<<<<<<<<<<<< begin eval >>>>>>>>>>>> #########################
msg_string_update <- function(x) {
        ifelse(grepl("buy", x, ignore.case = TRUE), 
               "buy",
               ifelse(grepl("sell", x, ignore.case = TRUE), 
                      "sell",
                      x))
}

poc.eval <- poc %>%
        mutate(across(contains("message_s"), msg_string_update)) %>%
        select(symbol, year, date, open, high, low, close, contains("message_s")) %>%
        arrange(symbol, date)

############# >>>>>>>>>>>>>>>>>>>>>>>>> #################################################
#symbols = c('WTW', 'SPY', 'GOOGL')
symbols = poc$symbol %>% unique()
temp_symbol_list = vector(mode = "list", length = length(symbols))
message_list <- names(poc) %>% grep(pattern = "message_s", ignore.case = TRUE, value = TRUE)
fund_begin = 1000

start_date = "2022-01-01"
#end_date = Sys.Date()
end_date = "2022-12-31"

temp_msg_list = vector(mode = "list", length = length(message_list))

tic()
for(i in 1:length(symbols)){
        
        s = symbols[i]
        
        for(j in 1:length(message_list)){
                
                col = message_list[j]
                x <- let(c(COL = col),
                         poc.eval %>%
                                 dplyr::mutate(message = COL) %>%
                                 dplyr::filter(date >= start_date & 
                                                       date <= end_date &
                                                       symbol == s))
                
                y1 <- strategyEval(fund_begin = fund_begin, x) %>% .$net_value
                
                z <- data.frame(start_date = start_date,
                                end_date = end_date,
                                symbol = s,
                                net_value = y1,
                                message_type = message_list[j])
                
                temp_msg_list[[j]] <- z
                
        }
        
        temp_msg_df = temp_msg_list %>% plyr::ldply()         
        temp_symbol_list[[i]] = temp_msg_df
        
}
toc()

# result of the "simulation poc"
simpoc = temp_symbol_list %>% plyr::ldply()
simpoc %>% arrange(desc(net_value)) %>% head()

# win
win = sum(simpoc$net_value[simpoc$net_value >0]); win

# loss
loss = sum(simpoc$net_value[simpoc$net_value <0]); loss

# ratio
rat = abs(win / loss); rat

# spread messages
simpoc2 <- simpoc %>%
        tidyr::spread(message_type, net_value) %>%
        rowwise() %>%
        dplyr::mutate(net_value = sum(c_across(starts_with("message_s"))),
                      net_value_chg = net_value / (length(message_list) * fund_begin)) %>%
        arrange(desc(net_value_chg))

simpoc2 %>% head()

##########################################################################################
####################################### >>>>>>>>>>>>>>>>>>>>>>>>>> ###################
####################### <<< summary stat >>> #####################################
simpoc3 <- simpoc %>%
        dplyr::mutate(is_win_yn = case_when(net_value >0 ~ 1, TRUE ~ 0),
                      percent_chg = net_value / fund_begin) 

simpoc3 %>% head()

# net value by message type, overall 
print(paste0("total net value is $", simpoc3 %>% select(net_value) %>% sum() %>% round(1)))
simpoc3 %>% 
        group_by(message_type) %>%
        summarise(net_value = sum(net_value)) %>%
        arrange(desc(net_value))

# win/loss rate by message type, overall 
print(paste0("overall win/loss rate is ", simpoc3 %>% select(is_win_yn) %>% sum() %>% round() / nrow(simpoc3)))

simpoc3 %>% 
        group_by(message_type) %>%
        summarise(n = n(),
                  is_win_yn = sum(is_win_yn)) %>%
        dplyr::mutate(win_loss_rate = is_win_yn / n) %>%
        select(message_type, win_loss_rate) %>%
        arrange(desc(win_loss_rate))

# percent change by message type, overall
print(paste0("overall percent change is ", simpoc3 %>% select(percent_chg) %>% . $percent_chg %>% mean() %>% round(2)))

simpoc3 %>% 
        group_by(message_type) %>%
        summarise(avg_percent_chg = mean(percent_chg)) %>%
        arrange(desc(avg_percent_chg)) 










