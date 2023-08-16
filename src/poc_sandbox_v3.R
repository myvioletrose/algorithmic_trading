output2_transformed <- output2 %>%
        #filter(date >= '2015-01-01') %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%        
        dplyr::mutate(
                # trailing_stop_loss
                trailing_stop_loss = close - 2*atr,
                trailing_stop_loss_yesterday = dplyr::lag(trailing_stop_loss, 1),
                
                # message
                message = "hold",
                
                # candle stick pattern
                candle_stick_pattern = case_when( csp_up_trend == 1 & csp_candle_stick_signal == 1 ~ 1,
                                                  csp_down_trend == 1 & csp_candle_stick_signal == -1 ~ -1,
                                                  TRUE ~ 0 ),
                candle_stick_pattern_lag1 = lag(candle_stick_pattern, 1),
                
                # buy if,
                message_b = case_when(macd_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - macd",
                                      macd_ha_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - macd_ha",
                                      evwma_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - evwma",
                                      TRUE ~ message),
                
                # close lag, percent change
                close_lag1 = lag(close, 1),
                percent_change_lag1_day = (close - close_lag1)/ close_lag1,
                
                close_lag5 = lag(close, 5),
                percent_change_lag5_day = (close - close_lag5)/ close_lag5,  
                
                percent_change_lag5_day_lag1 = lag(percent_change_lag5_day, 1),
                percent_change_lag5_day_lag2 = lag(percent_change_lag5_day, 2),
                percent_change_lag5_day_lag3 = lag(percent_change_lag5_day, 3),
                percent_change_lag5_day_lag4 = lag(percent_change_lag5_day, 4),
                
                # sell if,
                message_s = case_when(macd_flag == -1 & close < zlema & (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) ~ "sell - macd",
                                      macd_ha_flag == -1 & close < zlema & (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) ~ "sell - macd_ha",
                                      evwma_flag == -1 & close < zlema & (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) ~ "sell - evwma",
                                      candle_stick_pattern == -1 & close < zlema & (close < evwma | close < chanExit_long) ~ "sell - evwma/ce_long",
                                      (percent_change_lag5_day <0 & 
                                               percent_change_lag5_day_lag1 <0 &
                                               percent_change_lag5_day_lag2 <0 &
                                               percent_change_lag5_day_lag3 <0 &
                                               percent_change_lag5_day_lag4 <0 &
                                               close < zlema) ~ "sell - consecutive lost",
                                      # profit protection
                                      close < trailing_stop_loss_yesterday ~ "sell - profit protect",
                                      TRUE ~ message_b)
        ) %>%
        ungroup() %>%
        select(symbol, 
               date,
               is_today,
               year,
               volume,
               open,
               high,
               low,
               close,
               close_lag1,
               trailing_stop_loss_yesterday,
               atr,
               
               percent_change_lag1_day,
               percent_change_lag5_day,               
               
               sma5,               
               zlema,
               evwma,
               
               chanExit_short,
               chanExit_long,
               
               cci,
               rsi,
               macd_trend_dir,
               is_intraday_green_yn_ha,
               
               situation,
               
               contains("csp_"),
               matches("message_[bs]")
        ) %>%
        arrange(symbol, date)

#################################################################################################################
################## <<< stop-loss, support, target calculation >>> ########################################
# stop-loss - set at ATR * 'x' below flagged close
stop_loss_base = 2
# first support - set at ATR * 'x' above flagged close
support1 = 2
# second support - set at 'x' percent above flagged close
support2 = 0.1
# third support - set at 'x' percent above flagged close
support3 = 0.2

# begin series of transformation
pricing = output2_transformed %>%
        select(symbol, date, close, atr, message_b, message_s) %>%
        arrange(symbol, date)

target1 = pricing %>%
        filter(grepl("sell", message_s, ignore.case = TRUE)) %>%
        select(symbol, sell_date = date, close, atr, message_b, message_s)

target2 = pricing %>%
        filter(grepl("buy", message_b, ignore.case = TRUE)) %>%
        select(symbol, buy_date = date, close, atr, message_b, message_s)

target3 = pricing %>%
        select(symbol, date) %>%
        left_join(target1, join_by(symbol, date > sell_date)) %>%
        group_by(symbol, date) %>%
        summarise(last_sell_date = max(sell_date)) %>%
        ungroup()

target4 = target3 %>%
        left_join(target2, join_by(symbol, date >= buy_date, last_sell_date < buy_date)) %>%
        group_by(symbol, date, last_sell_date) %>%
        summarise(last_buy_date = min(buy_date),
                  last_buy_date2 = max(buy_date)) %>%
        ungroup()

target5.1 = target4 %>%
        left_join(pricing, join_by(symbol, last_buy_date == date)) %>%
        select(symbol, date, last_buy_date, 
               close_flagged_by_buy = close,
               atr_flagged_by_buy = atr) %>%
        inner_join(pricing %>% select(symbol, date, close, atr), by = c("symbol", "date")) %>%
        mutate(close_flagged_by_buy = case_when(is.na(close_flagged_by_buy) ~ close, TRUE ~ close_flagged_by_buy),
               atr_flagged_by_buy = case_when(is.na(atr_flagged_by_buy) ~ atr, TRUE ~ atr_flagged_by_buy),
               in_the_buy_yn = case_when(is.na(last_buy_date) ~ 0, TRUE ~ 1))

target5.2 = target4 %>%
        left_join(pricing, join_by(symbol, last_buy_date2 == date)) %>%
        select(symbol, date, 
               last_buy_date2, 
               close_flagged_by_buy2 = close,
               atr_flagged_by_buy2 = atr) %>%
        inner_join(pricing %>% select(symbol, date, close, atr), by = c("symbol", "date")) %>%
        mutate(close_flagged_by_buy2 = case_when(is.na(close_flagged_by_buy2) ~ close, TRUE ~ close_flagged_by_buy2),
               atr_flagged_by_buy2 = case_when(is.na(atr_flagged_by_buy2) ~ atr, TRUE ~ atr_flagged_by_buy2),
               in_the_buy_yn = case_when(is.na(last_buy_date2) ~ 0, TRUE ~ 1))

target5 = target5.1 %>%
        inner_join(target5.2 %>%
                           select(symbol, date,
                                  last_buy_date2,
                                  close_flagged_by_buy2,
                                  atr_flagged_by_buy2),
                   by = c("symbol", "date"))

target = target5 %>%
        mutate(stop_loss_e_base_line = close_flagged_by_buy - (stop_loss_base * atr_flagged_by_buy),
               support1_e_line = close_flagged_by_buy + (support1 * atr_flagged_by_buy),
               support2_e_line = close_flagged_by_buy + (support2 * close_flagged_by_buy),
               support3_e_line = close_flagged_by_buy + (support3 * close_flagged_by_buy),
               support1_e_line = case_when(support1_e_line > support2_e_line ~ support2_e_line, TRUE ~ support1_e_line),
               support2_e_line = case_when(support1_e_line > support2_e_line ~ support1_e_line, TRUE ~ support2_e_line)) %>%
        mutate(stop_loss_s_base_line = close_flagged_by_buy2 - (stop_loss_base * atr_flagged_by_buy2),
               support1_s_line = close_flagged_by_buy2 + (support1 * atr_flagged_by_buy2),
               support2_s_line = close_flagged_by_buy2 + (support2 * close_flagged_by_buy2),
               support3_s_line = close_flagged_by_buy2 + (support3 * close_flagged_by_buy2),
               support1_s_line = case_when(support1_s_line > support2_s_line ~ support2_s_line, TRUE ~ support1_s_line),
               support2_s_line = case_when(support1_s_line > support2_s_line ~ support1_s_line, TRUE ~ support2_s_line))

daily_price_target = output2_transformed %>%
        select(symbol, date, close, atr) %>%
        mutate(stop_loss_base_line = close - (stop_loss_base * atr),
               trailing_stop_loss_yesterday = dplyr::lag(stop_loss_base_line, 1),
               support1_line = close + (support1 * atr),
               support2_line = close + (support2 * close),
               support3_line = close + (support3 * close)) %>%
        mutate(support1_line = case_when(support1_line > support2_line ~ support2_line, TRUE ~ support1_line),
               support2_line = case_when(support1_line > support2_line ~ support1_line, TRUE ~ support2_line)) %>%
        arrange(symbol, date)

###########################################
poc <- output2_transformed %>%
        inner_join(target %>% select(symbol, date, in_the_buy_yn, 
                                     stop_loss_e_base_line, 
                                     support1_e_line,
                                     support2_e_line, 
                                     support3_e_line,
                                     stop_loss_s_base_line,
                                     support1_s_line,
                                     support2_s_line,
                                     support3_s_line), 
                   by = c("symbol", "date")) %>%
        # for "forward" evaluatoin, i.e., YTD
        # base on logic from target4 where last_buy_date = min(buy_date)
        mutate(message_e1 = case_when(in_the_buy_yn == 1 & close > support1_e_line ~ "sell - target1", 
                                      in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg1)",
                                      TRUE ~ message_s),
               message_e2 = case_when(in_the_buy_yn == 1 & close > support2_e_line ~ "sell - target2", 
                                      in_the_buy_yn == 1 & close_lag1 >= support1_e_line & close < support1_e_line ~ "sell - stop-loss (msg2)",
                                      in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg2)",
                                      TRUE ~ message_s),
               message_e3 = case_when(in_the_buy_yn == 1 & close > support3_e_line ~ "sell - target3", 
                                      in_the_buy_yn == 1 & close_lag1 >= support2_e_line & close < support2_e_line ~ "sell - stop-loss (msg3)",
                                      in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg3)",
                                      TRUE ~ message_s)) %>%
        # for "random" evaluatoin, i.e., rand_list_target_dates
        # base on logic from target4 where last_buy_date2 = max(buy_date)
        # this method should be more accurate in random evaluaton because it would constantly update (sync up) the base and support lines when the price is moving up
        mutate(message_s1 = case_when(in_the_buy_yn == 1 & close > support1_s_line ~ "sell - target1", 
                                      in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg1)",
                                      TRUE ~ message_s),
               message_s2 = case_when(in_the_buy_yn == 1 & close > support2_s_line ~ "sell - target2", 
                                      in_the_buy_yn == 1 & close_lag1 >= support1_s_line & close < support1_s_line ~ "sell - stop-loss (msg2)",
                                      in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg2)",
                                      TRUE ~ message_s),
               message_s3 = case_when(in_the_buy_yn == 1 & close > support3_s_line ~ "sell - target3", 
                                      in_the_buy_yn == 1 & close_lag1 >= support2_s_line & close < support2_s_line ~ "sell - stop-loss (msg3)",
                                      in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg3)",
                                      TRUE ~ message_s)) %>%
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

# choose message_e* for YTD evaluation
poc.eval <- poc %>%
        mutate(across(matches("message_e([[:digit:]]$)"), msg_string_update)) %>%
        select(symbol, year, date, open, high, low, close, contains("message_e")) %>%
        arrange(symbol, date)

############# >>>>>>>>>>>>>>>>>>>>>>>>> #################################################
#symbols = c('UBER')
symbols = poc$symbol %>% unique()
temp_symbol_list = vector(mode = "list", length = length(symbols))

# choose message_e* for YTD evaluation
message_list <- names(poc) %>% grep(pattern = "message_e([[:digit:]]$)", ignore.case = TRUE, value = TRUE)
fund_begin = c(2000, 3000, 5000)
fund_df = data.frame(message_type = message_list, fund_begin)

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
                
                fund = fund_begin[j]
                y1 <- strategyEval(fund_begin = fund, x) %>% .$net_value
                
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
        dplyr::mutate(net_value = sum(c_across(starts_with("message_e"))),
                      net_value_chg = net_value / sum(fund_begin)) %>%
        arrange(desc(net_value_chg))

simpoc2 %>% head()

##########################################################################################
####################################### >>>>>>>>>>>>>>>>>>>>>>>>>> ###################
####################### <<< summary stat >>> #####################################
simpoc3 <- simpoc %>%
        dplyr::inner_join(fund_df, by = "message_type") %>%
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










