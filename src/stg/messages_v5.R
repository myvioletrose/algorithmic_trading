# define risk tolerance factor (downside risk) for calculation of stop-loss and trailing stop-loss
risk_tolerance = 2

# subset data
subset_date = "2016-01-01"

# begin transformation
indicators_transformed <- indicators %>%
        #filter(date >= subset_date) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%        
        dplyr::mutate(
                # trailing_stop_loss
                trailing_stop_loss = close - risk_tolerance*atr,
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
                                      #ce_short_spike_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - ce_spike",
                                      #cci_oversold_flag == 1 & close > zlema & (candle_stick_pattern == 1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == 1)) ~ "buy - cci",
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
                                      ce_long_dip_flag == 1 & close < zlema & (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) ~ "sell - ce_dip",                                      
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
               sma50,
               sma200,               
               zlema,
               evwma,
               
               chanExit_short,
               chanExit_long,
               
               cci,
               rsi,
               macd_trend_dir,
               is_intraday_green_yn_ha,
               
               proxy_flag,
               sma5_flag,
               rsi_oversold_flag, 
               rsi_overbought_flag, 
               cci_oversold_flag, 
               cci_overbought_flag,
               
               ce_short_spike_flag,
               ce_long_dip_flag,
               
               situation,
               
               candle_stick_pattern,
               candle_stick_pattern_lag1,
               
               contains("csp_"),
               matches("message_[bs]")
        ) %>%
        arrange(symbol, date)

#################################################################################################################
################## <<< stop-loss, stop (support), target (resistance) calculation >>> ########################################
# stop-loss - set at ATR * 'x' below flagged close
stop_loss_base = 2
# first stop/support - set at ATR * 'x' above flagged close
support1 = 1
# second stop/support - set at ATR * 'x' above flagged close
support2 = 2
# third stop/support - set at ATR * 'x' above flagged close
support3 = 3
# profit secure (a) - set at 'x' percent above flagged close
secure_a = 0.05
# profit secure (b) - set at 'x' percent above flagged close
secure_b = 0.1
# profit secure (c) - set at 'x' percent above flagged close
secure_c = 0.2

# begin series of transformation
pricing = indicators_transformed %>%
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

day_since_last_buy = target5 %>%
        filter(!is.na(last_buy_date)) %>%
        arrange(symbol, date) %>%
        group_by(symbol, last_buy_date) %>%
        mutate(day_since_last_buy = row_number()) %>%
        ungroup() %>%
        select(symbol, date, last_buy_date, day_since_last_buy)

day_since_last_buy2 = target5 %>%
        filter(!is.na(last_buy_date2)) %>%
        arrange(symbol, date) %>%
        group_by(symbol, last_buy_date2) %>%
        mutate(day_since_last_buy2 = row_number()) %>%
        ungroup() %>%
        select(symbol, date, last_buy_date2, day_since_last_buy2)

target = target5 %>%
        mutate(stop_loss_e_base_line = close_flagged_by_buy - (stop_loss_base * atr_flagged_by_buy),
               support1_e_line = close_flagged_by_buy + (support1 * atr_flagged_by_buy),
               support2_e_line = close_flagged_by_buy + (support2 * atr_flagged_by_buy),
               support3_e_line = close_flagged_by_buy + (support3 * atr_flagged_by_buy),
               profit_secure1_e_line = close_flagged_by_buy + (secure_a * close_flagged_by_buy),
               profit_secure2_e_line = close_flagged_by_buy + (secure_b * close_flagged_by_buy),
               profit_secure3_e_line = close_flagged_by_buy + (secure_c * close_flagged_by_buy)) %>%
        mutate(stop_loss_s_base_line = close_flagged_by_buy2 - (stop_loss_base * atr_flagged_by_buy2),
               support1_s_line = close_flagged_by_buy2 + (support1 * atr_flagged_by_buy2),
               support2_s_line = close_flagged_by_buy2 + (support2 * atr_flagged_by_buy2),
               support3_s_line = close_flagged_by_buy2 + (support3 * atr_flagged_by_buy2),
               profit_secure1_s_line = close_flagged_by_buy2 + (secure_a * close_flagged_by_buy2),
               profit_secure2_s_line = close_flagged_by_buy2 + (secure_b * close_flagged_by_buy2),
               profit_secure3_s_line = close_flagged_by_buy2 + (secure_c * close_flagged_by_buy2)) %>%
        left_join(day_since_last_buy, by = c("symbol", "date", "last_buy_date")) %>%
        left_join(day_since_last_buy2, by = c("symbol", "date", "last_buy_date2")) %>%
        arrange(symbol, date)

daily_price_target = indicators_transformed %>%
        select(symbol, date, close, atr) %>%
        mutate(stop_loss_base_line = close - (stop_loss_base * atr),
               trailing_stop_loss_yesterday = dplyr::lag(stop_loss_base_line, 1),
               support1_line = close + (support1 * atr),
               support2_line = close + (support2 * atr),
               support3_line = close + (support3 * atr),
               profit_secure1_line = close + (secure_a * close),
               profit_secure2_line = close + (secure_b * close),
               profit_secure3_line = close + (secure_c * close)) %>%
        arrange(symbol, date)

#################################################################################################################
################## <<< identify first-buy >>> ########################################
first_buy = target3 %>%
        left_join(target2, join_by(symbol, date >= buy_date, last_sell_date < buy_date)) %>%
        group_by(symbol, date, last_sell_date) %>%
        summarise(first_buy_date = min(buy_date)) %>%
        ungroup() %>%
        filter(!is.na(first_buy_date)) %>%
        select(symbol, first_buy_date) %>%
        mutate(is_first_buy_yn = 1) %>%
        distinct() %>%
        arrange(symbol, first_buy_date)

#################################################################################################################
################## <<< identify daily support, target >>> ########################################
# helper function, i.e., get the prior-value, next-value after sorting
value_return_by_pos <- function(x, compare_with, return_pos = "prior"){
        
        y = c(x, compare_with) %>% sort()
        pos = match(compare_with, y)
        pos_prior = if(pos -1 <= 0){pos_prior = 1} else {pos -1}
        pos_next = if(pos +1 > length(y)){pos_next = length(y)} else {pos +1}
        
        if(return_pos == "prior"){
                return(y[pos_prior])
        } else {
                return(y[pos_next])
        }
        
}

targetSubset = target %>%
        inner_join(indicators_transformed %>% select(symbol, date, close_lag1, trailing_stop_loss_yesterday), 
                   by = c("symbol", "date")) %>%
        filter(in_the_buy_yn == 1) %>%
        #filter(date >= subset_date) %>%
        select(symbol, date, close, close_lag1,
               trailing_stop_loss_yesterday,
               stop_loss_e_base_line,
               support1_e_line,
               support2_e_line,
               support3_e_line,
               profit_secure1_e_line,
               profit_secure2_e_line,
               profit_secure3_e_line,
               stop_loss_s_base_line,
               support1_s_line,
               support2_s_line,
               support3_s_line,
               profit_secure1_s_line,
               profit_secure2_s_line,
               profit_secure3_s_line) %>%
        arrange(symbol, date)

daily_support_e = vector(mode = "list", length = nrow(targetSubset))
daily_target_e = vector(mode = "list", length = nrow(targetSubset))
# daily_support_s = vector(mode = "list", length = nrow(targetSubset))
# daily_target_s = vector(mode = "list", length = nrow(targetSubset))

tic()
# nrow(targetSubset)
# [1] 11620

for(i in 1:nrow(targetSubset)){
        
        compare_this_value = targetSubset$close_lag1[i]
        
        daily_support_e[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
                                                                stop_loss_e_base_line,
                                                                support1_e_line,
                                                                support2_e_line,
                                                                support3_e_line), 
                                   MARGIN = 1, 
                                   FUN = value_return_by_pos, 
                                   compare_with = compare_this_value, 
                                   return = "prior")
        
        daily_target_e[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
                                                               stop_loss_e_base_line,
                                                               support1_e_line,
                                                               support2_e_line,
                                                               support3_e_line), 
                                  MARGIN = 1, 
                                  FUN = value_return_by_pos, 
                                  compare_with = compare_this_value, 
                                  return = "next")
        
        # daily_support_s[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
        #                                                         stop_loss_s_base_line,
        #                                                         support1_s_line,
        #                                                         support2_s_line,
        #                                                         support3_s_line), 
        #                            MARGIN = 1, 
        #                            FUN = value_return_by_pos, 
        #                            compare_with = compare_this_value, 
        #                            return = "prior")
        # 
        # daily_target_s[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
        #                                                        stop_loss_s_base_line,
        #                                                        support1_s_line,
        #                                                        support2_s_line,
        #                                                        support3_s_line), 
        #                           MARGIN = 1, 
        #                           FUN = value_return_by_pos, 
        #                           compare_with = compare_this_value, 
        #                           return = "next")
        
}

toc()
#elapsed time is 422.240000 seconds 

targetSubset2 = cbind(targetSubset, 
                      daily_support_e = unlist(daily_support_e), 
                      daily_target_e = unlist(daily_target_e)
                      #daily_support_s = unlist(daily_support_s), 
                      #daily_target_s = unlist(daily_target_s)
                      ) %>%
        select(symbol, date, close, close_lag1,
               trailing_stop_loss_yesterday,
               stop_loss_e_base_line,
               support1_e_line,
               support2_e_line,
               support3_e_line,
               profit_secure1_e_line,
               profit_secure2_e_line,
               profit_secure3_e_line,
               daily_support_e,
               daily_target_e
               # stop_loss_s_base_line,
               # support1_s_line,
               # support2_s_line,
               # support3_s_line,
               # profit_secure1_s_line,
               # profit_secure2_s_line,
               # profit_secure3_s_line,
               #daily_support_s,
               #daily_target_s
               ) %>%
        arrange(symbol, date)

###############################################################################################################################
# put together - specify daily support, target, (re)define selling conditions
poc0 <- indicators_transformed %>%
        inner_join(target %>% select(symbol, date, 
                                     in_the_buy_yn, day_since_last_buy, day_since_last_buy2,
                                     stop_loss_e_base_line, 
                                     support1_e_line,
                                     support2_e_line, 
                                     support3_e_line,
                                     profit_secure1_e_line,
                                     profit_secure2_e_line,
                                     profit_secure3_e_line
                                     # stop_loss_s_base_line,
                                     # support1_s_line,
                                     # support2_s_line,
                                     # support3_s_line,
                                     # profit_secure1_s_line,
                                     # profit_secure2_s_line,
                                     # profit_secure3_s_line
                                     ), 
                   by = c("symbol", "date")) %>%        
        left_join(targetSubset2 %>% 
                          select(symbol, date,
                                 daily_support_e, daily_target_e
                                 #daily_support_s, daily_target_s
                                 ), 
                  by = c("symbol", "date")) %>%
        # for "forward" evaluatoin, i.e., YTD
        # base on logic from target4 where last_buy_date = min(buy_date)
        mutate(
                # message_e1 = case_when(in_the_buy_yn == 1 & close > support1_e_line ~ "sell - target1", 
                #                        in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg1)",
                #                        TRUE ~ message_s),
                # message_e2 = case_when(in_the_buy_yn == 1 & close > support2_e_line ~ "sell - target2",
                #                       in_the_buy_yn == 1 & close_lag1 >= support1_e_line & close < support1_e_line ~ "sell - stop-loss (msg2)",
                #                       in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg2)",
                #                       TRUE ~ message_s),
                # message_e3 = case_when(in_the_buy_yn == 1 & close > support3_e_line ~ "sell - target3", 
                #                        in_the_buy_yn == 1 & close_lag1 >= support2_e_line & close < support2_e_line ~ "sell - stop-loss (msg3)",
                #                        in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg3)",
                #                        TRUE ~ message_s),
                # message_e4 = case_when(in_the_buy_yn == 1 & 
                #                                (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
                #                                close < daily_support_e & close_lag1 >= daily_support_e ~ "sell - break support (msg4)",
                #                        in_the_buy_yn == 1 & close > support3_e_line ~ "sell - break target (msg4)", 
                #                        in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg4)",
                #                        TRUE ~ message_s),
                # message_e5 = case_when(in_the_buy_yn == 1 &                                                
                #                                (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
                #                                close < daily_support_e & close_lag1 >= daily_support_e ~ "sell - break support (msg5)",
                #                        in_the_buy_yn == 1 & close > profit_secure2_e_line ~ "sell - break target (msg5)", 
                #                        in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg5)",
                #                        TRUE ~ message_s),
                # message_e6 = case_when(in_the_buy_yn == 1 &                                                
                #                                (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
                #                                close < daily_support_e & close_lag1 >= daily_support_e ~ "sell - break support (msg6)",
                #                        in_the_buy_yn == 1 & close > profit_secure3_e_line ~ "sell - break target (msg6)",
                #                        in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg6)",
                #                        TRUE ~ message_s)
                message_e1 = case_when(in_the_buy_yn == 1 & 
                                               (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
                                               close < daily_support_e & close_lag1 >= daily_support_e ~ "sell - break support (msg1)",
                                       in_the_buy_yn == 1 & close > support3_e_line ~ "sell - break target (msg1)", 
                                       in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg1)",
                                       TRUE ~ message_s),
                message_e2 = case_when(in_the_buy_yn == 1 &                                                
                                               (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
                                               close < daily_support_e & close_lag1 >= daily_support_e ~ "sell - break support (msg2)",
                                       in_the_buy_yn == 1 & close > profit_secure3_e_line ~ "sell - break target (msg2)",
                                       in_the_buy_yn == 1 & close < stop_loss_e_base_line ~ "sell - stop-loss (msg2)",
                                       TRUE ~ message_s)
                ) %>%
        # for "random" evaluatoin, i.e., rand_list_target_dates
        # base on logic from target4 where last_buy_date2 = max(buy_date)
        # this method should be more accurate in random evaluaton because it would constantly update (sync up) the base and support lines when the price is moving up
        # mutate(
        #         # message_s1 = case_when(in_the_buy_yn == 1 & close > support1_s_line ~ "sell - target1", 
        #         #                        in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg1)",
        #         #                        TRUE ~ message_s),
        #         # message_s2 = case_when(in_the_buy_yn == 1 & close > support2_s_line ~ "sell - target2",
        #         #                        in_the_buy_yn == 1 & close_lag1 >= support1_s_line & close < support1_s_line ~ "sell - stop-loss (msg2)",
        #         #                        in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg2)",
        #         #                        TRUE ~ message_s),
        #         # message_s3 = case_when(in_the_buy_yn == 1 & close > support3_s_line ~ "sell - target3", 
        #         #                        in_the_buy_yn == 1 & close_lag1 >= support2_s_line & close < support2_s_line ~ "sell - stop-loss (msg3)",
        #         #                        in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg3)",
        #         #                        TRUE ~ message_s),
        #         message_s4 = case_when(in_the_buy_yn == 1 &                                                
        #                                        (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
        #                                        close < daily_support_s & close_lag1 >= daily_support_s ~ "sell - break support (msg4)",
        #                                in_the_buy_yn == 1 & close > support3_s_line ~ "sell - break target (msg4)",
        #                                in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg4)",
        #                                TRUE ~ message_s),
        #         message_s5 = case_when(in_the_buy_yn == 1 &                                                
        #                                        (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
        #                                        close < daily_support_s & close_lag1 >= daily_support_s ~ "sell - break support (msg5)",
        #                                in_the_buy_yn == 1 & close > profit_secure2_s_line ~ "sell - break target (msg5)",
        #                                in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg5)",
        #                                TRUE ~ message_s),
        #         message_s6 = case_when(in_the_buy_yn == 1 &                                                
        #                                        (candle_stick_pattern == -1 | (candle_stick_pattern == 0 & candle_stick_pattern_lag1 == -1)) & 
        #                                        close < daily_support_s & close_lag1 >= daily_support_s ~ "sell - break support (msg6)",
        #                                in_the_buy_yn == 1 & close > profit_secure3_s_line ~ "sell - break target (msg6)",
        #                                in_the_buy_yn == 1 & close < stop_loss_s_base_line ~ "sell - stop-loss (msg6)",
        #                                TRUE ~ message_s)) %>%
        arrange(symbol, date)

###############################################################################################################################
# red flag protocol
rfp <- poc0 %>%
        #filter(in_the_buy_yn == 1) %>%
        #filter(!is.na(proxy_flag)) %>%
        select(symbol, date, proxy_flag,
               candle_stick_pattern,
               contains("csp_"),
               -csp_up_trend,
               -csp_down_trend,
               -csp_bullish_candle,
               -csp_bearish_candle,
               -csp_candle_stick_signal) %>%
        tidyr::gather(pattern, value, -symbol, -date, -proxy_flag) %>%
        mutate(red_flag_yn = case_when(proxy_flag == -1 ~ 1, 
                                       is.na(proxy_flag) ~ NA,
                                       TRUE ~ 0)) %>%
        mutate(csp = stringr::str_remove(pattern, "csp_"),
               value = case_when(csp == "candle_stick_pattern" & value == 1 ~ "up",
                                 csp == "candle_stick_pattern" & value == 0 ~ "neutral",
                                 csp == "candle_stick_pattern" & value == -1 ~ "down",
                                 csp != "candle_stick_pattern" & value == 1 ~ csp,
                                 TRUE ~ "")) %>%
        select(symbol, date, red_flag_yn, pattern, value) %>%
        arrange(symbol, date, pattern) %>%
        tidyr::spread(pattern, value)

#with(rfp, round(prop.table(ftable(red_flag_yn, value), margin = 2) * 100, 1))

# candle stick pattern
csp <- rfp %>%
        dplyr::mutate(csp = apply(rfp %>% select(candle_stick_pattern:csp_three_white_soliders), 
                                  MARGIN = 1,
                                  FUN = paste0,
                                  collapse = ":")) %>%
        select(-(candle_stick_pattern:csp_three_white_soliders)) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        mutate(csp = stringr::str_replace(gsub("\\:+", "_", str_trim(csp)), "B", "b"),
               csp = stringr::str_replace(csp, "_$", ""),
               csp_lag1 = lag(csp, 1),
               # combine today and yesterday csp patterns
               csp_signal = paste(csp, csp_lag1, sep = "::")
               #csp_signal = csp
        ) %>%
        ungroup() 

# result at a glance
step = with(csp %>% filter(!is.na(red_flag_yn)), round(prop.table(ftable(red_flag_yn, csp_signal), margin = 2) * 100, 1)) %>% as.data.frame()
step2 = with(csp %>% filter(!is.na(red_flag_yn)), ftable(red_flag_yn, csp_signal)) %>% as.data.frame() %>% select(everything(), n = Freq)
step3 = step2 %>% 
        group_by(csp_signal) %>%
        summarise(total_n = sum(n)) %>%
        ungroup() %>%
        inner_join(step2, by = "csp_signal", multiple = "all")
step4 = inner_join(step, step3, by = c("red_flag_yn", "csp_signal")) %>%
        select(red_flag_yn, csp_signal, n, total_n, freq_red_flag = Freq) %>%
        arrange(red_flag_yn, csp_signal)
step4 %>% dim()  # [1] 3236    5
step4 %>% head(10)
#    red_flag_yn                                       csp_signal  n total_n freq_red_flag
# 1            0                                       down::down 12      18          66.7
# 2            0    down::down_bearish_engulfing_dark_cloud_cover  2       5          40.0
# 3            0       down::down_bearish_harami_dark_cloud_cover  1       3          33.3
# 4            0                   down::down_bearish_harami_doji  1       1         100.0
# 5            0                      down::down_dark_cloud_cover  6       8          75.0
# 6            0            down::down_dark_cloud_cover_kick_down  0       1           0.0
# 7            0                                  down::down_doji  1       2          50.0
# 8            0                       down::down_inverted_harmer  0       1           0.0
# 9            0                                    down::neutral 25      55          45.5
# 10           0 down::neutral_bearish_engulfing_dark_cloud_cover  7      13          53.8

#########################################################################
#########################################################################
# parameters
bayes_para1 <- csp %>%
        filter(date < subset_date) %>%
        filter(!is.na(red_flag_yn)) %>%
        group_by(symbol) %>%
        summarise(alpha = sum(red_flag_yn),
                  beta = n()) %>%
        ungroup()

bayes_para2 <- csp %>%
        filter(date < subset_date) %>%
        filter(!is.na(red_flag_yn)) %>%
        group_by(symbol, csp_signal) %>%
        summarise(obs = sum(red_flag_yn),
                  n = n()) %>%
        ungroup()

# bayes_para3 <- csp %>%
#         filter(date < subset_date) %>%
#         filter(!is.na(red_flag_yn)) %>%
#         group_by(csp_signal) %>%
#         summarise(obs = sum(red_flag_yn),
#                   n = n()) %>%
#         ungroup()

bayes_parameters <- inner_join(bayes_para1, bayes_para2, by = "symbol", multiple = "all") %>%
        select(symbol, csp_signal,
               alpha, beta, obs, n)

# bayes_parameters_update <- bayes_parameters %>%
#         select(symbol, csp_signal, alpha, beta) %>%
#         inner_join(bayes_para3, by = "csp_signal")

#######################################
# Bayesian point estimation
n_sample_list <- vector(mode = "list", length = nrow(bayes_parameters))
mu_list <- vector(mode = "list", length = nrow(bayes_parameters))
std_list <- vector(mode = "list", length = nrow(bayes_parameters))
me_list <- vector(mode = "list", length = nrow(bayes_parameters))
ci_up_list <- vector(mode = "list", length = nrow(bayes_parameters))
ci_down_list <- vector(mode = "list", length = nrow(bayes_parameters))

tic()
for(i in 1:nrow(bayes_parameters)){
        # parameters
        #alpha = bayes_parameters$alpha[i]
        #beta = bayes_parameters$beta[i]
        obs = bayes_parameters$obs[i]
        n = bayes_parameters$n[i]
        # posterior distribution
        #posterior_dist = bayesian_data_simulation(alpha = alpha, beta = beta, n_draw = 10000, obs = obs, n = n)
        posterior_dist = bayesian_data_simulation(n_draw = 10000, obs = obs, n = n, informative_prior_yn = FALSE)
        # n_sample
        n_sample = posterior_dist %>% length()
        # mean
        mu = posterior_dist %>% mean()
        # sd
        std = posterior_dist %>% sd()
        # margin of error
        me = 1.96 * std/sqrt(length(posterior_dist) -1)
        # confidence interval - lower limit
        ci_down = mu - me
        # confidence interval - upper limit
        ci_up = mu + me
        # write result
        n_sample_list[[i]] = n_sample
        mu_list[[i]] = mu
        std_list[[i]] = std
        me_list[[i]] = me
        ci_down_list[[i]] = ci_down
        ci_up_list[[i]] = ci_up
}
toc()

bayes_results <- cbind(bayes_parameters,
                       n_sample = n_sample_list %>% unlist,
                       mu = mu_list %>% unlist,
                       std = std_list %>% unlist,
                       margin_of_error = me_list %>% unlist,
                       ci_down = ci_down_list %>% unlist,
                       ci_up = ci_up_list %>% unlist) %>%
        dplyr::mutate(mu = case_when(is.na(mu) ~ obs/n, TRUE ~ mu))

##############################################################################################
# poc update
red_flag_threshold = 0.5
num_of_day_since_last_buy_date = 11

poc <- poc0 %>%
        left_join(csp %>% select(symbol, date, csp_signal), by = c("symbol", "date")) %>%
        left_join(bayes_results %>% select(symbol, csp_signal, red_flag_pred = mu), by = c("symbol", "csp_signal")) %>%
        mutate(message_e1 = case_when(close < sma50 & red_flag_pred > red_flag_threshold & close < support2_e_line & day_since_last_buy <= num_of_day_since_last_buy_date ~ "sell - red flag protocol", TRUE ~ message_e1),
               message_e2 = case_when(close < sma50 & red_flag_pred > red_flag_threshold & close < support2_e_line & day_since_last_buy <= num_of_day_since_last_buy_date ~ "sell - red flag protocol", TRUE ~ message_e2)
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

# choose message_e* for YTD evaluation
poc.eval <- poc %>%
        mutate(across(matches("message_e([[:digit:]]$)"), msg_string_update)) %>%
        select(symbol, year, date, open, high, low, close, contains("message_e")) %>%
        arrange(symbol, date)

############# >>>>>>>>>>>>>>>>>>>>>>>>> #################################################
#symbols = c('GOOGL')
symbols = poc$symbol %>% unique()
temp_symbol_list = vector(mode = "list", length = length(symbols))

# choose message_e* for YTD evaluation
message_list <- names(poc) %>% grep(pattern = "message_e([[:digit:]]$)", ignore.case = TRUE, value = TRUE)
fund_begin = c(1000, 1000)
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

# > simpoc
# start_date   end_date symbol  net_value message_type
# 1  2022-01-01 2022-12-31    AMC  714.15415   message_e1
# 2  2022-01-01 2022-12-31    AMC  708.31542   message_e2
# 3  2022-01-01 2022-12-31   AMZN   11.86241   message_e1
# 4  2022-01-01 2022-12-31   AMZN   50.39472   message_e2
# 5  2022-01-01 2022-12-31   DKNG  -84.10464   message_e1
# 6  2022-01-01 2022-12-31   DKNG  -84.10464   message_e2
# 7  2022-01-01 2022-12-31  GOOGL  -52.72673   message_e1
# 8  2022-01-01 2022-12-31  GOOGL  -52.72673   message_e2
# 9  2022-01-01 2022-12-31   META -168.56830   message_e1
# 10 2022-01-01 2022-12-31   META -168.56830   message_e2
# 11 2022-01-01 2022-12-31   PLUG  467.10607   message_e1
# 12 2022-01-01 2022-12-31   PLUG  259.61186   message_e2
# 13 2022-01-01 2022-12-31   PRPL -315.73649   message_e1
# 14 2022-01-01 2022-12-31   PRPL -315.73649   message_e2
# 15 2022-01-01 2022-12-31    SPY   20.13089   message_e1
# 16 2022-01-01 2022-12-31    SPY   20.13089   message_e2
# 17 2022-01-01 2022-12-31   TSLA  310.03375   message_e1
# 18 2022-01-01 2022-12-31   TSLA  345.69102   message_e2
# 19 2022-01-01 2022-12-31   UBER   41.85437   message_e1
# 20 2022-01-01 2022-12-31   UBER   41.85437   message_e2
# > simpoc2
# # A tibble: 10 × 7
# # Rowwise: 
# start_date end_date   symbol message_e1 message_e2 net_value net_value_chg
# <chr>      <chr>      <chr>       <dbl>      <dbl>     <dbl>         <dbl>
# 1 2022-01-01 2022-12-31 AMC         714.       708.     1422.         0.711 
# 2 2022-01-01 2022-12-31 PLUG        467.       260.      727.         0.363 
# 3 2022-01-01 2022-12-31 TSLA        310.       346.      656.         0.328 
# 4 2022-01-01 2022-12-31 UBER         41.9       41.9      83.7        0.0419
# 5 2022-01-01 2022-12-31 AMZN         11.9       50.4      62.3        0.0311
# 6 2022-01-01 2022-12-31 SPY          20.1       20.1      40.3        0.0201
# 7 2022-01-01 2022-12-31 GOOGL       -52.7      -52.7    -105.        -0.0527
# 8 2022-01-01 2022-12-31 DKNG        -84.1      -84.1    -168.        -0.0841
# 9 2022-01-01 2022-12-31 META       -169.      -169.     -337.        -0.169 
# 10 2022-01-01 2022-12-31 PRPL       -316.      -316.     -631.        -0.316 
# > simpoc3
#    start_date   end_date symbol  net_value message_type fund_begin is_win_yn percent_chg
# 1  2022-01-01 2022-12-31    AMC  714.15415   message_e1       1000         1  0.71415415
# 2  2022-01-01 2022-12-31    AMC  708.31542   message_e2       1000         1  0.70831542
# 3  2022-01-01 2022-12-31   AMZN   11.86241   message_e1       1000         1  0.01186241
# 4  2022-01-01 2022-12-31   AMZN   50.39472   message_e2       1000         1  0.05039472
# 5  2022-01-01 2022-12-31   DKNG  -84.10464   message_e1       1000         0 -0.08410464
# 6  2022-01-01 2022-12-31   DKNG  -84.10464   message_e2       1000         0 -0.08410464
# 7  2022-01-01 2022-12-31  GOOGL  -52.72673   message_e1       1000         0 -0.05272673
# 8  2022-01-01 2022-12-31  GOOGL  -52.72673   message_e2       1000         0 -0.05272673
# 9  2022-01-01 2022-12-31   META -168.56830   message_e1       1000         0 -0.16856830
# 10 2022-01-01 2022-12-31   META -168.56830   message_e2       1000         0 -0.16856830
# 11 2022-01-01 2022-12-31   PLUG  467.10607   message_e1       1000         1  0.46710607
# 12 2022-01-01 2022-12-31   PLUG  259.61186   message_e2       1000         1  0.25961186
# 13 2022-01-01 2022-12-31   PRPL -315.73649   message_e1       1000         0 -0.31573649
# 14 2022-01-01 2022-12-31   PRPL -315.73649   message_e2       1000         0 -0.31573649
# 15 2022-01-01 2022-12-31    SPY   20.13089   message_e1       1000         1  0.02013089
# 16 2022-01-01 2022-12-31    SPY   20.13089   message_e2       1000         1  0.02013089
# 17 2022-01-01 2022-12-31   TSLA  310.03375   message_e1       1000         1  0.31003375
# 18 2022-01-01 2022-12-31   TSLA  345.69102   message_e2       1000         1  0.34569102
# 19 2022-01-01 2022-12-31   UBER   41.85437   message_e1       1000         1  0.04185437
# 20 2022-01-01 2022-12-31   UBER   41.85437   message_e2       1000         1  0.04185437