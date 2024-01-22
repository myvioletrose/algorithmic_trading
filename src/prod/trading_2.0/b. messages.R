# define risk tolerance factor (downside risk) for calculation of stop-loss and trailing stop-loss
risk_tolerance = 2

# subset data
#subset_date = "1990-01-01"
#subset_symbols = symbols

# begin transformation
indicators_transformed <- indicators %>%
        filter(date >= subset_date) %>%
        filter(symbol %in% subset_symbols) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%        
        dplyr::mutate(
                # close lag, lead, near_future_flags
                close_lag1 = lag(close, 1),
                close_next2 = lead(close, 2),
                close_next4 = lead(close, 4),
                close_next10 = lead(close, 10),
                near_future_flag2 = case_when(close_next2 > close ~ 1,
                                              close_next2 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag4 = case_when(close_next4 > close ~ 1,
                                              close_next4 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag10 = case_when(close_next10 > close ~ 1,
                                               close_next10 < close ~ -1,
                                               TRUE ~ 0),
                
                # trailing_stop_loss
                trailing_stop_loss = close - risk_tolerance*atr,
                trailing_stop_loss_yesterday = dplyr::lag(trailing_stop_loss, 1),
                
                # sma_trend_flag, ema_trend_flag
                sma_pos_trend_flag = case_when(close > sma5 & close > sma8 & close > sma13 ~ 1, TRUE ~ 0),                
                ema_pos_trend_flag = case_when(close > ema5 & close > ema20 ~ 1, TRUE ~ 0),
                
                # buy alert if,
                message_b = case_when(dcc_flag == 1 & close > zlema & ema5 > ema20 & demark_flag != -1 & rsi > 50 & sma5_flag == 1 ~ "buy - dcc",                                                                            
                                      
                                      demark_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ "buy - demark (v0)",
                                      demark_flag == 1 & ema5 > ema20 ~ "buy - demark (v1)",
                                      
                                      evwma_flag == 1 & close > zlema & ema5 > ema20 & demark_flag != -1 & rsi > 50 & (dcc_flag == 1 | demark_flag == 1 | ha_real_flag == 1 | ha_smooth_flag == 1 | macd_flag == 1) ~ "buy - evwma (v0)",                                      
                                      evwma_flag == 1 & close > zlema & ema5 > ema20 & demark_flag != -1 & rsi > 50 & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) & obv_flag == 1 ~ "buy - evwma (v1)",
                                      
                                      ha_real_flag == 1 & close > zlema & ema5 > ema20 & demark_flag != -1 & rsi > 50 & green_flag == 1 & (rsi_trend_dir == 1 | cci_trend_dir == 1 | macd_trend_dir == 1 | obv_flag == 1) ~ "buy - ha_real",

                                      macd_flag == 1 & close > zlema & ema5 > ema20 & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ "buy - macd",
                                      
                                      sma5_flag == 1 & close > zlema & ema5 > ema20 & demark_flag != -1 & rsi > 50 & sma_pos_trend_flag == 1 ~ "buy - sma (v0)",
                                      sma5_flag == 1 & close > zlema & demark_flag != -1 & rsi > 50 & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) & obv_flag == 1 ~ "buy - sma (v1)",
                                      
                                      TRUE ~ "hold"),
                
                # sell alert if,
                message_s = case_when(ce_long_dip_flag == 1 & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - ce",
                                      evwma_flag == -1 & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - evwma",                                      
                                      macd_flag == -1 & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - macd",                                      
                                      ha_real_flag == -1 & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - ha_real",
                                      #ha_smooth_flag == -1 & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - ha_smooth",                                      
                                      (cci_overbought_flag == 1 | rsi_overbought_flag == 1) & close < zlema & ema5 < ema20 & demark_flag != 1 ~ "down_alert - overbought",
                                      demark_flag == -1 & close < zlema ~ "down_alert - demark",
                                      
                                      # profit protection
                                      close < trailing_stop_loss_yesterday ~ "sell - profit protect",
                                      TRUE ~ message_b)
        ) %>%
        ungroup() %>%        
        arrange(symbol, date)

# # add "buy - handpicked"
# indicators_transformed <- indicators_transformed %>%
#         mutate(message_b = case_when(symbol == "CZR" & date == "2023-10-31" ~ "buy - handpicked",
#                                      symbol == "GOOGL" & date == "2023-11-01" ~ "buy - handpicked",
#                                      symbol == "SNAP" & date == "2023-10-31" ~ "buy - handpicked",
#                                      symbol == "VRT" & date == "2023-10-31" ~ "buy - handpicked",
#                                      symbol == "TSLA" & date == "2023-11-07" ~ "buy - handpicked",
#                                      symbol == "MRK" & date == "2023-12-04" ~ "buy - handpicked",
#                                      symbol == "CF" & date == "2023-12-07" ~ "buy - handpicked",
#                                      symbol == "META" & date == "2023-12-08" ~ "buy - handpicked",
#                                      TRUE ~ message_b),
#                message_s = case_when(symbol == "CZR" & date == "2023-10-31" ~ message_b,
#                                      symbol == "GOOGL" & date == "2023-11-01" ~ message_b,
#                                      symbol == "SNAP" & date == "2023-10-31" ~ message_b,
#                                      symbol == "VRT" & date == "2023-10-31" ~ message_b,
#                                      symbol == "TSLA" & date == "2023-11-07" ~ message_b,
#                                      symbol == "MRK" & date == "2023-12-04" ~ message_b,
#                                      symbol == "CF" & date == "2023-12-07" ~ message_b,
#                                      symbol == "META" & date == "2023-12-08" ~ message_b,
#                                      TRUE ~ message_s))

#################################################################################################################
################## <<< stop-loss, support, target calculation >>> ########################################
# stop-loss - set at ATR * 'x' below flagged close
stop_loss_base = 2
# first stop/support - set at ATR * 'x' above flagged close
support1 = 1
# second stop/support - set at ATR * 'x' above flagged close
support2 = 2
# third stop/support - set at ATR * 'x' above flagged close
support3 = 3
# profit target (a) - set at 'x' percent above flagged close
target_a = 0.03
# profit target (b) - set at 'x' percent above flagged close
target_b = 0.1
# profit target (c) - set at 'x' percent above flagged close
target_c = 0.2

# begin series of transformation
target0 = indicators_transformed %>%
        select(symbol, date, close, atr, message_b, message_s) %>%
        arrange(symbol, date)

target1 = target0 %>%
        filter(grepl("sell|down_alert", message_s, ignore.case = TRUE)) %>%
        select(symbol, sell_date = date, close, atr, message_b, message_s)

target2 = target0 %>%
        filter(grepl("buy", message_b, ignore.case = TRUE)) %>%
        select(symbol, buy_date = date, close, atr, message_b, message_s)

target3 = target0 %>%
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
        left_join(target0, join_by(symbol, last_buy_date == date)) %>%
        select(symbol, date, last_buy_date, 
               close_flagged_by_buy = close,
               atr_flagged_by_buy = atr) %>%
        inner_join(target0 %>% select(symbol, date, close, atr), by = c("symbol", "date")) %>%
        mutate(close_flagged_by_buy = case_when(is.na(close_flagged_by_buy) ~ close, TRUE ~ close_flagged_by_buy),
               atr_flagged_by_buy = case_when(is.na(atr_flagged_by_buy) ~ atr, TRUE ~ atr_flagged_by_buy),
               in_the_buy_yn = case_when(is.na(last_buy_date) ~ 0, TRUE ~ 1))

target5.2 = target4 %>%
        left_join(target0, join_by(symbol, last_buy_date2 == date)) %>%
        select(symbol, date, 
               last_buy_date2, 
               close_flagged_by_buy2 = close,
               atr_flagged_by_buy2 = atr) %>%
        inner_join(target0 %>% select(symbol, date, close, atr), by = c("symbol", "date")) %>%
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
               profit_target1_e_line = close_flagged_by_buy + (target_a * close_flagged_by_buy),               
               profit_target2_e_line = close_flagged_by_buy + (target_b * close_flagged_by_buy),
               profit_target3_e_line = close_flagged_by_buy + (target_c * close_flagged_by_buy),
               # update support1_e_line (based on the profit_target1_e_line, by default (as of 2023-12-11, i.e., 3% -> the exit target threshold))
               support1_e_line = case_when(support1_e_line > profit_target1_e_line ~ profit_target1_e_line, TRUE ~ support1_e_line)) %>%
        mutate(stop_loss_s_base_line = close_flagged_by_buy2 - (stop_loss_base * atr_flagged_by_buy2),
               support1_s_line = close_flagged_by_buy2 + (support1 * atr_flagged_by_buy2),
               support2_s_line = close_flagged_by_buy2 + (support2 * atr_flagged_by_buy2),
               support3_s_line = close_flagged_by_buy2 + (support3 * atr_flagged_by_buy2),
               profit_target1_s_line = close_flagged_by_buy2 + (target_a * close_flagged_by_buy2),
               profit_target2_s_line = close_flagged_by_buy2 + (target_b * close_flagged_by_buy2),
               profit_target3_s_line = close_flagged_by_buy2 + (target_c * close_flagged_by_buy2),
               # update support1_s_line (based on the profit_target1_s_line, by default (as of 2023-12-11, i.e., 3% -> the exit target threshold))
               support1_s_line = case_when(support1_s_line > profit_target1_s_line ~ profit_target1_s_line, TRUE ~ support1_s_line)) %>%
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
               profit_target1_line = close + (target_a * close),
               profit_target2_line = close + (target_b * close),
               profit_target3_line = close + (target_c * close)) %>%
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
        filter(date >= subset_date) %>%
        select(symbol, date, close, close_lag1,
               trailing_stop_loss_yesterday,
               stop_loss_e_base_line,
               support1_e_line,
               support2_e_line,
               support3_e_line,
               profit_target1_e_line,
               profit_target2_e_line,
               profit_target3_e_line,
               stop_loss_s_base_line,
               support1_s_line,
               support2_s_line,
               support3_s_line,
               profit_target1_s_line,
               profit_target2_s_line,
               profit_target3_s_line) %>%
        arrange(symbol, date)

daily_support_e = vector(mode = "list", length = nrow(targetSubset))
daily_ceiling_e = vector(mode = "list", length = nrow(targetSubset))
daily_support_s = vector(mode = "list", length = nrow(targetSubset))
daily_ceiling_s = vector(mode = "list", length = nrow(targetSubset))

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
        
        daily_ceiling_e[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
                                                               stop_loss_e_base_line,
                                                               support1_e_line,
                                                               support2_e_line,
                                                               support3_e_line), 
                                  MARGIN = 1, 
                                  FUN = value_return_by_pos, 
                                  compare_with = compare_this_value, 
                                  return = "next")
        
        daily_support_s[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
                                                                stop_loss_s_base_line,
                                                                support1_s_line,
                                                                support2_s_line,
                                                                support3_s_line),
                                   MARGIN = 1,
                                   FUN = value_return_by_pos,
                                   compare_with = compare_this_value,
                                   return = "prior")

        daily_ceiling_s[i] = apply(targetSubset[i, ] %>% select(trailing_stop_loss_yesterday,
                                                               stop_loss_s_base_line,
                                                               support1_s_line,
                                                               support2_s_line,
                                                               support3_s_line),
                                  MARGIN = 1,
                                  FUN = value_return_by_pos,
                                  compare_with = compare_this_value,
                                  return = "next")
        
}

toc()
#elapsed time is 422.240000 seconds 

targetSubset2 = cbind(targetSubset, 
                      daily_support_e = unlist(daily_support_e), 
                      daily_ceiling_e = unlist(daily_ceiling_e),
                      daily_support_s = unlist(daily_support_s), 
                      daily_ceiling_s = unlist(daily_ceiling_s)
) %>%
        select(symbol, date, close, close_lag1,
               trailing_stop_loss_yesterday,
               stop_loss_e_base_line,
               support1_e_line,
               support2_e_line,
               support3_e_line,
               profit_target1_e_line,
               profit_target2_e_line,
               profit_target3_e_line,
               daily_support_e,
               daily_ceiling_e,
               stop_loss_s_base_line,
               support1_s_line,
               support2_s_line,
               support3_s_line,
               profit_target1_s_line,
               profit_target2_s_line,
               profit_target3_s_line,
               daily_support_s,
               daily_ceiling_s
        ) %>%
        arrange(symbol, date)

###############################################################################################################################
###############################################################################################################################
# put together - specify daily support, target, (re)define selling conditions
poc0 <- indicators_transformed %>%
        inner_join(target %>% select(symbol, date, 
                                     in_the_buy_yn, day_since_last_buy, day_since_last_buy2,
                                     stop_loss_e_base_line, 
                                     support1_e_line,
                                     support2_e_line, 
                                     support3_e_line,
                                     profit_target1_e_line,
                                     profit_target2_e_line,
                                     profit_target3_e_line,
                                     stop_loss_s_base_line,
                                     support1_s_line,
                                     support2_s_line,
                                     support3_s_line,
                                     profit_target1_s_line,
                                     profit_target2_s_line,
                                     profit_target3_s_line
        ), 
        by = c("symbol", "date")) %>%        
        left_join(targetSubset2 %>% 
                          select(symbol, date,
                                 daily_support_e, daily_ceiling_e,
                                 daily_support_s, daily_ceiling_s
                          ), 
                  by = c("symbol", "date")) %>%
        left_join(first_buy, by = c("symbol" = "symbol", "date" = "first_buy_date")) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        mutate(is_first_buy_yn = case_when(is.na(is_first_buy_yn) ~ 0, TRUE ~ is_first_buy_yn)) %>%
        mutate(stop_loss_base_line = case_when(stop_loss_e_base_line > stop_loss_s_base_line ~ stop_loss_e_base_line, TRUE ~ stop_loss_s_base_line), 
               support1_line = case_when(support1_e_line > support1_s_line ~ support1_e_line, TRUE ~ support1_s_line),
               support2_line = case_when(support2_e_line > support2_s_line ~ support2_e_line, TRUE ~ support2_s_line),
               support3_line = case_when(support3_e_line > support3_s_line ~ support3_e_line, TRUE ~ support3_s_line),
               profit_target1_line = case_when(profit_target1_e_line > profit_target1_s_line ~ profit_target1_e_line, TRUE ~ profit_target1_s_line),
               profit_target2_line = case_when(profit_target2_e_line > profit_target2_s_line ~ profit_target2_e_line, TRUE ~ profit_target2_s_line),
               profit_target3_line = case_when(profit_target3_e_line > profit_target3_s_line ~ profit_target3_e_line, TRUE ~ profit_target3_s_line),
               daily_support = case_when(daily_support_e > daily_support_s ~ daily_support_e, TRUE ~ daily_support_s),
               daily_ceiling = case_when(daily_ceiling_e > daily_ceiling_s ~ daily_ceiling_e, TRUE ~ daily_ceiling_s)) %>%
        mutate(                
                # update messages based on daily support, target
                message_e0 = case_when(in_the_buy_yn == 1 & close < stop_loss_base_line ~ "sell - stop-loss (msg0)",
                                       in_the_buy_yn == 1 & close < daily_support ~ "sell - break support (msg0)",
                                       in_the_buy_yn == 1 & close > support1_line ~ "sell - meet target (msg0)",
                                       TRUE ~ message_s),
                message_e1 = case_when(in_the_buy_yn == 1 & close < stop_loss_base_line ~ "sell - stop-loss (msg1)",
                                       in_the_buy_yn == 1 & close_lag1 > support1_line & (close < daily_support | close < evwma) ~ "sell - break support (msg1)",
                                       in_the_buy_yn == 1 & close > profit_target2_line ~ "sell - meet target (msg1)", 
                                       TRUE ~ message_s),
                message_e2 = case_when(in_the_buy_yn == 1 & close < stop_loss_base_line ~ "sell - stop-loss (msg2)",
                                       in_the_buy_yn == 1 & close_lag1 > support2_line & (close < daily_support | close < evwma) ~ "sell - break support (msg2)",
                                       in_the_buy_yn == 1 & close > profit_target3_line ~ "sell - meet target (msg2)", 
                                       TRUE ~ message_s)
        ) %>%
        ungroup() %>%
        select(-stop_loss_e_base_line, 
               -support1_e_line,
               -support2_e_line, 
               -support3_e_line,
               -profit_target1_e_line,
               -profit_target2_e_line,
               -profit_target3_e_line,
               -stop_loss_s_base_line,
               -support1_s_line,
               -support2_s_line,
               -support3_s_line,
               -profit_target1_s_line,
               -profit_target2_s_line,
               -profit_target3_s_line,
               -daily_support_e, 
               -daily_ceiling_e,
               -daily_support_s, 
               -daily_ceiling_s,               
        ) %>%
        arrange(symbol, date)

###############################################################################################################################
# find support (for next trading day)
supportSubset = poc0 %>%
        filter(in_the_buy_yn == 1) %>%
        filter(date >= subset_date) %>%
        select(symbol, date, 
               close, 
               daily_support,
               trailing_stop_loss,
               support1_line,
               support2_line,
               support3_line) %>%
        arrange(symbol, date)

next_day_support = vector(mode = "list", length = nrow(supportSubset))

tic()

for(i in 1:nrow(supportSubset)){
        
        compare_this_value = supportSubset$close[i]
        
        next_day_support[i] = apply(supportSubset[i, ] %>% select(daily_support,
                                                                  trailing_stop_loss,
                                                                  support1_line,
                                                                  support2_line,
                                                                  support3_line),
                                    MARGIN = 1, 
                                    FUN = value_return_by_pos, 
                                    compare_with = compare_this_value, 
                                    return = "prior")
        
}

toc()

supportSubset2 = cbind(supportSubset %>% select(symbol, date), 
                       support = unlist(next_day_support)) %>% 
        arrange(symbol, date)

###############################################################################################################################
# final poc - include "support" for next trading day
poc <- poc0 %>%
        left_join(supportSubset2, by = c("symbol", "date")) %>%        
        select(
                symbol, 
                date, 
                year, 
                is_today, 
                volume, 
                open, 
                high, 
                low, 
                close, 
                trailing_stop_loss_yesterday,
                trailing_stop_loss,
                atr, 
                zlema, 
                evwma, 
                sma5, 
                sma8,
                sma13,
                sma50, 
                sma200, 
                ema5, 
                ema20,                 
                chanExit_long, 
                chanExit_short, 
                dcc_high, 
                dcc_mid, 
                dcc_low, 
                macd_diff, 
                rsi, 
                cci, 
                obv, 
                volume_inconsistency_alert, 
                macd_trend_dir, 
                rsi_trend_dir,
                cci_trend_dir,
                rsi_oversold_yn, 
                rsi_overbought_yn, 
                cci_oversold_yn, 
                cci_overbought_yn, 
                proxy_flag, 
                near_future_flag2,
                near_future_flag4,
                near_future_flag10,
                macd_flag, 
                rsi_oversold_flag, 
                rsi_overbought_flag, 
                cci_oversold_flag, 
                cci_overbought_flag, 
                ce_long_dip_flag, 
                ce_short_spike_flag, 
                dcc_flag, 
                ha_real_flag, 
                ha_smooth_flag, 
                evwma_flag, 
                overnight_flag, 
                sma5_flag, 
                red_flag, 
                green_flag, 
                obv_flag,
                demark_flag, 
                csp_doji, 
                csp_dragonfly_doji, 
                csp_gravestone_doji, 
                csp_hammer, 
                csp_inverted_harmer, 
                csp_bullish_engulfing, 
                csp_bearish_engulfing, 
                csp_bullish_harami, 
                csp_bearish_harami, 
                csp_piercing_line, 
                csp_dark_cloud_cover, 
                csp_kick_up, 
                csp_kick_down, 
                csp_three_white_soliders, 
                csp_three_black_crows, 
                csp_morning_star, 
                csp_evening_star, 
                csp_rising_three, 
                csp_failling_three, 
                csp_up_trend, 
                csp_down_trend, 
                csp_bullish_candle, 
                csp_bearish_candle, 
                csp_candle_stick_signal, 
                csp_candle_stick_pattern, 
                csp_trend_dir, 
                message_b, 
                message_s, 
                in_the_buy_yn, 
                day_since_last_buy, 
                day_since_last_buy2, 
                is_first_buy_yn, 
                stop_loss_base_line, 
                support1_line, 
                support2_line, 
                support3_line, 
                profit_target1_line, 
                profit_target2_line, 
                profit_target3_line,                 
                today_support = daily_support,
                today_ceiling = daily_ceiling, 
                message_e0, 
                message_e1, 
                message_e2,
                support
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
