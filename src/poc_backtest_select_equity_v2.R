unique_trading_date = poc %>% select(date) %>% arrange(date) %>% distinct %>% .$date

set.seed(8321)
rand_list_target_dates <- poc$date[poc$year>=2007] %>% unique() %>% sample(10) %>% sort()
rand_list_target_dates

backtest_rand_list <- vector(mode = "list", length = length(rand_list_target_dates))

tic()

for(i in 1:length(rand_list_target_dates)){
        
        target_date = rand_list_target_dates[i]
        target_date_index = which(unique_trading_date == target_date)
        target_lookback_date = unique_trading_date[target_date_index -20]
        target_lookback_date = as.Date(target_lookback_date)
        message_symbol = poc %>% filter(date == target_date & message == 1) %>% .$symbol
        
        if(length(message_symbol) == 0){
                next
        } else {
                ########################################################################
                # part I: get stocks where message == 1, macd_flag_lag_x == 1, is/was "oversold" in past 10 days
                pick_up_signals <- poc %>%                
                        dplyr::filter(date >= target_lookback_date & 
                                              date <= target_date &
                                              symbol %in% message_symbol) %>%
                        arrange(symbol, date) %>%
                        group_by(symbol) %>%
                        # get "oversold"
                        dplyr::mutate(rsi_lag1 = lag(rsi, 1),
                                      rsi_lag2 = lag(rsi, 2),
                                      rsi_oversold_yn = case_when(rsi > 35 & rsi_lag1 <= 35 & rsi_lag2 <= 35 ~ 1,
                                                                  rsi > 35 & (rsi > rsi_lag1) & rsi_lag1 > 35 & rsi_lag2 <= 35 ~ 1,
                                                                  TRUE ~ 0),
                                      rsi_oversold_flag = pracma::movavg(rsi_oversold_yn, n = 10, type = "s"),
                                      rsi_oversold_flag = case_when(rsi_oversold_flag >0 ~ 1, TRUE ~ 0),
                                      cci_lag1 = lag(cci, 1),
                                      cci_lag2 = lag(cci, 2),
                                      cci_oversold_yn = case_when(cci > -100 & cci_lag1 <= -100 & cci_lag2 <= -100 ~ 1,
                                                                  cci > -100 & (cci > cci_lag1) & cci_lag1 > -100 & cci_lag2 <= -100 ~ 1,
                                                                  TRUE ~ 0),
                                      cci_oversold_flag = pracma::movavg(cci_oversold_yn, n = 10, type = "s"),
                                      cci_oversold_flag = case_when(cci_oversold_flag >0 ~ 1, TRUE ~ 0)) %>%
                        dplyr::mutate(message_lag1 = lag(message, 1),
                                      message_lag2 = lag(message, 2),
                                      message_lag3 = lag(message, 3),
                                      message_lag4 = lag(message, 4),
                                      
                                      macd_flag_lag1 = lag(macd_flag, 1),
                                      macd_flag_lag2 = lag(macd_flag, 2),
                                      macd_flag_lag3 = lag(macd_flag, 3),
                                      macd_flag_lag4 = lag(macd_flag, 4)
                        ) %>%
                        ungroup() %>%
                        # start filtering here
                        dplyr::filter(date == target_date &
                                              message == 1 &
                                              message_lag1 == 0 &
                                              message_lag2 == 0 & 
                                              message_lag3 == 0 &
                                              message_lag4 == 0 &
                                              (macd_flag == 1 | macd_flag_lag1 == 1 | macd_flag_lag2 == 1 | macd_flag_lag3 == 1 | macd_flag_lag4 == 1) &
                                              rsi < 60 &
                                              cci < 90 &
                                              (rsi_oversold_flag == 1 | cci_oversold_flag == 1))
                
                combined_shortList_symbols <- pick_up_signals$symbol
                
                if(length(combined_shortList_symbols) == 0){
                        next 
                } else {
                        
                        ###################################################################
                        # part II: upside opportunity - measure the gap between 20% up from close and 2 * ATR, i.e., smaller the better
                        price_est <- poc %>%
                                dplyr::filter(symbol %in% combined_shortList_symbols & date == target_date) %>%
                                dplyr::select(symbol, close, atr) %>%
                                dplyr::mutate(atr2 = close + atr * 2,
                                              close2 = close * 1.2,
                                              diff = (close2 - atr2) / close) %>%
                                arrange(diff) %>%
                                dplyr::mutate(rank_by_upside_opp = row_number())
                        
                        # get the top X symbols ranked by potential upside opportunity
                        refined_symbols = price_est %>%
                                #dplyr::filter(rank_by_upside_opp <= 10) %>%
                                select(symbol) %>%
                                .$symbol
                        
                        ##################################################################################
                        # part III: performance analytics & backtest simulation
                        if(length(refined_symbols) == 0){
                                next
                        } else {
                                #################################################
                                # performance analytics
                                Ra <- poc %>%
                                        dplyr::filter(symbol %in% refined_symbols) %>%
                                        dplyr::filter(date < target_date) %>%
                                        arrange(symbol, date) %>%
                                        group_by(symbol) %>%
                                        tq_transmute(select = close,
                                                     mutate_fun = periodReturn,
                                                     period = "monthly",
                                                     col_rename = "Ra") %>%
                                        arrange(desc(date)) %>%
                                        dplyr::mutate(id = row_number()) %>%
                                        dplyr::filter(id <= 60) %>%
                                        dplyr::select(-id) %>%
                                        ungroup %>%
                                        arrange(symbol, date)
                                
                                # at least 6 months of data for calculating return
                                RaFilter <- Ra %>%
                                        group_by(symbol) %>%
                                        summarise(n = n()) %>%
                                        dplyr::filter(n>5) %>%
                                        .$symbol
                                
                                Ra <- Ra %>%
                                        dplyr::filter(symbol %in% RaFilter)
                                
                                Rb <- poc %>%
                                        dplyr::filter(symbol == "SPY") %>%
                                        dplyr::filter(date < target_date) %>%
                                        arrange(date) %>%
                                        tq_transmute(select = close,
                                                     mutate_fun = periodReturn,
                                                     period = "monthly",
                                                     col_rename = "Rb") %>%
                                        arrange(desc(date)) %>%
                                        dplyr::mutate(id = row_number()) %>%
                                        dplyr::filter(id <= 60) %>%
                                        dplyr::select(-id) %>%
                                        arrange(date)
                                
                                RaRb <- left_join(Ra, Rb, by = "date")
                                
                                RaRb_capm <- RaRb %>%
                                        group_by(symbol) %>%
                                        tq_performance(Ra = Ra,
                                                       Rb = Rb,
                                                       performance_fun = table.CAPM)
                                
                                ######################################################
                                # backtest simulation, i.e., follow "message" and see how that goes in next XX days
                                eval_list = vector(mode = "list", length = length(refined_symbols))
                                
                                eval_start_date = as.Date(target_date)
                                eval_end_date = unique_trading_date[target_date_index +20]
                                eval_end_date = as.Date(eval_end_date)
                                
                                for(j in 1:length(refined_symbols)){
                                        
                                        try({
                                                s = refined_symbols[j]
                                                
                                                x <- poc %>%
                                                        dplyr::filter(date >= eval_start_date & 
                                                                              date <= eval_end_date &
                                                                              symbol == s) %>%
                                                        dplyr::select(symbol, date, open, high, low, close, message) %>%
                                                        dplyr::mutate(message = case_when(message == 1 ~ "buy",
                                                                                          message == 0 ~ "hold",
                                                                                          message == -1 ~ "sell"))
                                                
                                                y1 <- strategyEval(fund_begin = 1000, x) %>% .$net_value
                                                
                                                z <- data.frame(symbol = s,
                                                                net_value = y1,
                                                                eval_start_date = eval_start_date,
                                                                eval_end_date = eval_end_date)
                                                
                                                eval_list[[j]] = z        
                                        }, 
                                        silent = TRUE
                                        )
                                        
                                }
                                
                                eval_list_df = eval_list %>% plyr::ldply()
                                
                                #######################################################################
                                ### FINAL OUTPUT: combine the "performance analytics", "price estimation", and "backtest simulation" together ###
                                evalDf <- RaRb_capm %>%
                                        dplyr::inner_join(eval_list_df, by = c("symbol")) %>%
                                        dplyr::inner_join(price_est %>% filter(symbol %in% refined_symbols) %>% select(symbol, close2, atr2, diff, rank_by_upside_opp), by = c("symbol")) %>%
                                        dplyr::mutate(rand_eval_index = i)
                                
                                backtest_rand_list[[i]] <- evalDf
                        }
                        
                }
        
        }
        
}

toc()

####################################################################
#backtest_rand_list

backtest_rand_evalDf <- backtest_rand_list %>% 
        plyr::ldply() %>%
        arrange(rand_eval_index, desc(net_value))

dim(backtest_rand_evalDf)
backtest_rand_evalDf %>% write_clip() 


