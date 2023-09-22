unique_trading_date = poc %>% select(date) %>% arrange(date) %>% distinct %>% .$date

set.seed(3210)
rand_list_target_dates <- poc$date[poc$year>=2017] %>% unique() %>% sample(100) %>% sort()
rand_list_target_dates

backtest_rand_list <- vector(mode = "list", length = length(rand_list_target_dates))

tic()

for(i in 1:length(rand_list_target_dates)){
        
        target_date = rand_list_target_dates[i]
        target_date_index = which(unique_trading_date == target_date)
        
        ###############################################################################################
        # # part I: from stock_pick2 - filter out any stock that signals down trend in the past X days
        # target_date_minus_x = unique_trading_date[target_date_index -5]
        # up = stock_pick2 %>% filter(date > target_date_minus_x & date <= target_date) %>%
        #         select(symbol, date, contains("_up_flag")) %>%
        #         tidyr::pivot_longer(vwap_up_flag:ema_up_flag) %>%
        #         group_by(symbol) %>%
        #         summarise(up_flag = sum(value))
        # 
        # down = stock_pick2 %>% filter(date > target_date_minus_x & date <= target_date) %>%
        #         select(symbol, date, contains("_down_flag")) %>%
        #         tidyr::pivot_longer(vwap_down_flag:ema_down_flag) %>%
        #         group_by(symbol) %>%
        #         summarise(down_flag = sum(value))
        # 
        # combined <- dplyr::inner_join(up, down, by = "symbol") %>%
        #         #dplyr::mutate(flag = dplyr::case_when(down_flag == 0 & up_flag >0 ~ 1, TRUE ~ 0)) %>%
        #         #dplyr::mutate(flag = dplyr::case_when(up_flag > 0 ~ 1, TRUE ~ 0)) %>%
        #         dplyr::mutate(flag = dplyr::case_when(down_flag == 0 ~ 1, TRUE ~ 0)) %>%
        #         arrange(desc(flag), desc(up_flag))
        
        # part I: from stock_pick2 - only stocks that signals up trend (from vwap_up_flag) in the past X days
        # target_date_minus_x = unique_trading_date[target_date_index -5]
        # up = stock_pick2 %>% filter(date > target_date_minus_x & date <= target_date) %>%
        #         select(symbol, date, vwap_up_flag) %>%
        #         group_by(symbol) %>%
        #         summarise(flag = sum(vwap_up_flag))
        # 
        # testS = up$symbol[up$flag > 0] %>% sort()
        # 
        #####################################
        # part II: get stocks where message == 1 and message_lag1 == 0 and message_lag2 == 0
        pick_up_signals <- poc %>%
                #dplyr::filter(symbol %in% testS) %>%
                arrange(symbol, date) %>%
                group_by(symbol) %>%
                dplyr::mutate(message_lag1 = lag(message, 1),
                              message_lag2 = lag(message, 2),
                              message_lag3 = lag(message, 3),
                              message_lag4 = lag(message, 4),
                              
                              macd_flag_lag1 = lag(macd_flag, 1),
                              macd_flag_lag2 = lag(macd_flag, 2),
                              macd_flag_lag3 = lag(macd_flag, 3),
                              macd_flag_lag4 = lag(macd_flag, 4)
                              ) %>%
                dplyr::filter(date == target_date &
                                      message == 1 &
                                      message_lag1 == 0 &
                                      message_lag2 == 0 & 
                                      message_lag3 == 0 &
                                      message_lag4 == 0 &
                                      (macd_flag == 1 | macd_flag_lag1 == 1 | macd_flag_lag2 == 1 | macd_flag_lag3 == 1 | macd_flag_lag4 == 1)) %>%
                dplyr::select(-message_lag1, -message_lag2, -message_lag3, -message_lag4) %>%
                ungroup()
        
        combined_shortList_symbols <- pick_up_signals$symbol
        
        ###################################################################
        # part III: backtest simulation - get stocks that are the most profitable from the past Y days
        if(length(combined_shortList_symbols) == 0){
                next
        } else {
                # backtest simulation for past Y days
                backtest_start_date = unique_trading_date[target_date_index -60]
                backtest_start_date = as.Date(backtest_start_date)
                backtest_end_date = as.Date(target_date) 
                backtest_list = vector(mode = "list", length = length(combined_shortList_symbols))
                
                for(k in 1:length(combined_shortList_symbols)){
                        
                        try({
                                s = combined_shortList_symbols[k]
                                
                                x <- poc %>%
                                        dplyr::filter(date >= backtest_start_date & 
                                                              date < backtest_end_date &
                                                              symbol == s) %>%
                                        dplyr::select(symbol, date, open, high, low, close, message) %>%
                                        dplyr::mutate(message = case_when(message == 1 ~ "buy",
                                                                          message == 0 ~ "hold",
                                                                          message == -1 ~ "sell"))
                                
                                y1 <- strategyEval(fund_begin = 1000, x) %>% .$net_value
                                
                                z <- data.frame(symbol = s,
                                                backtest_net_value = y1,
                                                backtest_start_date = backtest_start_date,
                                                backtest_end_date = backtest_end_date)
                                
                                backtest_list[[k]] = z
                        }, 
                        silent = TRUE
                        )
                        
                }
                
                backtest_list_df = backtest_list %>% plyr::ldply() %>%
                        arrange(desc(backtest_net_value)) %>%
                        dplyr::mutate(backtest_net_value_rank = row_number())
                
                # get the top 10 symbols that make the most profit (backtest_net_value >0) 
                refined_symbols = backtest_list_df %>%
                        dplyr::filter(backtest_net_value >0 & backtest_net_value_rank <= 10) %>%
                        select(symbol) %>%
                        .$symbol
                
                ##################################################################################
                # part IV: performance analytics & backtest simulation (again) for future price
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
                        # backtest simulation (again) for future price
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
                        ### FINAL OUTPUT: combine the "performance analytics", "backtest simulation (future)", and "backtest simulation (past)" together ###
                        evalDf <- RaRb_capm %>%
                                dplyr::inner_join(eval_list_df, by = c("symbol")) %>%
                                dplyr::inner_join(backtest_list_df %>% filter(symbol %in% refined_symbols) %>% select(symbol, backtest_net_value, backtest_net_value_rank), by = c("symbol")) %>%
                                dplyr::mutate(rand_eval_index = i)
                        
                        backtest_rand_list[[i]] <- evalDf
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


