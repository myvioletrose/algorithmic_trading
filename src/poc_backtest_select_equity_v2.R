tic()
seed = 2485
n = 120
fund_begin = 1000
days_after_signal = 2
#symbol_to_study = c("GOOGL")

unique_trading_date = poc %>% select(date) %>% arrange(date) %>% distinct %>% .$date
desirable_situations = c(
        "EBear-EBear::SBull-SBear",
        "EBull-EBear::SBear-SBull",
        "EBull-EBear::SBull-SBear", 
        "EBull-EBull::SBull-SBear"
        #"EBull-EBull::SBull-SBull"
)

set.seed(seed)
rand_list_target_dates <- poc$date[poc$year>=2007 & 
                                           #poc$situation %in% desirable_situations &
                                           poc$message == 1 &
                                           #poc$symbol %in% symbol_to_study &
                                           poc$date < '2023-06-01'] %>% 
        unique() %>% 
        sample(n) %>% 
        sort()

rand_list_target_dates

backtest_rand_list <- vector(mode = "list", length = length(rand_list_target_dates))

tic()

for(i in 1:length(rand_list_target_dates)){
        
        target_date = rand_list_target_dates[i]
        target_date_index = which(unique_trading_date == target_date)
        target_lookback_date = unique_trading_date[target_date_index -20]
        target_lookback_date = as.Date(target_lookback_date)
        
        combined_shortList_symbols = poc %>% filter(date == target_date & 
                                                            #situation %in% desirable_situations &
                                                            message == 1) %>% .$symbol
        #combined_shortList_symbols = base::intersect(combined_shortList_symbols, symbol_to_study)
        
        # upside opportunity - measure the gap between 20% up from close and 2 * ATR, i.e., smaller the better
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
        
        ##################################################################################################
        # # performance analytics
        # Ra <- poc %>%
        #         dplyr::filter(symbol %in% refined_symbols) %>%
        #         dplyr::filter(date < target_date) %>%
        #         arrange(symbol, date) %>%
        #         group_by(symbol) %>%
        #         tq_transmute(select = close,
        #                      mutate_fun = periodReturn,
        #                      period = "monthly",
        #                      col_rename = "Ra") %>%
        #         arrange(desc(date)) %>%
        #         dplyr::mutate(id = row_number()) %>%
        #         dplyr::filter(id <= 60) %>%
        #         dplyr::select(-id) %>%
        #         ungroup %>%
        #         arrange(symbol, date)
        # 
        # # at least 6 months of data for calculating return
        # RaFilter <- Ra %>%
        #         group_by(symbol) %>%
        #         summarise(n = n()) %>%
        #         dplyr::filter(n>5) %>%
        #         .$symbol
        # 
        # Ra <- Ra %>%
        #         dplyr::filter(symbol %in% RaFilter)
        # 
        # Rb <- poc %>%
        #         dplyr::filter(symbol == "SPY") %>%
        #         dplyr::filter(date < target_date) %>%
        #         arrange(date) %>%
        #         tq_transmute(select = close,
        #                      mutate_fun = periodReturn,
        #                      period = "monthly",
        #                      col_rename = "Rb") %>%
        #         arrange(desc(date)) %>%
        #         dplyr::mutate(id = row_number()) %>%
        #         dplyr::filter(id <= 60) %>%
        #         dplyr::select(-id) %>%
        #         arrange(date)
        # 
        # RaRb <- left_join(Ra, Rb, by = "date")
        # 
        # RaRb_capm <- RaRb %>%
        #         group_by(symbol) %>%
        #         tq_performance(Ra = Ra,
        #                        Rb = Rb,
        #                        performance_fun = table.CAPM)
        
        ######################################################
        # backtest simulation, i.e., follow "message" and see how that goes in next XX days
        eval_list = vector(mode = "list", length = length(refined_symbols))
        
        eval_start_date = as.Date(target_date)
        
        for(j in 1:length(refined_symbols)){
                
                try({
                        s = refined_symbols[j]
                        
                        eval_end_date_step1 = poc %>%
                                filter(symbol == s & date >= eval_start_date) %>%
                                select(date, symbol, message)
                        
                        eval_end_date_step2 = eval_end_date_step1 %>%
                                filter(message == -1) %>%
                                group_by(symbol) %>%
                                summarise(sell_on_date = min(date)) %>%
                                ungroup %>%
                                .$sell_on_date
                        
                        if(length(eval_end_date_step2) == 0){eval_end_date_step2 = unique_trading_date %>% max() -1}
                        eval_end_date_index = which(unique_trading_date == eval_end_date_step2)+1
                        eval_end_date = unique_trading_date[eval_end_date_index]
                        
                        x <- poc %>%
                                dplyr::filter(date >= eval_start_date & 
                                                      date <= eval_end_date &
                                                      symbol == s) %>%
                                dplyr::select(symbol, date, open, high, low, close, message) %>%
                                dplyr::mutate(message = case_when(message == 1 ~ "buy",                                                                  
                                                                  message == -1 ~ "sell",
                                                                  TRUE ~ "hold"))
                        
                        # strategyEval
                        y1 <- strategyEval(fund_begin = fund_begin, x) %>% .$net_value
                        
                        z <- data.frame(symbol = s,
                                        net_value = y1,
                                        eval_start_date = eval_start_date,
                                        eval_end_date = eval_end_date) %>%
                                dplyr::mutate(days_between = eval_end_date - eval_start_date)
                        
                        ###########################################################################################
                        # what happened in the next X day (after message == 1)?
                        eval_start_date_plusX_index = which(unique_trading_date == eval_start_date)+days_after_signal
                        eval_end_date_plusX = unique_trading_date[eval_start_date_plusX_index]
                        
                        daysPlus_x <- poc %>%
                                dplyr::filter(date %in% c(eval_start_date, eval_end_date_plusX) &
                                                      symbol == s) %>%
                                dplyr::select(symbol, date, close, vwap) %>%
                                dplyr::mutate(seq = case_when(date == eval_start_date ~ "begin", TRUE ~ "end"),
                                              vwap_threshold = case_when(close > vwap ~ 1, TRUE ~ 0)) 
                        
                        daysPlus_y <- daysPlus_x %>%
                                select(symbol, seq, close, vwap) %>%
                                tidyr::gather(key, value, close:vwap) %>%
                                tidyr::spread(seq, value) %>%
                                dplyr::mutate(percent_change = (end - begin) / begin)
                        
                        daysPlus_z <- daysPlus_y %>%
                                select(symbol, key, percent_change) %>%
                                tidyr::spread(key, percent_change) %>%
                                dplyr::inner_join(daysPlus_x %>%
                                                          filter(seq == "end") %>%
                                                          select(symbol, vwap_threshold),
                                                  by = "symbol") %>%
                                dplyr::mutate(eval_start_date = eval_start_date,
                                              plusX_date = eval_end_date_plusX) %>%
                                select(symbol,
                                       eval_start_date,
                                       plusX_date,
                                       chg_in_close = close,
                                       chg_in_vwap = vwap,
                                       vwap_threshold)
                        
                        combinedZ <- z %>% 
                                dplyr::inner_join(daysPlus_z, by = c("symbol", "eval_start_date"))
                        
                        eval_list[[j]] = combinedZ
                }, 
                silent = TRUE
                )
                
        }
        
        eval_list_df = eval_list %>% plyr::ldply()
        
        #######################################################################
        ### FINAL OUTPUT: combine the "performance analytics", "price estimation", and "backtest simulation" together ###
        # evalDf <- RaRb_capm %>%
        #         dplyr::inner_join(eval_list_df, by = c("symbol")) %>%
        #         dplyr::inner_join(price_est %>% filter(symbol %in% refined_symbols) %>% select(symbol, close2, atr2, diff, rank_by_upside_opp), by = c("symbol")) %>%
        #         dplyr::mutate(rand_eval_index = i)
        
        evalDf <- eval_list_df %>% 
                dplyr::inner_join(price_est %>% filter(symbol %in% refined_symbols) %>% select(symbol, close2, atr2, diff, rank_by_upside_opp), by = c("symbol")) %>%
                dplyr::mutate(rand_eval_index = i)
        
        backtest_rand_list[[i]] <- evalDf
}

toc()

####################################################################
############# <<< result >>> #################################
#backtest_rand_list

backtest_rand_evalDf <- backtest_rand_list %>% 
        plyr::ldply() %>%
        dplyr::mutate(is_win_yn = case_when(net_value >0 ~ 1, TRUE ~ 0),
                      percent_change = net_value / fund_begin) %>%
        arrange(rand_eval_index, desc(net_value)) %>%
        janitor::clean_names() %>%
        select(rand_eval_index, 
               symbol, 
               # active_premium, 
               # alpha, 
               # annualized_alpha, 
               # beta, 
               # beta_2, 
               # beta_3, 
               # correlation, 
               # correlationp_value, 
               # information_ratio, 
               # r_squared, 
               # tracking_error, 
               # treynor_ratio, 
               close2, 
               atr2, 
               diff, 
               rank_by_upside_opp, 
               net_value, 
               is_win_yn, 
               percent_change, 
               eval_start_date, 
               eval_end_date, 
               days_between,
               plus_x_date,
               chg_in_close,
               chg_in_vwap,
               vwap_threshold)

dim(backtest_rand_evalDf)

w = backtest_rand_evalDf %>%
        group_by(rand_eval_index) %>%
        summarise(n = n(),
                  win = sum(is_win_yn)) %>%
        dplyr::mutate(win_loss_rate = win / n) %>%
        ungroup %>%
        .$win_loss_rate 

#################### <<< print overall result >>> ###########################
backtest_rand_evalDf$net_value %>% sum()
backtest_rand_evalDf$net_value %>% sum() / (nrow(backtest_rand_evalDf) * fund_begin)

mu_percent_change = mean(backtest_rand_evalDf$percent_change)
sd_percent_change = sd(backtest_rand_evalDf$percent_change)
margin_of_error = 1.96 * sd_percent_change / sqrt(nrow(backtest_rand_evalDf)-1)
ci_percent_change_up = mu_percent_change + margin_of_error
ci_percent_change_down = mu_percent_change - margin_of_error

print(paste0("average percent change in investment is ", round(mu_percent_change,3)))
print(paste0("confidence interval is between ", round(ci_percent_change_down,3), " and ", round(ci_percent_change_up,3)))

print(paste0("win/loss rate (session level) is ", round(mean(w),2)))
print(paste0("win/loss rate (overall) is ", round(sum(backtest_rand_evalDf$is_win_yn) / nrow(backtest_rand_evalDf), 2)))

print(paste0("average number of day for 'hold' is ", round(mean(backtest_rand_evalDf$days_between))))

toc()

#backtest_rand_evalDf %>% write_clip() 

















###############################################################################################
########### <<< detail report >>>
backtest_rand_evalDf2 <- backtest_rand_evalDf %>%
        dplyr::inner_join(poc %>% 
                                  select(symbol, eval_start_date = date, 
                                         year, volume, situation),
                          by = c("symbol", "eval_start_date")) 

######### <<< year >>> ##################
yearly <- backtest_rand_evalDf2 %>%
        group_by(year) %>%
        summarise(n = n(),
                  win_loss_rate = round(sum(is_win_yn) / n, 3),
                  avg_days_hold = round(mean(days_between)),
                  mu_percent_change = sum(net_value) / (n * fund_begin)) %>%
        ungroup() %>%
        inner_join(backtest_rand_evalDf2 %>%
                           dplyr::mutate(percent_change = net_value / fund_begin) %>%
                           group_by(year) %>%
                           summarise(sd_percent_change = sd(percent_change)) %>%
                           ungroup,
                   by = "year") %>%
        filter(!is.na(sd_percent_change)) %>%
        dplyr::mutate(margin_of_error = 1.96 * sd_percent_change / sqrt(n-1),
                      ci_percent_change_down = mu_percent_change - margin_of_error,
                      ci_percent_change_up = mu_percent_change + margin_of_error) %>%
        arrange(year)

######### <<< situation >>> ##################
situation <- backtest_rand_evalDf2 %>%
        group_by(situation) %>%
        summarise(n = n(),
                  win_loss_rate = round(sum(is_win_yn) / n, 3),
                  avg_days_hold = round(mean(days_between)),
                  mu_percent_change = sum(net_value) / (n * fund_begin)) %>%
        ungroup() %>%
        inner_join(backtest_rand_evalDf2 %>%
                           dplyr::mutate(percent_change = net_value / fund_begin) %>%
                           group_by(situation) %>%
                           summarise(sd_percent_change = sd(percent_change)) %>%
                           ungroup,
                   by = "situation") %>%
        filter(!is.na(sd_percent_change)) %>%
        dplyr::mutate(margin_of_error = 1.96 * sd_percent_change / sqrt(n-1),
                      ci_percent_change_down = mu_percent_change - margin_of_error,
                      ci_percent_change_up = mu_percent_change + margin_of_error) %>%
        arrange(situation)

##########
detail_report <- rbind(
        yearly %>%
                dplyr::mutate(type = "year") %>%
                select(type, value = year, everything()),
        situation %>%
                dplyr::mutate(type = "situation") %>%
                select(type, value = situation, everything())
)

#backtest_rand_evalDf2 %>% write_clip()

#detail_report %>% write_clip()









