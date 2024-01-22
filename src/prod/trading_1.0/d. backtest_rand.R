#seed = 99
#n = 100
#days_after_signal = 10
#symbol_to_study = poc$symbol %>% unique()
#rand_date_start = '1990-01-01'
#rand_date_end = '2023-07-01'
#first_buy_only = FALSE

# msg_string_update function
msg_string_update <- function(x) {
        ifelse(grepl("buy", x, ignore.case = TRUE), 
               "buy",
               ifelse(grepl("sell", x, ignore.case = TRUE), 
                      "sell",
                      x))
}

# choose message_e*
message_list <- names(poc) %>% grep(pattern = "message_e([[:digit:]]$)", ignore.case = TRUE, value = TRUE)
fund_begin = rep(1000, length(message_list))

fund_df = data.frame(message_type = message_list, fund_begin)

# desirable_situations = c(
#         "EBear-EBear::SBull-SBear",
#         "EBull-EBear::SBear-SBull",
#         "EBull-EBear::SBull-SBear", 
#         "EBull-EBull::SBull-SBear"
#         #"EBull-EBull::SBull-SBull"
# )

unique_trading_date = poc %>% filter(symbol == "SPY") %>% select(date) %>% arrange(date) %>% distinct %>% .$date

set.seed(seed)

date_subset_df <- poc %>%        
        dplyr::mutate_at("message_s", msg_string_update) %>%        
        #filter(date %in% date_test_set) %>%
        filter(message_s == "buy" &
                       symbol %in% symbol_to_study &
                       #situation %in% desirable_situations &
                       date >= as.Date(rand_date_start, '%Y-%m-%d') &
                       date < as.Date(rand_date_end, '%Y-%m-%d')) %>% 
        select(symbol, date)

if(first_buy_only){date_subset_df = date_subset_df %>% inner_join(first_buy, by = c("symbol" = "symbol", "date" = "first_buy_date"))}

date_subset = date_subset_df$date %>% unique() %>% sort()
        
if(length(date_subset) < n){n = length(date_subset)}

rand_list_target_dates <- sample(date_subset, n) %>% sort()
print(rand_list_target_dates)

backtest_rand_list <- vector(mode = "list", length = length(rand_list_target_dates))

tic()

for(i in 1:length(rand_list_target_dates)){
        
        target_date = rand_list_target_dates[i]
        target_date_index = which(unique_trading_date == target_date)
        target_lookback_date = unique_trading_date[target_date_index -20]
        target_lookback_date = as.Date(target_lookback_date)
        
        if(first_buy_only){
                combined_shortList_symbols = poc %>% 
                        inner_join(first_buy, by = c("symbol" = "symbol", "date" = "first_buy_date")) %>%
                        dplyr::mutate_at("message_s", msg_string_update) %>%
                        dplyr::mutate(message_s = case_when(message_s == "buy" ~ 1,
                                                            #message_s == "sell" ~ -1,
                                                            TRUE ~ 0)) %>%
                        filter(date == target_date & 
                                       symbol %in% symbol_to_study &
                                       #situation %in% desirable_situations &
                                       message_s == 1) %>% 
                        .$symbol 
        } else {
                combined_shortList_symbols = poc %>% 
                        dplyr::mutate_at("message_s", msg_string_update) %>%
                        dplyr::mutate(message_s = case_when(message_s == "buy" ~ 1,
                                                            #message_s == "sell" ~ -1,
                                                            TRUE ~ 0)) %>%
                        filter(date == target_date & 
                                       symbol %in% symbol_to_study &
                                       #situation %in% desirable_situations &
                                       message_s == 1) %>% 
                        .$symbol 
        }               
        
        # upside opportunity - measure the percent of upside opp based on ATR * 3, i.e., larger percentage the better
        price_est <- poc %>%
                dplyr::filter(symbol %in% combined_shortList_symbols & date == target_date) %>%
                dplyr::select(symbol, close, atr) %>%
                dplyr::mutate(support3_line = close + (atr * 3),
                              diff = (support3_line - close) / close) %>%
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
                
                s = refined_symbols[j]
                
                message_list_result = vector(mode = "list", length = length(message_list))
                
                for(k in 1:length(message_list)){
                        
                        col = message_list[k]
                        
                        try({
                                stop_loss_base = daily_price_target %>% filter(symbol == s & date == eval_start_date) %>% .$stop_loss_base_line
                                support1 = daily_price_target %>% filter(symbol == s & date == eval_start_date) %>% .$support1_line
                                support2 = daily_price_target %>% filter(symbol == s & date == eval_start_date) %>% .$support2_line
                                support3 = daily_price_target %>% filter(symbol == s & date == eval_start_date) %>% .$support3_line                                
                                
                                eval_end_date_step1 = let(c(COL = col),
                                                          poc %>%
                                                                  filter(symbol == s & date >= eval_start_date) %>%
                                                                  dplyr::mutate(message = COL,
                                                                                stop_loss_base_line = stop_loss_base,
                                                                                support1_line = support1,
                                                                                support2_line = support2,
                                                                                support3_line = support3) %>%
                                                                  # dplyr::mutate(message = case_when(col == "message_e1" & close > support1_line ~ "sell - target1",
                                                                  #                                   col == "message_e1" & close < stop_loss_base_line ~ "sell - stop-loss (msg1)",
                                                                  #                                   col == "message_e2" & close > support2_line ~ "sell - target2",
                                                                  #                                   col == "message_e2" & close_lag1 >= support1_line & close < support1_line ~ "sell - stop-loss (msg2)",
                                                                  #                                   col == "message_e2" & close < stop_loss_base_line ~ "sell - stop-loss (msg2)",
                                                                  #                                   col == "message_e3" & close > support3_line ~ "sell - target3", 
                                                                  #                                   col == "message_e3" & close_lag1 >= support2_line & close < support2_line ~ "sell - stop-loss (msg3)",
                                                                  #                                   col == "message_e3" & close < stop_loss_base_line ~ "sell - stop-loss (msg3)",
                                                                  #                                   col == "message_e4" ~ message_e4,
                                                                  #                                   col == "message_e5" ~ message_e5,
                                                                  #                                   col == "message_e6" ~ message_e6,
                                                          #                                   TRUE ~ message_s)) %>%
                                                          dplyr::mutate_at("message", msg_string_update) %>%
                                                                  dplyr::mutate(message = case_when(message == "buy" ~ 1,
                                                                                                    message == "sell" ~ -1,
                                                                                                    TRUE ~ 0)) %>%
                                                                  select(date, symbol, message, 
                                                                         stop_loss_base_line,
                                                                         support1_line,
                                                                         support2_line,
                                                                         support3_line))
                                
                                eval_end_date_step2 = eval_end_date_step1 %>%
                                        filter(message == -1) %>%
                                        group_by() %>%
                                        summarise(sell_on_date = min(date)) %>%
                                        ungroup %>%
                                        .$sell_on_date
                                
                                if(length(eval_end_date_step2) == 0){eval_end_date_step2 = unique_trading_date %>% max() -1}
                                eval_end_date_index = which(unique_trading_date == eval_end_date_step2)+1
                                eval_end_date = unique_trading_date[eval_end_date_index]
                                
                                x <- let(c(COL = col),
                                         poc %>%
                                                 dplyr::filter(date >= eval_start_date & 
                                                                       date <= eval_end_date &
                                                                       symbol == s) %>%
                                                 dplyr::mutate(message = COL,
                                                               stop_loss_base_line = stop_loss_base,
                                                               support1_line = support1,
                                                               support2_line = support2,
                                                               support3_line = support3) %>%
                                                 # dplyr::mutate(message = case_when(col == "message_e1" & close > support1_line ~ "sell - target1",
                                                 #                                   col == "message_e1" & close < stop_loss_base_line ~ "sell - stop-loss (msg1)",
                                                 #                                   col == "message_e2" & close > support2_line ~ "sell - target2",
                                                 #                                   col == "message_e2" & close_lag1 >= support1_line & close < support1_line ~ "sell - stop-loss (msg2)",
                                                 #                                   col == "message_e2" & close < stop_loss_base_line ~ "sell - stop-loss (msg2)",
                                                 #                                   col == "message_e3" & close > support3_line ~ "sell - target3", 
                                                 #                                   col == "message_e3" & close_lag1 >= support2_line & close < support2_line ~ "sell - stop-loss (msg3)",
                                                 #                                   col == "message_e3" & close < stop_loss_base_line ~ "sell - stop-loss (msg3)",
                                                 #                                   col == "message_e4" ~ message_e4,
                                                 #                                   col == "message_e5" ~ message_e5,
                                                 #                                   col == "message_e6" ~ message_e6,
                                         #                                   TRUE ~ message_s)) %>%
                                         dplyr::mutate_at("message", msg_string_update) %>%
                                                 dplyr::select(symbol, date, open, high, low, close, message))
                                
                                # strategyEval
                                fund = fund_begin[k]
                                y1 <- strategyEval(fund_begin = fund, x) %>% .$net_value
                                
                                z <- data.frame(symbol = s,
                                                net_value = y1,
                                                eval_start_date = eval_start_date,
                                                eval_end_date = eval_end_date) %>%
                                        dplyr::mutate(days_between = eval_end_date - eval_start_date,
                                                      message_type = message_list[k])
                                
                                message_list_result[[k]] = z
                                
                        }, 
                        silent = TRUE
                        )
                }
                
                # combine messages result together 
                message_result_combined <- do.call(rbind, message_list_result)
                
                ###########################################################################################
                # what happened in the next X day (after message == 1)?
                eval_start_date_plusX_index = which(unique_trading_date == eval_start_date)+days_after_signal
                eval_end_date_plusX = unique_trading_date[eval_start_date_plusX_index]
                
                daysPlus_x <- poc %>%
                        dplyr::filter(date %in% c(eval_start_date, eval_end_date_plusX) &
                                              symbol == s) %>%
                        dplyr::select(symbol, date, close) %>%
                        dplyr::mutate(seq = case_when(date == eval_start_date ~ "begin", TRUE ~ "end")) 
                
                daysPlus_y <- daysPlus_x %>%
                        select(symbol, seq, close) %>%
                        tidyr::gather(key, value, close) %>%
                        tidyr::spread(seq, value) %>%
                        dplyr::mutate(percent_change = (end - begin) / begin)
                
                daysPlus_z <- daysPlus_y %>%
                        select(symbol, key, percent_change) %>%
                        tidyr::spread(key, percent_change) %>%
                        dplyr::mutate(eval_start_date = eval_start_date,
                                      plusX_date = eval_end_date_plusX) %>%
                        select(symbol,
                               eval_start_date,
                               plusX_date,
                               chg_in_close = close)
                
                combined_df <- daysPlus_z %>% 
                        dplyr::inner_join(message_result_combined, by = c("symbol", "eval_start_date"), multiple = "all")
                
                eval_list[[j]] <- combined_df
                
        }
        
        eval_list_df = eval_list %>% plyr::ldply()
        
        #######################################################################
        ### FINAL OUTPUT: combine the "performance analytics", "price estimation", and "backtest simulation" together ###
        # evalDf <- RaRb_capm %>%
        #         dplyr::inner_join(eval_list_df, by = c("symbol")) %>%
        #         dplyr::inner_join(price_est %>% filter(symbol %in% refined_symbols) %>% select(symbol, close2, atr2, diff, rank_by_upside_opp), by = c("symbol")) %>%
        #         dplyr::mutate(rand_eval_index = i)
        
        evalDf <- eval_list_df %>% 
                dplyr::inner_join(price_est %>% filter(symbol %in% refined_symbols) %>% select(symbol, atr, support3_line, diff, rank_by_upside_opp), by = c("symbol")) %>%
                dplyr::mutate(rand_eval_index = i)
        
        backtest_rand_list[[i]] <- evalDf
}

toc()

####################################################################
############# <<< result >>> #################################
#backtest_rand_list

backtest_rand_evalDf <- backtest_rand_list %>% 
        plyr::ldply() %>%
        dplyr::inner_join(fund_df, by = "message_type") %>%
        dplyr::mutate(is_win_yn = case_when(net_value >0 ~ 1, TRUE ~ 0),
                      percent_chg = net_value / fund_begin) %>%
        arrange(rand_eval_index, desc(net_value)) %>%
        janitor::clean_names() %>%
        inner_join(poc %>% select(symbol, date, close_start_date = close), by = c("symbol" = "symbol", "eval_start_date" = "date")) %>%
        inner_join(poc %>% select(symbol, date, close_end_date = close), by = c("symbol" = "symbol", "eval_end_date" = "date")) %>%
        mutate(month_key = format(as.Date(eval_start_date), "%Y-%m") %>%
                       stringr::str_remove(pattern = "-") %>%
                       as.integer()) %>%
        left_join(first_buy, by = c("symbol" = "symbol", "eval_start_date" = "first_buy_date")) %>%
        mutate(is_first_buy_yn = case_when(is.na(is_first_buy_yn) ~ 0, TRUE ~ is_first_buy_yn)) %>%
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
               close_start_date,
               close_end_date,
               atr,
               support3_line, 
               diff, 
               rank_by_upside_opp,
               month_key,
               eval_start_date, 
               eval_end_date,
               plus_x_date,
               chg_in_close,
               message_type,
               fund_begin,
               days_between,
               net_value, 
               is_win_yn, 
               percent_chg,
               is_first_buy_yn)

# high level summary across messages
msg_summary <- backtest_rand_evalDf %>%
        group_by(rand_eval_index, eval_start_date, symbol) %>%
        summarise(total_net_value = sum(net_value),
                  avg_days_of_hold = mean(days_between),
                  min_days_of_hold = min(days_between),
                  max_days_of_hold = max(days_between),
                  percent_chg = sum(net_value) / sum(fund_begin)) %>%
        arrange(desc(percent_chg))

############################################################################################################        
#################### <<< print result in detail >>> ###########################
### dim
# > overall level
# > session level
# > message level

### KPI
# > avg_days_of_hold
# > win rate
# > win/loss ratio
# > percent_chg (mean, CI)

# overall level
overall_compare <- msg_summary %>%
        dplyr::mutate(win_rate_flag = case_when(total_net_value >0 ~ 1, TRUE ~ 0),
                      win = case_when(total_net_value >0 ~ total_net_value, TRUE ~ 0),
                      loss = case_when(total_net_value <=0 ~ abs(total_net_value), TRUE ~ 0)) %>%
        group_by() %>%
        summarise(n = n(),
                  win_rate  = sum(win_rate_flag) / nrow(msg_summary),
                  avg_days_of_hold = round(mean(avg_days_of_hold)),
                  win = sum(win),
                  loss = sum(loss),
                  avg_percent_chg = round(mean(percent_chg), 3),
                  sd_percent_chg = sd(percent_chg)) %>%
        ungroup() %>%
        group_by(n, win_rate, avg_days_of_hold, win, loss, avg_percent_chg, sd_percent_chg) %>%
        dplyr::mutate(loss = case_when(loss == 0 ~ 1, TRUE ~ loss)) %>%
        summarise(win_loss_ratio = sum(win) / sum(loss)) %>%
        ungroup() %>%
        dplyr::mutate(margin_of_error = round(1.96 * sd_percent_chg / sqrt(n-1), 3),
                      CI.up = avg_percent_chg + margin_of_error,
                      CI.down = avg_percent_chg - margin_of_error) %>%
        select(n, avg_days_of_hold, win_rate, win, loss, win_loss_ratio, avg_percent_chg, CI.down, CI.up)

# session level
session_compare <- msg_summary %>%
        dplyr::mutate(win_rate_flag = case_when(total_net_value >0 ~ 1, TRUE ~ 0),
                      win = case_when(total_net_value >0 ~ total_net_value, TRUE ~ 0),
                      loss = case_when(total_net_value <=0 ~ abs(total_net_value), TRUE ~ 0)) %>%
        group_by(rand_eval_index, eval_start_date) %>%
        summarise(n = n(),
                  win_rate = sum(win_rate_flag) / n,
                  avg_days_of_hold = round(mean(avg_days_of_hold)),
                  win = sum(win),
                  loss = sum(loss),
                  avg_percent_chg = round(mean(percent_chg), 3),
                  sd_percent_chg = sd(percent_chg)) %>%
        ungroup() %>%
        group_by(rand_eval_index, eval_start_date, n, win_rate, avg_days_of_hold, win, loss, avg_percent_chg, sd_percent_chg) %>%
        dplyr::mutate(loss = case_when(loss == 0 ~ 1, TRUE ~ loss)) %>%
        summarise(win_loss_ratio = sum(win) / sum(loss)) %>%
        ungroup() %>%
        dplyr::mutate(margin_of_error = round(1.96 * sd_percent_chg / sqrt(n-1), 3),
                      CI.up = avg_percent_chg + margin_of_error,
                      CI.down = avg_percent_chg - margin_of_error) %>%
        select(rand_eval_index, eval_start_date, n, avg_days_of_hold, win_rate, win, loss, win_loss_ratio, avg_percent_chg, CI.down, CI.up) %>%
        arrange(rand_eval_index)

# message level
msg_compare <- backtest_rand_evalDf %>%
        dplyr::mutate(win_rate_flag = case_when(net_value >0 ~ 1, TRUE ~ 0),
                      win = case_when(net_value >0 ~ net_value, TRUE ~ 0),
                      loss = case_when(net_value <=0 ~ abs(net_value), TRUE ~ 0)) %>%
        group_by(message_type) %>%
        summarise(n = n(),
                  win_rate = sum(win_rate_flag) / n,
                  avg_days_of_hold = round(mean(days_between)),
                  win = sum(win),
                  loss = sum(loss),
                  avg_percent_chg = round(mean(percent_chg), 3),
                  sd_percent_chg = sd(percent_chg)) %>%
        ungroup() %>%
        group_by(message_type, n, win_rate, avg_days_of_hold, win, loss, avg_percent_chg, sd_percent_chg) %>%
        dplyr::mutate(loss = case_when(loss == 0 ~ 1, TRUE ~ loss)) %>%
        summarise(win_loss_ratio = sum(win) / sum(loss)) %>%
        ungroup() %>%
        dplyr::mutate(margin_of_error = round(1.96 * sd_percent_chg / sqrt(n-1), 3),
                      CI.up = avg_percent_chg + margin_of_error,
                      CI.down = avg_percent_chg - margin_of_error) %>%
        select(message_type, n, avg_days_of_hold, win_rate, win, loss, win_loss_ratio, avg_percent_chg, CI.down, CI.up) %>%
        arrange(message_type)

# # message level2 (with situations)
# msg_compare2 <- backtest_rand_evalDf %>%
#         dplyr::inner_join(poc %>%
#                                   select(symbol, eval_start_date = date,
#                                          year, volume, situation),
#                           by = c("symbol", "eval_start_date")) %>%
#         dplyr::mutate(win_rate_flag = case_when(net_value >0 ~ 1, TRUE ~ 0),
#                       win = case_when(net_value >0 ~ net_value, TRUE ~ 0),
#                       loss = case_when(net_value <=0 ~ abs(net_value), TRUE ~ 0)) %>%
#         group_by(message_type, situation) %>%
#         summarise(n = n(),
#                   win_rate = sum(win_rate_flag) / n,
#                   avg_days_of_hold = round(mean(days_between)),
#                   win = sum(win),
#                   loss = sum(loss),
#                   avg_percent_chg = round(mean(percent_chg), 3),
#                   sd_percent_chg = sd(percent_chg)) %>%
#         ungroup() %>%
#         group_by(message_type, situation, n, win_rate, avg_days_of_hold, win, loss, avg_percent_chg, sd_percent_chg) %>%
#         dplyr::mutate(loss = case_when(loss == 0 ~ 1, TRUE ~ loss)) %>%
#         summarise(win_loss_ratio = sum(win) / sum(loss)) %>%
#         ungroup() %>%
#         dplyr::mutate(margin_of_error = round(1.96 * sd_percent_chg / sqrt(n-1), 3),
#                       CI.up = avg_percent_chg + margin_of_error,
#                       CI.down = avg_percent_chg - margin_of_error) %>%
#         select(message_type, situation, n, avg_days_of_hold, win_rate, win, loss, win_loss_ratio, avg_percent_chg, CI.down, CI.up) %>%
#         arrange(message_type, situation)

###############################################################################################
########### <<< detail report >>>
# backtest_rand_evalDf2 <- backtest_rand_evalDf %>%
#         dplyr::inner_join(poc %>% 
#                                   select(symbol, eval_start_date = date, 
#                                          year, volume, situation),
#                           by = c("symbol", "eval_start_date")) 
# 
# ######### <<< year >>> ##################
# yearly <- backtest_rand_evalDf2 %>%
#         group_by(year) %>%
#         summarise(n = n(),
#                   win_loss_rate = round(sum(is_win_yn) / n, 3),
#                   avg_days_hold = round(mean(days_between)),
#                   mu_percent_change = sum(net_value) / (n * sum(fund_begin))) %>%
#         ungroup() %>%
#         inner_join(backtest_rand_evalDf2 %>%
#                            dplyr::mutate(percent_change = net_value / sum(fund_begin)) %>%
#                            group_by(year) %>%
#                            summarise(sd_percent_change = sd(percent_change)) %>%
#                            ungroup,
#                    by = "year") %>%
#         filter(!is.na(sd_percent_change)) %>%
#         dplyr::mutate(margin_of_error = 1.96 * sd_percent_change / sqrt(n-1),
#                       ci_percent_change_down = mu_percent_change - margin_of_error,
#                       ci_percent_change_up = mu_percent_change + margin_of_error) %>%
#         arrange(year)
# 
# ######### <<< situation >>> ##################
# situation <- backtest_rand_evalDf2 %>%
#         group_by(situation) %>%
#         summarise(n = n(),
#                   win_loss_rate = round(sum(is_win_yn) / n, 3),
#                   avg_days_hold = round(mean(days_between)),
#                   mu_percent_change = sum(net_value) / (n * sum(fund_begin))) %>%
#         ungroup() %>%
#         inner_join(backtest_rand_evalDf2 %>%
#                            dplyr::mutate(percent_change = net_value / sum(fund_begin)) %>%
#                            group_by(situation) %>%
#                            summarise(sd_percent_change = sd(percent_change)) %>%
#                            ungroup,
#                    by = "situation") %>%
#         filter(!is.na(sd_percent_change)) %>%
#         dplyr::mutate(margin_of_error = 1.96 * sd_percent_change / sqrt(n-1),
#                       ci_percent_change_down = mu_percent_change - margin_of_error,
#                       ci_percent_change_up = mu_percent_change + margin_of_error) %>%
#         arrange(situation)
# 
# ##########
# detail_report <- rbind(
#         yearly %>%
#                 dplyr::mutate(type = "year") %>%
#                 select(type, value = year, everything()),
#         situation %>%
#                 dplyr::mutate(type = "situation") %>%
#                 select(type, value = situation, everything())
# )
# 
# backtest_rand_evalDf2 %>% head()
# rand_eval_index symbol  close2     atr2      diff rank_by_upside_opp eval_start_date eval_end_date plus_x_date chg_in_close message_type days_between   net_value is_win_yn   percent_chg year   volume
# 1               1  GOOGL 14.1891 12.23382 0.1653622                  1      2007-04-05    2007-04-16  2007-04-11   -0.0148035   message_s3      11 days   9.2032046         1  0.0092032046 2007  5426100
# 2               1  GOOGL 14.1891 12.23382 0.1653622                  1      2007-04-05    2007-05-03  2007-04-11   -0.0148035   message_s2      28 days  -0.5160178         0 -0.0005160178 2007  5426100
# 3               1  GOOGL 14.1891 12.23382 0.1653622                  1      2007-04-05    2007-05-01  2007-04-11   -0.0148035   message_s1      26 days  -0.8564832         0 -0.0008564832 2007  5426100
# 4               1  GOOGL 14.1891 12.23382 0.1653622                  1      2007-04-05    2007-05-11  2007-04-11   -0.0148035   message_s4      36 days -12.3471896         0 -0.0123471896 2007  5426100
# 5               2   AMZN  5.0406  4.53998 0.1191811                  1      2007-07-26    2007-08-03  2007-07-31   -0.0651113   message_s3       8 days -66.4048752         0 -0.0664048752 2007 23267100
# 6               2   AMZN  5.0406  4.53998 0.1191811                  1      2007-07-26    2007-08-01  2007-07-31   -0.0651113   message_s1       6 days -81.8157449         0 -0.0818157449 2007 23267100
# situation
# 1 EBull-EBull::SBull-SBull
# 2 EBull-EBull::SBull-SBull
# 3 EBull-EBull::SBull-SBull
# 4 EBull-EBull::SBull-SBull
# 5 EBull-EBull::SBull-SBull
# 6 EBull-EBull::SBull-SBull
# 
# detail_report %>% head()
# # A tibble: 6 × 10
# type  value     n win_loss_rate avg_days_hold mu_percent_change sd_percent_change margin_of_error ci_percent_change_down ci_percent_change_up
# <chr> <chr> <int>         <dbl> <drtn>                    <dbl>             <dbl>           <dbl>                  <dbl>                <dbl>
#         1 year  2007     12         0.083 19 days                -0.0445            0.0419          0.0248                 -0.0693             -0.0197 
# 2 year  2009     16         0.375 20 days                -0.00144           0.0441          0.0223                 -0.0237              0.0209 
# 3 year  2010      8         0.625 20 days                 0.0101            0.0284          0.0210                 -0.0110              0.0311 
# 4 year  2011      8         0     11 days                -0.0282            0.0276          0.0204                 -0.0486             -0.00775
# 5 year  2014      4         0      8 days                -0.0273            0.00479         0.00542                -0.0327             -0.0219 
# 6 year  2015      4         0     16 days                -0.0752            0.0674          0.0763                 -0.152               0.00107





########################################
########### print results ######################
#backtest_rand_evalDf
#msg_summary

print(overall_compare)
#session_compare
print(msg_compare)
#msg_compare2
