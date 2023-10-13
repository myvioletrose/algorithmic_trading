#########################################
# equity trading curve analysis - day
day_df_a1 <- poc %>%
        filter(symbol == 'SPY') %>%
        select(date) %>%
        distinct() %>%
        arrange(date) %>%
        mutate(type = "every_2_day",
               day_n2 = lead(date, 2),
               day_n4 = lead(date, 4),
               day_n6 = lead(date, 6),
               day_n8 = lead(date, 8),
               day_n10 = lead(date, 10),
               day_n12 = lead(date, 12),
               day_n14 = lead(date, 14),
               day_n16 = lead(date, 16),
               day_n18 = lead(date, 18),
               day_n20 = lead(date, 20))

day_df_a2 <- day_df_a1 %>%
        tidyr::gather(key, ndate, day_n2:day_n20) %>%
        mutate(eval_date = date) %>%
        select(eval_date, type, key, from = date, to = ndate)

day_df_b1 <- poc %>%
        filter(symbol == 'SPY') %>%
        select(date) %>%
        distinct() %>%
        arrange(date) %>%
        mutate(type = "every_3_day",
               day_n3 = lead(date, 3),
               day_n6 = lead(date, 6),
               day_n9 = lead(date, 9),
               day_n12 = lead(date, 12),
               day_n15 = lead(date, 15),
               day_n18 = lead(date, 18))

day_df_b2 <- day_df_b1 %>%
        tidyr::gather(key, ndate, day_n3:day_n18) %>%
        mutate(eval_date = date) %>%
        select(eval_date, type, key, from = date, to = ndate)

day_df_c1 <- poc %>%
        filter(symbol == 'SPY') %>%
        select(date) %>%
        distinct() %>%
        arrange(date) %>%
        mutate(type = "every_4_day",
               day_n4 = lead(date, 4),
               day_n8 = lead(date, 8),
               day_n12 = lead(date, 12),
               day_n16 = lead(date, 16),
               day_n20 = lead(date, 20))

day_df_c2 <- day_df_c1 %>%
        tidyr::gather(key, ndate, day_n4:day_n20) %>%
        mutate(eval_date = date) %>%
        select(eval_date, type, key, from = date, to = ndate)

day_df <- rbind(day_df_a2, day_df_b2, day_df_c2) %>%
        arrange(type, eval_date, to)

# head(day_df, 15)
# # A tibble: 15 × 5
#   eval_date  type        key     from       to        
#   <date>     <chr>       <chr>   <date>     <date>    
# 1 1999-11-01 every_2_day day_n2  1999-11-01 1999-11-03
# 2 1999-11-01 every_2_day day_n4  1999-11-01 1999-11-05
# 3 1999-11-01 every_2_day day_n6  1999-11-01 1999-11-09
# 4 1999-11-01 every_2_day day_n8  1999-11-01 1999-11-11
# 5 1999-11-01 every_2_day day_n10 1999-11-01 1999-11-15
# 6 1999-11-01 every_2_day day_n12 1999-11-01 1999-11-17
# 7 1999-11-01 every_2_day day_n14 1999-11-01 1999-11-19
# 8 1999-11-01 every_2_day day_n16 1999-11-01 1999-11-23
# 9 1999-11-01 every_2_day day_n18 1999-11-01 1999-11-26
# 10 1999-11-01 every_2_day day_n20 1999-11-01 1999-11-30
# 11 1999-11-02 every_2_day day_n2  1999-11-02 1999-11-04
# 12 1999-11-02 every_2_day day_n4  1999-11-02 1999-11-08
# 13 1999-11-02 every_2_day day_n6  1999-11-02 1999-11-10
# 14 1999-11-02 every_2_day day_n8  1999-11-02 1999-11-12
# 15 1999-11-02 every_2_day day_n10 1999-11-02 1999-11-16

# dim(day_df)
# [1] 126273    5
#etca_start = '2023-05-01'
#etca_end = '2023-09-05'

etca_para <- day_df %>%
        #ilter(eval_date >= etca_start & eval_date <= etca_end) %>%
        filter(eval_date %in% rand_list_target_dates) %>%
        arrange(type, eval_date, to) 

###########################################################################################
############# <<< begin evaluation >>> ###############################

# symbols to eval
symbols_to_eval = c("TSLA")

# symbols to eval, date
symbols_to_eval_df = poc %>% 
        filter(symbol %in% symbols_to_eval) %>%
        filter(date %in% rand_list_target_dates) %>%
        dplyr::mutate_at("message_s", msg_string_update) %>%
        dplyr::mutate(message_s = case_when(message_s == "buy" ~ 1,
                                            #message_s == "sell" ~ -1,
                                            TRUE ~ 0)) %>%
        filter(message_s == 1) %>%
        select(symbol, date)

# message & fund - choose message_e*
message = "message_e1"
fund = 1000

# nested df
# etca_nested = cross_join(symbols_to_eval %>% as.data.frame() %>% select(symbol = "."),
#                          etca_para) %>%
#         tidyr::nest(data = -symbol)
etca_loop_all_dates = symbols_to_eval_df %>%
        inner_join(etca_para, by = c("date" = "eval_date"), multiple = "all") %>%
        arrange(symbol, type, date, to)

etca_nested = etca_loop_all_dates %>%
        select(symbol, date, from, to) %>%
        distinct() %>%
        tidyr::nest(data = -symbol)

output_list = vector(mode = "list", length = nrow(etca_nested))

tic()
for(i in 1:nrow(etca_nested)){
        
        for(j in 1:nrow(etca_nested)){
                
                s = etca_nested$symbol[j]
                col = message
                f = fund
                temp_df = etca_nested$data[[j]]
                net_value_output <- vector(mode = "list", length = nrow(temp_df))
                
                for(k in 1:nrow(temp_df)){
                        
                        start_date = temp_df$from[k]
                        end_date = temp_df$to[k]
                        
                        # get data
                        x <- let(c(COL = col),
                                 poc %>%
                                         dplyr::filter(symbol == s &
                                                               date >= start_date & 
                                                               date <= end_date) %>%
                                         dplyr::mutate(message = COL) %>%
                                         dplyr::mutate_at("message", msg_string_update) %>%
                                         dplyr::select(symbol, date, open, high, low, close, message))
                        
                        # strategyEval
                        try({y <- strategyEval(fund_begin = fund, x) %>% .$net_value}, silent = TRUE)
                        
                        net_value_output[[k]] = y
                        
                }
                
                # put together
                temp_df$symbol = s
                temp_df$net_value_output = unlist(net_value_output)
                
                output_list[[j]] = temp_df
                print(paste0("<<< ", glue(s), " done >>>"))
        }
        
}
toc()

################################################
############ >>> test red_flag version 1
# etca_df0 = output_list %>% plyr::ldply() %>%
#         inner_join(etca_loop_all_dates, by = c("symbol", "date", "from", "to"), multiple = "all") %>%
#         filter(type == "every_2_day") %>%
#         mutate(year = lubridate::year(date),
#                month = lubridate::month(date),
#                month_key = format(as.Date(date), "%Y-%m") %>%
#                        stringr::str_remove(pattern = "-") %>%
#                        as.integer()) %>%
#         arrange(symbol, date, to) %>%
#         group_by(symbol, date) %>%
#         mutate(net_value_output_lag1 = lag(net_value_output, 1),
#                temp_flag = case_when(net_value_output <= net_value_output_lag1 ~ 1, TRUE ~ 0),
#                temp_flag_lag1 = lag(temp_flag, 1),
#                red_flag = case_when(temp_flag == 1 & temp_flag_lag1 == 1 ~ 1, TRUE ~ 0)) %>%
#         ungroup() %>%
#         select(symbol, year, month, month_key,
#                date, key, from, to,
#                net_value_output,
#                temp_flag, red_flag) %>%
#         arrange(symbol, date, to)
# 
# etca_df = etca_df0 %>%
#         filter(red_flag == 1) %>%
#         group_by(symbol, date) %>%
#         summarise(sell_date = min(to)) %>%
#         ungroup()

###################################################################
###################### >>> test test red_flag version 2
etca_df1 = output_list %>% plyr::ldply() %>%
        inner_join(etca_loop_all_dates, by = c("symbol", "date", "from", "to"), multiple = "all") %>%
        arrange(symbol, type, date, to) %>%
        group_by(symbol, type, date) %>%
        mutate(net_value_output_lag1 = lag(net_value_output, 1),
               temp_flag = case_when(net_value_output <= net_value_output_lag1 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, 
               type, date, key, from, to,
               net_value_output, 
               temp_flag) %>%
        arrange(symbol, type, date, to)

etca_df2 <- etca_df1 %>%
        filter(type == "every_2_day") %>%
        select(symbol, date, key, from, to, net_value_output2 = net_value_output, temp_flag2 = temp_flag)

etca_df3 <- etca_df1 %>%
        filter(type == "every_3_day") %>%
        select(symbol, date, key, from, to, net_value_output3 = net_value_output, temp_flag3 = temp_flag)

etca_df4 <- etca_df1 %>%
        filter(type == "every_4_day") %>%
        select(symbol, date, key, from, to, net_value_output4 = net_value_output, temp_flag4 = temp_flag)

############################################
etca_df <- etca_df1 %>%
        left_join(etca_df2 %>%
                          select(-from, -to), 
                  by = c("symbol", "date", "key")) %>%
        left_join(etca_df3 %>%
                          select(-from, -to), 
                  by = c("symbol", "date", "key")) %>%
        left_join(etca_df4 %>%
                          select(-from, -to), 
                  by = c("symbol", "date", "key")) %>%
        mutate(temp_flag2 = case_when(is.na(temp_flag2) ~ 0, TRUE ~ temp_flag2),
               temp_flag3 = case_when(is.na(temp_flag3) ~ 0, TRUE ~ temp_flag3),
               temp_flag4 = case_when(is.na(temp_flag4) ~ 0, TRUE ~ temp_flag4),
               red_flag = temp_flag2 + temp_flag3 + temp_flag4,
               red_flag = case_when(red_flag >= 2 ~ 1, TRUE ~ 0)) %>%
        filter(red_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(sell_date = min(to)) %>%
        ungroup()

# etca_df
# # A tibble: 124 × 3
# symbol date       sell_date 
# <chr>  <date>     <date>    
# 1 AMC    2014-05-27 2014-06-04
# 2 AMC    2014-11-26 2014-12-05
# 3 AMC    2015-01-12 2015-01-29
# 4 AMC    2016-10-06 2016-10-18
# 5 AMC    2016-12-21 2016-12-30
# 6 AMC    2018-01-02 2018-01-10
# 7 AMC    2019-08-21 2019-09-17
# 8 AMC    2020-05-26 2020-06-11
# 9 AMC    2020-11-20 2020-12-01
# 10 AMC    2021-01-20 2021-02-01

sell_date_index = sapply(1:length(etca_df$sell_date), \(x) {which(unique_trading_date == etca_df$sell_date[x])}) + 1
sell_date_adj = unique_trading_date[sell_date_index]

etca_df_summary <- etca_df %>%
        mutate(sell_date_adj = sell_date_adj,
               days_between_adj = sell_date_adj - date) %>%
        join(poc %>% 
                     select(symbol, date, buy_at = close),
             by = c("symbol", "date")) %>%
        join(poc %>% 
                     select(symbol, sell_date_adj = date, sell_at_adj = close),
             by = c("symbol", "sell_date_adj")) %>%
        inner_join(
                backtest_rand_evalDf %>%
                        filter(message_type == "message_e1") %>%
                        select(symbol, date = eval_start_date, eval_end_date, days_between) %>%
                        join(poc %>%
                                     select(symbol, eval_end_date = date, sell_at = close),
                             by = c("symbol", "eval_end_date")),
                by = c("symbol", "date")) %>%
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               month_key = format(as.Date(date), "%Y-%m") %>%
                       stringr::str_remove(pattern = "-") %>%
                       as.integer()) %>%
        select(symbol, year, month, month_key,
               buy_date = date, sell_date_default = eval_end_date, sell_date_adj,
               days_between, days_between_adj,
               buy_at, sell_at, sell_at_adj) %>%
        mutate(final_sell_at = case_when(days_between < days_between_adj ~ sell_at, TRUE ~ sell_at_adj),
               is_win_yn = case_when(sell_at > buy_at ~ 1, TRUE ~ 0),
               is_win_yn_adj = case_when(sell_at_adj > buy_at ~ 1, TRUE ~ 0),
               is_win_yn_final_adj = case_when(final_sell_at > buy_at ~ 1, TRUE ~ 0)) %>%
        mutate(percent_chg = (sell_at - buy_at) / buy_at,
               percent_chg_adj = (final_sell_at - buy_at) / buy_at) %>%
        arrange(symbol, buy_date)

etca_df_summary %>% write_clip()                












###############################################################################################




pocX = poc %>%
        left_join(etca_df %>% 
                          select(-date) %>%
                          mutate(etca_red_flag = 1), 
                  by = c("symbol" = "symbol", "date" = "sell_date")) %>%
        mutate(message_e1 = case_when(in_the_buy_yn == 1 & etca_red_flag == 1 ~ "sell - etca", TRUE ~ message_e1),
               message_e2 = case_when(in_the_buy_yn == 1 & etca_red_flag == 1 ~ "sell - etca", TRUE ~ message_e2))

# choose message_e* for YTD evaluation
pocX.eval <- pocX %>%
        mutate(across(matches("message_e([[:digit:]]$)"), msg_string_update)) %>%
        select(symbol, year, date, open, high, low, close, contains("message_e")) %>%
        arrange(symbol, date)