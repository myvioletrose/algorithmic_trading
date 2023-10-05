#########################################
# equity trading curve analysis - month
etca <- poc %>%
        select(symbol, date, close, message_e1, message_e2)

date_df0 <- poc %>%
        select(date) %>%
        distinct() %>%
        arrange(date) %>%
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               month_key = format(as.Date(date), "%Y-%m") %>%
                       stringr::str_remove(pattern = "-") %>%
                       as.integer())

date_df_temp <- date_df0 %>%
        group_by(month_key) %>%
        summarise(first_date = min(date),
                  last_date = max(date)) %>%
        ungroup() %>%
        mutate(index = row_number(desc(month_key)))

date_df <- date_df0 %>%
        inner_join(date_df_temp, by = "month_key")

# tail(date_df)
# # A tibble: 6 × 7
#   date        year month month_key first_date last_date  index
#   <date>     <dbl> <dbl>     <int> <date>     <date>     <int>
# 1 2023-09-15  2023     9    202309 2023-09-01 2023-09-22     1
# 2 2023-09-18  2023     9    202309 2023-09-01 2023-09-22     1
# 3 2023-09-19  2023     9    202309 2023-09-01 2023-09-22     1
# 4 2023-09-20  2023     9    202309 2023-09-01 2023-09-22     1
# 5 2023-09-21  2023     9    202309 2023-09-01 2023-09-22     1
# 6 2023-09-22  2023     9    202309 2023-09-01 2023-09-22     1

# dim(date_df)
# [1] 6012    7

etca_start = 201601
etca_end = 202309
etca_look_back_period = 3

etca_start_base_begin = date_df %>%
        filter(month_key < etca_start) %>%
        select(month_key) %>%
        distinct() %>%
        mutate(reindex = row_number(desc(month_key))) %>%
        filter(reindex == etca_look_back_period) %>%
        .$month_key

etca_para0 <- date_df %>%
        filter(month_key >= etca_start_base_begin & month_key <= etca_end) %>%
        select(index, month_key, first_date, last_date) %>%
        distinct()

etca_para1 <- etca_para0 %>%
        select(index, month_key) %>%
        cross_join(etca_para0 %>% select(index, month_key)) %>%
        mutate(diff = index.y - index.x) %>%
        filter(diff <= etca_look_back_period & diff >=0) %>%
        select(eval_month_key_start = month_key.x, month_key = month_key.y)

etca_para2 <- etca_para0 %>%
        mutate(index = index + etca_look_back_period) %>%
        select(index, eval_month_key_start = month_key) %>%
        inner_join(etca_para0, by = "index") %>%
        select(eval_month_key_start, base_month_key = month_key, base_date = first_date)

etca_para <- etca_para1 %>%
        inner_join(etca_para2, by = "eval_month_key_start") %>%
        inner_join(etca_para0, by = "month_key") %>%
        select(eval_month_key = eval_month_key_start, 
               start_month_key = base_month_key, 
               end_month_key = month_key, 
               start_date = base_date, 
               end_date = last_date) %>%
        arrange(eval_month_key, end_month_key) %>%
        group_by(eval_month_key) %>%
        mutate(index = row_number(end_month_key)) %>%
        ungroup()
        
###########################################################################################
############# <<< begin evaluation >>> ###############################

# symbols to eval
symbols_to_eval = c("WMT")
#symbols_to_eval = poc$symbol %>% unique()

# message & fund - choose message_e*
message = "message_e1"
fund = 1000

# nested df
etca_nested = cross_join(symbols_to_eval %>% as.data.frame() %>% select(symbol = "."),
                         etca_para) %>%
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
                        
                        eval_month_key = temp_df$eval_month_key[k]
                        start_date = temp_df$start_date[k]
                        end_date = temp_df$end_date[k]
                                
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

etca_df = output_list %>% plyr::ldply() %>%
        mutate(year = substr(eval_month_key, 1, 4) %>% as.numeric) %>%
        arrange(symbol, eval_month_key, index) %>%
        group_by(symbol, eval_month_key) %>%
        mutate(net_value_output_lag1 = lag(net_value_output, 1),
               temp_flag = case_when(net_value_output > net_value_output_lag1 ~ 1, TRUE ~ 0),
               flag = case_when(temp_flag == 1 & eval_month_key == end_month_key ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        #filter(index == 4) %>%
        select(symbol, year, index, eval_month_key, start_month_key, end_month_key, net_value_output, flag) %>%
        arrange(symbol, eval_month_key)

etca_df %>% write_clip()































