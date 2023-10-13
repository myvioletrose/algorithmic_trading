#########################################
# equity trading curve analysis - week
week_df0 <- poc %>%
        filter(symbol == 'SPY') %>%        
        tidyquant::tq_transmute(mutate_fun = to.period,
                                period = "weeks",
                                col_rename = "drop_this_col") %>%
        select(week_end_date = date) %>%
        mutate(week_end_date_lag1 = lag(week_end_date, 1))

week_df1 <- poc %>%
        filter(symbol == 'SPY') %>%
        select(date) %>%
        distinct() %>%
        inner_join(week_df0, 
                   join_by(date <= week_end_date, date > week_end_date_lag1)) %>%
        select(date, week_end_date)

week_df_temp <- week_df1 %>%
        group_by(week_end_date) %>%
        summarise(week_begin_date = min(date)) %>%
        ungroup() %>%
        mutate(index = row_number(desc(week_begin_date)))

week_df <- week_df1 %>%
        inner_join(week_df_temp, by = "week_end_date") %>%
        select(date, week_begin_date, week_end_date, index)

# tail(week_df, 20)
# A tibble: 20 × 4
#   date       week_begin_date week_end_date index
#   <date>     <date>          <date>        <int>
# 1 2023-08-28 2023-08-28      2023-09-01        5
# 2 2023-08-29 2023-08-28      2023-09-01        5
# 3 2023-08-30 2023-08-28      2023-09-01        5
# 4 2023-08-31 2023-08-28      2023-09-01        5
# 5 2023-09-01 2023-08-28      2023-09-01        5
# 6 2023-09-05 2023-09-05      2023-09-08        4
# 7 2023-09-06 2023-09-05      2023-09-08        4
# 8 2023-09-07 2023-09-05      2023-09-08        4
# 9 2023-09-08 2023-09-05      2023-09-08        4
# 10 2023-09-11 2023-09-11      2023-09-15        3
# 11 2023-09-12 2023-09-11      2023-09-15        3
# 12 2023-09-13 2023-09-11      2023-09-15        3
# 13 2023-09-14 2023-09-11      2023-09-15        3
# 14 2023-09-15 2023-09-11      2023-09-15        3
# 15 2023-09-18 2023-09-18      2023-09-22        2
# 16 2023-09-19 2023-09-18      2023-09-22        2
# 17 2023-09-20 2023-09-18      2023-09-22        2
# 18 2023-09-21 2023-09-18      2023-09-22        2
# 19 2023-09-22 2023-09-18      2023-09-22        2
# 20 2023-09-25 2023-09-25      2023-09-25        1

# dim(week_df)
# [1] 6008    4
etca_start = '2023-01-01'
etca_end = '2023-09-25'
etca_look_back_period = 12

etca_start_base_begin = week_df %>%
        filter(week_begin_date < etca_start) %>%
        select(week_begin_date) %>%
        distinct() %>%
        mutate(reindex = row_number(desc(week_begin_date))) %>%
        filter(reindex == etca_look_back_period) %>%
        .$week_begin_date

etca_para0 <- week_df %>%
        filter(week_begin_date >= etca_start_base_begin & 
                       week_begin_date <= etca_end) %>%
        select(index, week_begin_date, week_end_date) %>%
        distinct()

etca_para1 <- etca_para0 %>%
        select(index, week_begin_date) %>%
        cross_join(etca_para0 %>% select(index, week_begin_date)) %>%
        mutate(diff = index.y - index.x) %>%
        filter(diff <= etca_look_back_period & diff >=0) %>%
        select(eval_week_start = week_begin_date.x, 
               week_begin = week_begin_date.y)

etca_para2 <- etca_para0 %>%
        mutate(index = index + etca_look_back_period) %>%
        select(index, eval_week_start = week_begin_date) %>%
        inner_join(etca_para0, by = "index") %>%
        select(eval_week_start, week_begin_date, week_end_date)

etca_para <- etca_para1 %>%
        inner_join(etca_para2 %>% select(-week_end_date), by = "eval_week_start") %>%
        inner_join(etca_para0, by = c("week_begin" = "week_begin_date")) %>%
        arrange(eval_week_start, week_begin) %>%
        group_by(eval_week_start) %>%
        mutate(index = row_number(week_end_date)) %>%
        ungroup() %>%
        select(eval_week_start, index, week_begin, from = week_begin_date, to = week_end_date)

# etca_para %>% tail(15)
# # A tibble: 15 × 5
#   eval_week_start index week_begin from       to        
#   <date>          <int> <date>     <date>     <date>    
# 1 2023-09-18         12 2023-09-11 2023-06-26 2023-09-15
# 2 2023-09-18         13 2023-09-18 2023-06-26 2023-09-22
# 3 2023-09-25          1 2023-07-03 2023-07-03 2023-07-07
# 4 2023-09-25          2 2023-07-10 2023-07-03 2023-07-14
# 5 2023-09-25          3 2023-07-17 2023-07-03 2023-07-21
# 6 2023-09-25          4 2023-07-24 2023-07-03 2023-07-28
# 7 2023-09-25          5 2023-07-31 2023-07-03 2023-08-04
# 8 2023-09-25          6 2023-08-07 2023-07-03 2023-08-11
# 9 2023-09-25          7 2023-08-14 2023-07-03 2023-08-18
# 10 2023-09-25          8 2023-08-21 2023-07-03 2023-08-25
# 11 2023-09-25          9 2023-08-28 2023-07-03 2023-09-01
# 12 2023-09-25         10 2023-09-05 2023-07-03 2023-09-08
# 13 2023-09-25         11 2023-09-11 2023-07-03 2023-09-15
# 14 2023-09-25         12 2023-09-18 2023-07-03 2023-09-22
# 15 2023-09-25         13 2023-09-25 2023-07-03 2023-09-25

###########################################################################################
############# <<< begin evaluation >>> ###############################

# symbols to eval
symbols_to_eval = c("UBER")

# message & fund - choose message_e*
message = "message_e1"
fund = 1000

# nested df
etca_nested = cross_join(symbols_to_eval %>% as.data.frame() %>% select(symbol = "."),
                         etca_para) %>%
        tidyr::nest(data = -symbol)

output_list = vector(mode = "list", length = nrow(etca_nested))

tic()
for(j in 1:nrow(etca_nested)){
        
        s = etca_nested$symbol[j]
        col = message
        f = fund
        temp_df = etca_nested$data[[j]]
        net_value_output <- vector(mode = "list", length = nrow(temp_df))
        
        for(k in 1:nrow(temp_df)){
                
                eval_week_start = temp_df$eval_week_start[k]
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
toc()

etca_df = output_list %>% plyr::ldply() %>%
        mutate(year = lubridate::year(eval_week_start),
               month = lubridate::month(eval_week_start),
               month_key = format(as.Date(eval_week_start), "%Y-%m") %>%
                       stringr::str_remove(pattern = "-") %>%
                       as.integer()) %>%
        arrange(symbol, eval_week_start, index) %>%
        group_by(symbol, eval_week_start) %>%
        mutate(net_value_output_lag1 = lag(net_value_output, 1),
               temp_flag = case_when(net_value_output > net_value_output_lag1 ~ 1, TRUE ~ 0),
               flag = case_when(temp_flag == 1 & eval_week_start == week_begin ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        #filter(index == 4) %>%
        select(symbol, year, month, month_key, 
               eval_week_start, index, week_begin, from, to,
               net_value_output, flag) %>%
        arrange(symbol, eval_week_start, index)

#etca_df %>% write_clip()
etca_df
