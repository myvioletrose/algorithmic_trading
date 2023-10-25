#########################################
# date parameters
#etca_start = "2023-10-06"
#etca_end = "2023-10-10"
#etca_look_back_period = 63

# symbols to eval
#symbols_to_eval = poc$symbol %>% unique()
#symbols_to_eval = c("DKNG", "AMD")

# message & fund - choose message_e*
#message_to_eval = "message_s"
fund = 1000

# equity trading curve analysis - day
etca <- poc %>%
        select(symbol, date, close, message_e0, message_e1, message_e2)

date_df0 <- poc %>%
        select(date) %>%
        distinct() %>%
        arrange(desc(date)) %>%
        mutate(index = row_number(),
               look_back_index = index + etca_look_back_period) %>%
        arrange(date)

date_df1 = date_df0 %>%
        inner_join(date_df0 %>%
                           select(etca_date = date, 
                                  start_pos = index), 
                   join_by(look_back_index >= start_pos, 
                           date >= etca_date)) %>%
        filter(date >= etca_start & date <= etca_end) %>%
        select(date, etca_date) %>%
        arrange(date)

date_df2 <- date_df1 %>%
        group_by(date) %>%
        summarise(start_date = min(etca_date)) %>%
        ungroup() %>%
        mutate(index = row_number(desc(start_date)))

date_df <- date_df1 %>%
        inner_join(date_df2, by = "date") %>%
        filter(start_date != etca_date) %>%
        select(date, start_date, end_date = etca_date, index) %>%
        arrange(date, start_date, end_date)

#date_df
# A tibble: 130 × 4
# date       start_date end_date  index
# <date>     <date>     <date>     <int>
# 1 2023-09-01 2023-07-27 2023-07-28     5
# 2 2023-09-01 2023-07-27 2023-07-31     5
# 3 2023-09-01 2023-07-27 2023-08-01     5
# 4 2023-09-01 2023-07-27 2023-08-02     5
# 5 2023-09-01 2023-07-27 2023-08-03     5
# 6 2023-09-01 2023-07-27 2023-08-04     5
# 7 2023-09-01 2023-07-27 2023-08-07     5
# 8 2023-09-01 2023-07-27 2023-08-08     5
# 9 2023-09-01 2023-07-27 2023-08-09     5
# 10 2023-09-01 2023-07-27 2023-08-10    5
# … with 120 more rows
# ℹ Use `print(n = ...)` to see more rows

###########################################################################################
############# <<< begin evaluation >>> ###############################

# nested df
etca_nested = cross_join(symbols_to_eval %>% as.data.frame() %>% select(symbol = "."),
                         date_df) %>%
        tidyr::nest(data = -symbol)

output_list = vector(mode = "list", length = nrow(etca_nested))

tic()
for(j in 1:nrow(etca_nested)){
        
        s = etca_nested$symbol[j]
        col = message_to_eval
        f = fund
        temp_df = etca_nested$data[[j]]
        net_value_output <- vector(mode = "list", length = nrow(temp_df))
        
        for(k in 1:nrow(temp_df)){
                
                eval_date = temp_df$date[k]
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
toc()

etca_df = output_list %>% plyr::ldply() %>%
        mutate(year = lubridate::year(date)) %>%
        arrange(symbol, index, end_date) %>%
        group_by(symbol, index) %>%
        mutate(slow = pracma::movavg(net_value_output, n = 26, type = "e"),
               fast = pracma::movavg(net_value_output, n = 12, type = "e"),
               macd = fast - slow,
               sig = pracma::movavg(macd, n = 9, type = "e"),
               diff = macd - sig) %>%
        mutate(diff_lag1 = lag(diff, 1),
               diff_lag2 = lag(diff, 2),
               diff_lag3 = lag(diff, 3),
               flag = case_when(diff > diff_lag1 & diff_lag1 > diff_lag2 & diff_lag2 > diff_lag3 & diff >0 ~ 1,
                                diff < diff_lag1 & diff_lag1 < diff_lag2 & diff_lag2 < diff_lag3 & diff <0 ~ -1,
                                TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, year, date, index,
               start_date, end_date, 
               net_value_output, 
               slow, fast, macd, sig, diff, flag) %>%
        arrange(symbol, index, end_date)
