# win/loss ratio
# avg day
# avg return ($)

proxy <- "proxy.bloomberg.com:81"

Sys.setenv(
        https_proxy = proxy,
        http_proxy = proxy
)

library(tictoc)
library(tidyr)
library(plyr)
library(tidyverse)
library(Hmisc)
library(clipr)

#####################################################################
# read data
# ha <- readRDS("ha.RDS")
# indicators <- readRDS("indicators.RDS")
# poc <- readRDS("poc.RDS")
# t0 <- readRDS("t0.RDS")

# extract data
threshold = 0.006
filter_date = '2024-05-01'

tic()
df <- poc %>%        
        select(symbol, date, 
               message_b, message_s, 
               in_the_buy_yn, is_first_buy_yn, atr,
               open, high, low, close, volume) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        mutate(high_n1 = lead(high, 1),
               high_n2 = lead(high, 2),
               high_n3 = lead(high, 3),
               high_n4 = lead(high, 4),
               high_n5 = lead(high, 5),
               
               close_n1 = lead(close, 1),
               close_n2 = lead(close, 2),
               close_n3 = lead(close, 3),
               close_n4 = lead(close, 4),
               close_n5 = lead(close, 5),
               
               low_n1 = lead(low, 1),
               low_n2 = lead(low, 2),
               low_n3 = lead(low, 3),
               low_n4 = lead(low, 4),
               low_n5 = lead(low, 5),
               
               open_n1 = lead(open, 1),
               
               # stop, limit, bid prices               
               stop_price = (open + low) / 2,               
               limit_price = (open + high) / 2,
               suggested_limit_price = (open_n1 *2 + high) / 3,               
               bid_price = (close *2 + high) / 3,
               # update limit price, bid_price
               limit_price = case_when(suggested_limit_price > limit_price ~ suggested_limit_price, TRUE ~ limit_price),
               bid_price = case_when(bid_price < stop_price ~ stop_price,
                                     bid_price > limit_price ~ limit_price, 
                                     TRUE ~ bid_price),
               # target, stop-loss
               #target = bid_price * (1 + threshold),
               target = limit_price * (1 + threshold),
               stop_loss = bid_price - (1 * atr),               
               # logic trigger buy
               entry_trade_yn = case_when(bid_price >= stop_price & bid_price <= limit_price ~ 1, TRUE ~ 0),
               year = lubridate::year(date)
        ) %>%
        ungroup() %>%
        filter(date < filter_date & !is.na(stop_loss))

# transform data
dfH0 <- df %>%
        filter(entry_trade_yn == 1) %>%
        select(symbol, date,
               target, high_n1:high_n5) %>%
        tidyr::gather(key, value, high_n1:high_n5) %>% 
        tidyr::separate(col = "key", into = c("key", "day"), sep = "high_n") %>%
        mutate(day = as.integer(day))

dfH <- dfH0 %>%
        mutate(key = "high",
               day = as.numeric(day),
               high_reach_target_flag = case_when(value > target ~ 1, TRUE ~ 0)) %>%
        filter(high_reach_target_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(n_day_reach_high = min(day)) %>%
        ungroup() %>% 
        join(dfH0 %>%
                     select(symbol, date,
                            n_day_reach_high = day,
                            high_n = value), 
             by = c("symbol", "date", "n_day_reach_high")) %>%
        arrange(symbol, date)

dfC0 <- df %>%
        filter(entry_trade_yn == 1) %>%
        select(symbol, date,
               target, close_n1:close_n5) %>%
        tidyr::gather(key, value, close_n1:close_n5) %>%
        tidyr::separate(col = "key", into = c("key", "day"), sep = "close_n")

dfC <-  dfC0 %>%
        mutate(key = "close",
               day = as.numeric(day),
               close_reach_target_flag = case_when(value > target ~ 1, TRUE ~ 0)) %>%
        filter(close_reach_target_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(n_day_reach_close = min(day)) %>%
        ungroup() %>%
        join(dfC0 %>%
                     select(symbol, date,
                            n_day_reach_close = day,
                            close_n = value), 
             by = c("symbol", "date", "n_day_reach_close")) %>%
        arrange(symbol, date)

dfL0 <- df %>%
        filter(entry_trade_yn == 1) %>%
        select(symbol, date,
               stop_loss, low_n1:low_n5) %>%
        tidyr::gather(key, value, low_n1:low_n5) %>%
        tidyr::separate(col = "key", into = c("key", "day"), sep = "low_n")

dfL <- dfL0 %>%
        mutate(key = "low",
               day = as.numeric(day),
               low_reach_stop_loss_flag = case_when(value < stop_loss ~ 1, TRUE ~ 0)) %>%
        filter(low_reach_stop_loss_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(n_day_reach_low = min(day)) %>%
        ungroup() %>%
        join(dfL0 %>%
                     select(symbol, date,
                            n_day_reach_low = day,
                            low_n = value), 
             by = c("symbol", "date", "n_day_reach_low")) %>%
        arrange(symbol, date)

##############################
df2 <- df %>%
        select(symbol, date, year,
               message_b, message_s, 
               is_first_buy_yn, entry_trade_yn,
               open, high, low, close, 
               stop_price, limit_price,
               bid_price, target, stop_loss,
               close_n5) %>%
        left_join(dfH, by = c("symbol", "date")) %>%
        left_join(dfC, by = c("symbol", "date")) %>%
        left_join(dfL, by = c("symbol", "date")) %>%
        mutate(signal_entry_flag = case_when(grepl("buy|near|sma alert", message_s, ignore.case = TRUE) ~ 1, 
                                             grepl("sell|down", message_s, ignore.case = TRUE) ~ -1,
                                             TRUE ~ 0),
               is_high_win_yn = case_when(is.na(n_day_reach_high) ~ 0, TRUE ~ 1),
               is_close_win_yn = case_when(is.na(n_day_reach_close) ~ 0, TRUE ~ 1),
               reach_stop_loss_yn = case_when(is.na(n_day_reach_low) ~ 0, TRUE ~ 1),
               
               diff = case_when((reach_stop_loss_yn == 0 & is_high_win_yn == 1) ~ high_n - bid_price,
                                (reach_stop_loss_yn == 1 & n_day_reach_low > n_day_reach_high) ~ high_n - bid_price,
                                (reach_stop_loss_yn == 1 & ((n_day_reach_low < n_day_reach_high)|is.na(n_day_reach_high))) ~ low_n - bid_price,
                                TRUE ~ (close_n5 - bid_price)),
               
               is_win_yn = case_when(diff > 0 & (reach_stop_loss_yn == 0 | n_day_reach_low > n_day_reach_high) ~ 1,
                                     TRUE ~ 0))

trade_entered <- df2 %>%
        group_by(symbol, year) %>%
        summarise(pct_trade_made = mean(entry_trade_yn)) %>%
        arrange(symbol, year)

win_pct = df2 %>%
        filter(entry_trade_yn == 1) %>%
        group_by(symbol, signal_entry_flag) %>%
        summarise(n = n(),
                  win_pct = mean(is_win_yn),
                  avg_diff = mean(diff)) %>%
        arrange(symbol, signal_entry_flag)

win_pct_spread <- win_pct %>%
        filter(signal_entry_flag != 0) %>%
        select(-n) %>%
        tidyr::gather(key, value, win_pct:avg_diff) %>%
        tidyr::spread(signal_entry_flag, value) %>%
        mutate(is_1_better_yn = case_when(`1` > `-1` ~ 1, TRUE ~ 0)) %>%
        arrange(symbol, key)

win_pct2 = df2 %>%
        filter(entry_trade_yn == 1) %>%
        group_by(symbol, signal_entry_flag, year) %>%
        summarise(n = n(),
                  win_pct = mean(is_win_yn),
                  avg_diff = mean(diff)) %>%
        #arrange(symbol, signal_entry_flag, year)
        arrange(symbol, year)

win_pct3 = df2 %>%
        filter(entry_trade_yn == 1) %>%
        group_by(symbol, signal_entry_flag, year, message_s) %>%
        summarise(n = n(),
                  win_pct = mean(is_win_yn),
                  avg_diff = mean(diff)) %>%
        arrange(symbol, signal_entry_flag, year, message_s)

#win_pct3 %>% write_clip()

################################################################################################################
output = with(df2 %>% filter(entry_trade_yn == 1),
              ftable(year, message_s, 
                     is_win_yn)) %>%
        as.data.frame() %>%
        tidyr::spread(is_win_yn, Freq) %>%
        mutate(ratio = round(`1` / `0`, 1)) %>%
        arrange(year, desc(ratio))

n2 = output %>%
        #filter(grepl("demark", message_b, ignore.case = TRUE)) %>%
        #filter(`1` > 500) %>%
        select(year, message_s, `1`, `0`, ratio) %>%
        arrange(year, desc(ratio))

n2 %>%
        arrange(message_s, year)

n2 %>%
        filter(`0` >0) %>%
        group_by(message_s) %>%
        summarise(`1` = mean(`1`),
                  ratio = mean(ratio)) %>%
        arrange(desc(ratio))

sum(n2$`1`) / sum(n2$`0`)
toc()







#######################################################################
# feature at a glance
modelDf <- poc %>%
        select(c(symbol, date, message_s,
                 open, high, low, close, volume, volume_inconsistency_alert,
                 ends_with("_yn")|ends_with("_flag"))) %>%
        select(-is_first_buy_yn, -in_the_buy_yn, -proxy_flag) %>%
        inner_join(df2 %>%
                           filter(entry_trade_yn == 1 & 
                                          signal_entry_flag == 1) %>%
                           select(symbol, date, bid_price, target, is_win_yn),
                   by = c("symbol", "date")) %>%
        select(symbol, date, open, high, low, close, volume,
               bid_price, target, is_win_yn, message_s, volume_inconsistency_alert, everything()) %>%
        arrange(symbol, date)

modelDf2 <- modelDf %>%
        tidyr::gather(key, value, message_s:rising_flag)

test_key <- modelDf2$key %>% unique()

for(i in 1:length(test_key)){
        cat(paste(rep("#", 100), collapse = ""))
        cat("\n")
        cat("\n")
        cat(test_key[i])
        cat("\n")
        
        print(
        modelDf2 %>%
                filter(key == test_key[i]) %>%
                select(value, is_win_yn) %>%
                ftable() %>%
                prop.table(margin = 1) %>%
                round(2)
        )
        cat("\n")
        cat("\n")
}

# does the logic, execution make sense?
# how are "buy", "down" message measurably different?
# classification model?














