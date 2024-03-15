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
threshold = 1.005

df <- poc %>%
        select(symbol, date, 
               message_b, message_s, 
               in_the_buy_yn, is_first_buy_yn,
               open, high, low, close, volume) %>%
        mutate(high_n2 = lead(high, 2),
               high_n3 = lead(high, 3),
               high_n4 = lead(high, 4),
               high_n5 = lead(high, 5),
               close_n2 = lead(close, 2),
               close_n3 = lead(close, 3),
               close_n4 = lead(close, 4),
               close_n5 = lead(close, 5),
               signal_entry_flag = case_when(grepl("buy", message_b, ignore.case = TRUE) ~ 1, TRUE ~ 0),
               target = case_when(signal_entry_flag == 1 ~ close * threshold),
               year = lubridate::year(date)
               ) %>%
        filter(signal_entry_flag == 1 & date < '2024-03-01')

# transform data
dfH <- df %>%
        select(symbol, date,
               target, high_n2:high_n5) %>%
        tidyr::gather(key, value, high_n2:high_n5) %>%
        tidyr::separate(col = "key", into = c("key", "day"), sep = "high_n") %>%
        mutate(key = "high",
               day = as.numeric(day),
               high_reach_target_flag = case_when(value > target ~ 1, TRUE ~ 0)) %>%
        filter(high_reach_target_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(n_day_reach_high = min(day)) %>%
        ungroup() %>%
        arrange(symbol, date)

dfC <- df %>%
        select(symbol, date,
               target, close_n2:close_n5) %>%
        tidyr::gather(key, value, close_n2:close_n5) %>%
        tidyr::separate(col = "key", into = c("key", "day"), sep = "close_n") %>%
        mutate(key = "close",
               day = as.numeric(day),
               close_reach_target_flag = case_when(value > target ~ 1, TRUE ~ 0)) %>%
        filter(close_reach_target_flag == 1) %>%
        group_by(symbol, date) %>%
        summarise(n_day_reach_close = min(day)) %>%
        ungroup() %>%
        arrange(symbol, date)

##############################
df2 <- df %>%
        select(symbol, date, year,
               message_b, message_s, 
               is_first_buy_yn, 
               close, target, 
               close_n5) %>%
        left_join(dfH, by = c("symbol", "date")) %>%
        left_join(dfC, by = c("symbol", "date")) %>%
        mutate(is_high_win_yn = case_when(is.na(n_day_reach_high) ~ 0, TRUE ~ 1),
               is_close_win_yn = case_when(is.na(n_day_reach_close) ~ 0, TRUE ~ 1),
               diff = case_when(is_high_win_yn == 1 ~ (target - close),
                                TRUE ~ (close_n5 - close)),
               is_win_yn = case_when(diff > 0 ~ 1,
                                     TRUE ~ 0))

################################################################################################################
output = with(df2,
              ftable(year, message_b, 
                     is_win_yn)) %>%
        as.data.frame() %>%
        tidyr::spread(is_win_yn, Freq) %>%
        mutate(ratio = round(`1` / `0`, 1)) %>%
        arrange(year, desc(ratio))
                       
n2 = output %>%
        #filter(grepl("demark", message_b, ignore.case = TRUE)) %>%
        #filter(`1` > 500) %>%
        select(year, message_b, `1`, `0`, ratio) %>%
        arrange(year, desc(ratio))

n2 %>%
        arrange(message_b, year)

n2 %>%
        filter(`0` >0) %>%
        group_by(message_b) %>%
        summarise(`1` = mean(`1`),
                  ratio = mean(ratio)) %>%
        arrange(desc(ratio))

sum(n2$`1`) / sum(n2$`0`)












