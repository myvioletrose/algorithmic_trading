# symbols
s = symbols

# get data
df = symbols %>%
        tq_get(get = "alphavantage", 
               av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
               outputsize = "full") 

##################################################################
########################### SP500 ###############################
#symbols = tq_exchange("NASDAQ")$symbol %>% sort()
# Getting data...
# 
# Error in open.connection(con, "rb") : 
#         cannot open the connection to 'https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=nasdaq&download=true'
# In addition: Warning message:
#         In open.connection(con, "rb") :
#         URL 'https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=nasdaq&download=true': Timeout of 60 seconds was reached

# # temp solution 1 - not working
# library(jsonlite)
# tmp <- jsonlite::fromJSON('https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=nasdaq&download=true')
# head(tmp$data$rows)
# 
# # temp solution 2 - ran too long (>10 min) and had to suspend it
# library(curl)
# library(readr)
# handle <- new_handle(verbose = TRUE) 
# temp_con <- curl("https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=nasdaq&download=true", handle = handle)
# temp_fix = read_csv(temp_con)
# dim(temp_fix); temp_fix %>% head()

# sp500 = tq_index("SP500") 
# symbols = sp500$symbol %>% sort()
# #symbols = tq_exchange("NASDAQ")$symbol %>% sort()
# symbols = c(symbols, "SPY") %>% sort()
# symbols = symbols[symbols %nin% "-"]  # remove "-" from result
# s = symbols 
# 
# symbolDf = data.frame(symbol = symbols) %>%
#         arrange(symbol) %>%
#         dplyr::mutate(id = row_number(),
#                       batch = dplyr::case_when(id >= 1 & id <= 100 ~ 1,
#                                                id >= 101 & id <= 200 ~ 2,
#                                                id >= 201 & id <= 300 ~ 3,
#                                                id >= 301 & id <= 400 ~ 4,
#                                                TRUE ~ 5))
# 
# dfList = vector(mode = "list", length = length(symbolDf$batch %>% unique()))
# 
# tic()
# for(i in 1:length(dfList)){
#         batch = symbolDf %>%
#                 dplyr::filter(batch == i) %>%
#                 .$symbol
#         df = batch %>%
#                 tq_get(get = "alphavantage", 
#                        av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
#                        outputsize = "full")
#         dfList[[i]] = df        
#         Sys.sleep(15)
# }
# toc()
# 
# df <- dfList %>% 
#         plyr::ldply() %>%
#         as.tibble()
# dim(df); head(df)
# 
# check = df %>%
#         group_by(symbol) %>%
#         summarise(n = n()) %>%
#         arrange(n)
# check %>% head()
# 
# symbols <- check %>%
#         dplyr::filter(n >500) %>%
#         arrange(symbol) %>%
#         .$symbol
# s = symbols 

##################################################################
########### data transformation #########################
# adjust for OHLC
df2 <- df %>%
        dplyr::mutate(adj_coefficient = adjusted_close / close,
                      adj_open = open * adj_coefficient,
                      adj_high = high * adj_coefficient,
                      adj_low = low * adj_coefficient)

# rename columns              
df3 <- df2 %>%
        dplyr::select(symbol, 
                      date = timestamp, 
                      open, 
                      high, 
                      low, 
                      close, 
                      volume, 
                      dividend_amount, 
                      split_coefficient, 
                      adj_open, 
                      adj_high, 
                      adj_low, 
                      adj_close = adjusted_close) %>%
        dplyr::arrange(symbol, date)

###########################################################################################################
# filter out symbols that have 200 or less trading days of data
exclude_symbols = df3 %>% group_by(symbol) %>% summarise(n = n()) %>% filter(n <= 200) %>% .$symbol

#subset_date = "1990-01-01"

# begin transformation
# use subset_date to get smaller dataset in exchange for faster processing
t0 <- df3 %>%        
        dplyr::filter(symbol %nin% exclude_symbols) %>%
        dplyr::filter(date >= subset_date) %>%
        dplyr::select(symbol,
                      date, 
                      open = adj_open,
                      high = adj_high,
                      low = adj_low,
                      close = adj_close,
                      volume) %>%
        dplyr::arrange(symbol, date)

# # add crypto
# crypto <- "BTC-USD" %>%
#         tq_get() %>%
#         dplyr::mutate(adj_coefficient = adjusted / close,
#                       adj_open = open * adj_coefficient,
#                       adj_high = high * adj_coefficient,
#                       adj_low = low * adj_coefficient) %>%
#         dplyr::select(symbol,
#                       date, 
#                       open = adj_open,
#                       high = adj_high,
#                       low = adj_low,
#                       close = adjusted,
#                       volume) %>%
#         dplyr::arrange(symbol, date)
# 
# t0 = rbind(t0, crypto)

############################
# macd
macd <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_mutate(select = close,
                  nFast = 12,
                  nSlow = 26,
                  nSig = 9,
                  maType = EMA,
                  mutate_fun = MACD) %>%
        dplyr::mutate(diff = macd - signal) %>%
        dplyr::mutate(diff_lag1 = lag(diff, 1),
                      diff_lag2 = lag(diff, 2),
                      diff_lag3 = lag(diff, 3),
                      diff_lag4 = lag(diff, 4),
                      diff_lag5 = lag(diff, 5),
                      diff_lag6 = lag(diff, 6),
                      diff_lag7 = lag(diff, 7),
                      diff_lag8 = lag(diff, 8),                      
                      flag = dplyr::case_when( diff >0 & 
                                                       diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 & diff_lag5 <0 & diff_lag6 <0 & diff_lag7 <0 & diff_lag8 <0 ~ 1,
                                               diff >0 & diff_lag1 >0 & 
                                                       diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 & diff_lag5 <0 & diff_lag6 <0 & diff_lag7 <0 & diff_lag8 <0 ~ 1,

                                               diff <0 & 
                                                       diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 & diff_lag5 >0 & diff_lag6 >0 & diff_lag7 >0 & diff_lag8 >0 ~ -1,
                                               diff <0 & diff_lag1 <0 & 
                                                       diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 & diff_lag5 >0 & diff_lag6 >0 & diff_lag7 >0 & diff_lag8 >0 ~ -1,
                                               TRUE ~ 0 ),
                      trend_dir = dplyr::case_when(diff > diff_lag1 &
                                                           diff_lag1 > diff_lag2 &
                                                           diff_lag2 > diff_lag3 &
                                                           diff_lag3 > diff_lag4 ~ 1,
                                                   diff < diff_lag1 &
                                                           diff_lag1 < diff_lag2 &
                                                           diff_lag2 < diff_lag3 &
                                                           diff_lag3 < diff_lag4 ~ -1,
                                                   TRUE ~ 0)
        ) %>%
        ungroup() %>%
        select(symbol, date, close, macd, signal, diff, flag, trend_dir) %>%
        arrange(symbol, date)

############################
# heikin ashi (smooth)
ha_transform <- function(df) {
        x = df %>%
                dplyr::select(date, open, high, low, close) %>%
                dplyr::arrange(date) %>%
                timetk::tk_xts() %>%
                heikin_ashi(output_as_df = TRUE) %>%
                dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"),
                              intraday_volatility = (close - open) / (high - low)) %>%
                as_tibble()
        return(x)
}

vector_of_symbols = unique(t0$symbol)

ha0 = lapply(1:length(vector_of_symbols), function(x){
        
        ha_transform(df = t0 %>% 
                             dplyr::filter(symbol == vector_of_symbols[x])) %>%
                dplyr::mutate(symbol = vector_of_symbols[x]) %>%
                dplyr::select(symbol, everything()) %>%
                dplyr::arrange(date) %>%
                dplyr::mutate(open_ema = pracma::movavg(open, n = 10, type = "e"),
                              high_ema = pracma::movavg(high, n = 10, type = "e"),
                              low_ema = pracma::movavg(low, n = 10, type = "e"),
                              close_ema = pracma::movavg(close, n = 10, type = "e"))
        
}) %>%
        plyr::ldply() %>%
        as.tibble() %>%
        select(symbol, 
               date, 
               open, 
               high, 
               low, 
               close,
               open_ema,
               high_ema,
               low_ema,
               close_ema,
               intraday_volatility) %>%
        arrange(symbol, date)

# join with t0 to get real 'close'
ha <- dplyr::inner_join(ha0, t0 %>% select(symbol, date, real_open = open, real_close = close), by = c("symbol", "date")) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(real_open_lag1 = lag(real_open, 1),
                      open_ema_lag1 = lag(open_ema, 1),
                      
                      real_close_lag1 = lag(real_close, 1),
                      real_close_lag2 = lag(real_close, 2),
                      real_close_lag3 = lag(real_close, 3),
                      real_close_lag4 = lag(real_close, 4),
                      real_close_lag5 = lag(real_close, 5),
                      real_close_lag6 = lag(real_close, 6),
                      real_close_lag7 = lag(real_close, 7),
                      real_close_lag8 = lag(real_close, 8),                      
                      
                      close_lag1 = lag(close, 1),
                      close_lag2 = lag(close, 2),
                      close_lag3 = lag(close, 3),
                      close_lag4 = lag(close, 4),
                      close_lag5 = lag(close, 5),
                      close_lag6 = lag(close, 6),
                      close_lag7 = lag(close, 7),
                      close_lag8 = lag(close, 8),                      
                      
                      close_ema_lag1 = lag(close_ema, 1),
                      close_ema_lag2 = lag(close_ema, 2),
                      close_ema_lag3 = lag(close_ema, 3),
                      close_ema_lag4 = lag(close_ema, 4),
                      close_ema_lag5 = lag(close_ema, 5),
                      close_ema_lag6 = lag(close_ema, 6),
                      close_ema_lag7 = lag(close_ema, 7),
                      close_ema_lag8 = lag(close_ema, 8)) %>%
        # real close vs close ema (heikin ashi transformed)
        dplyr::mutate(real_flag = case_when(real_close > close_ema & 
                                                    real_close_lag1 < close_ema_lag1 &
                                                    real_close_lag2 < close_ema_lag2 &
                                                    real_close_lag3 < close_ema_lag3 &
                                                    real_close_lag4 < close_ema_lag4 &
                                                    real_close_lag5 < close_ema_lag5 &
                                                    real_close_lag6 < close_ema_lag6 &
                                                    real_close_lag7 < close_ema_lag7 &
                                                    real_close_lag8 < close_ema_lag8 ~ 1,
                                            real_close > close_ema & real_close_lag1 > close_ema_lag1 &
                                                    real_close_lag2 < close_ema_lag2 &
                                                    real_close_lag3 < close_ema_lag3 &
                                                    real_close_lag4 < close_ema_lag4 &
                                                    real_close_lag5 < close_ema_lag5 &
                                                    real_close_lag6 < close_ema_lag6 &
                                                    real_close_lag7 < close_ema_lag7 &
                                                    real_close_lag8 < close_ema_lag8 ~ 1,
                                            real_close > close_ema & real_close_lag1 > close_ema_lag1 & real_close_lag2 > close_ema_lag2 &
                                                    real_close_lag3 < close_ema_lag3 &
                                                    real_close_lag4 < close_ema_lag4 &
                                                    real_close_lag5 < close_ema_lag5 &
                                                    real_close_lag6 < close_ema_lag6 &
                                                    real_close_lag7 < close_ema_lag7 &
                                                    real_close_lag8 < close_ema_lag8 ~ 1,
                                            real_close > close_ema & real_close_lag1 > close_ema_lag1 & real_close_lag2 > close_ema_lag2 & real_close_lag3 > close_ema_lag3 &
                                                    real_close_lag4 < close_ema_lag4 &
                                                    real_close_lag5 < close_ema_lag5 &
                                                    real_close_lag6 < close_ema_lag6 &
                                                    real_close_lag7 < close_ema_lag7 &
                                                    real_close_lag8 < close_ema_lag8 ~ 1,                                               
                                            real_close < close_ema & 
                                                    real_close_lag1 > close_ema_lag1 &
                                                    real_close_lag2 > close_ema_lag2 &
                                                    real_close_lag3 > close_ema_lag3 &
                                                    real_close_lag4 > close_ema_lag4 &
                                                    real_close_lag5 > close_ema_lag5 &
                                                    real_close_lag6 > close_ema_lag6 &
                                                    real_close_lag7 > close_ema_lag7 &
                                                    real_close_lag8 > close_ema_lag8 ~ -1,
                                            real_close < close_ema & real_close_lag1 < close_ema_lag1 & 
                                                    real_close_lag2 > close_ema_lag2 &
                                                    real_close_lag3 > close_ema_lag3 &
                                                    real_close_lag4 > close_ema_lag4 &
                                                    real_close_lag5 > close_ema_lag5 &
                                                    real_close_lag6 > close_ema_lag6 &
                                                    real_close_lag7 > close_ema_lag7 &
                                                    real_close_lag8 > close_ema_lag8 ~ -1,
                                            real_close < close_ema & real_close_lag1 < close_ema_lag1 & real_close_lag2 < close_ema_lag2 &
                                                    real_close_lag3 > close_ema_lag3 &
                                                    real_close_lag4 > close_ema_lag4 &
                                                    real_close_lag5 > close_ema_lag5 &
                                                    real_close_lag6 > close_ema_lag6 &
                                                    real_close_lag7 > close_ema_lag7 &
                                                    real_close_lag8 > close_ema_lag8 ~ -1, 
                                            real_close < close_ema & real_close_lag1 < close_ema_lag1 & real_close_lag2 < close_ema_lag2 & real_close_lag3 < close_ema_lag3 &
                                                    real_close_lag4 > close_ema_lag4 &
                                                    real_close_lag5 > close_ema_lag5 &
                                                    real_close_lag6 > close_ema_lag6 &
                                                    real_close_lag7 > close_ema_lag7 &
                                                    real_close_lag8 > close_ema_lag8 ~ -1,
                                            TRUE ~ 0),
                      real2_flag = case_when(real_open > open_ema & real_open > close_ema &
                                                     real_close > open_ema & real_close > close_ema &
                                                     real_open_lag1 > open_ema_lag1 & real_open_lag1 > close_ema_lag1 &
                                                     real_close_lag1 > open_ema_lag1 & real_close_lag1 > close_ema_lag1 ~ 1,
                                             real_open < open_ema & real_open < close_ema &
                                                     real_close < open_ema & real_close < close_ema &
                                                     real_open_lag1 < open_ema_lag1 & real_open_lag1 < close_ema_lag1 &
                                                     real_close_lag1 < open_ema_lag1 & real_close_lag1 < close_ema_lag1 ~ -1,
                                             TRUE ~ 0),
                      smooth_flag = case_when(close > close_ema & 
                                                      close_lag1 < close_ema_lag1 &
                                                      close_lag2 < close_ema_lag2 &
                                                      close_lag3 < close_ema_lag3 &
                                                      close_lag4 < close_ema_lag4 &
                                                      close_lag5 < close_ema_lag5 &
                                                      close_lag6 < close_ema_lag6 &
                                                      close_lag7 < close_ema_lag7 &
                                                      close_lag8 < close_ema_lag8 ~ 1,
                                              close > close_ema & close_lag1 > close_ema_lag1 & 
                                                      close_lag2 < close_ema_lag2 &
                                                      close_lag3 < close_ema_lag3 &
                                                      close_lag4 < close_ema_lag4 &
                                                      close_lag5 < close_ema_lag5 &
                                                      close_lag6 < close_ema_lag6 &
                                                      close_lag7 < close_ema_lag7 &
                                                      close_lag8 < close_ema_lag8 ~ 1,
                                              close > close_ema & close_lag1 > close_ema_lag1 & close_lag2 > close_ema_lag2 & 
                                                      close_lag3 < close_ema_lag3 &
                                                      close_lag4 < close_ema_lag4 &
                                                      close_lag5 < close_ema_lag5 &
                                                      close_lag6 < close_ema_lag6 &
                                                      close_lag7 < close_ema_lag7 &
                                                      close_lag8 < close_ema_lag8 ~ 1,                                       
                                              close > close_ema & close_lag1 > close_ema_lag1 & close_lag2 > close_ema_lag2 & close_lag3 > close_ema_lag3 &
                                                      close_lag4 < close_ema_lag4 &
                                                      close_lag5 < close_ema_lag5 &
                                                      close_lag6 < close_ema_lag6 &
                                                      close_lag7 < close_ema_lag7 &
                                                      close_lag8 < close_ema_lag8 ~ 1,                                       
                                              close < close_ema & 
                                                      close_lag1 > close_ema_lag1 &
                                                      close_lag2 > close_ema_lag2 &
                                                      close_lag3 > close_ema_lag3 &
                                                      close_lag4 > close_ema_lag4 &
                                                      close_lag5 > close_ema_lag5 &
                                                      close_lag6 > close_ema_lag6 &
                                                      close_lag7 > close_ema_lag7 &
                                                      close_lag8 > close_ema_lag8 ~ -1,
                                              close < close_ema & close_lag1 < close_ema_lag1 &
                                                      close_lag2 > close_ema_lag2 &
                                                      close_lag3 > close_ema_lag3 &
                                                      close_lag4 > close_ema_lag4 &
                                                      close_lag5 > close_ema_lag5 &
                                                      close_lag6 > close_ema_lag6 &
                                                      close_lag7 > close_ema_lag7 &
                                                      close_lag8 > close_ema_lag8 ~ -1,
                                              close < close_ema & close_lag1 < close_ema_lag1 & close_lag2 < close_ema_lag2 &                                                      
                                                      close_lag3 > close_ema_lag3 &
                                                      close_lag4 > close_ema_lag4 &
                                                      close_lag5 > close_ema_lag5 &
                                                      close_lag6 > close_ema_lag6 &
                                                      close_lag7 > close_ema_lag7 &
                                                      close_lag8 > close_ema_lag8 ~ -1,        
                                              close < close_ema & close_lag1 < close_ema_lag1 & close_lag2 < close_ema_lag2 & close_lag3 < close_ema_lag3 &
                                                      close_lag4 > close_ema_lag4 &
                                                      close_lag5 > close_ema_lag5 &
                                                      close_lag6 > close_ema_lag6 &
                                                      close_lag7 > close_ema_lag7 &
                                                      close_lag8 > close_ema_lag8 ~ -1,
                                              TRUE ~ 0)
        ) %>%
        ungroup() %>%
        select(symbol, 
               date, 
               open_ha = open, 
               high_ha = high, 
               low_ha = low, 
               close_ha = close,
               real_close,
               open_ha_ema = open_ema,
               high_ha_ema = high_ema,
               low_ha_ema = low_ema,
               close_ha_ema = close_ema,
               intraday_volatility,
               real_flag,
               real2_flag,
               smooth_flag) %>%
        arrange(symbol, date)

############################
# # ADX
# adx <- t0 %>%
#         arrange(symbol, date) %>%
#         group_by(symbol) %>%
#         tq_transmute(select = c("high", "low", "close"),
#                      n = 14, 
#                      mutate_fun = ADX,
#                      col_rename = c("dmi_p", "dmi_n", "dx", "adx")) %>%
#         ungroup() %>%
#         arrange(symbol, date) %>%
#         dplyr::mutate(dmi_p_cross_above_yn = dplyr::case_when(dmi_p > dmi_n ~ 1, TRUE ~ 0),
#                       dmi_n_cross_above_yn = dplyr::case_when(dmi_n > dmi_p ~ 1, TRUE ~ 0)) %>%        
#         ungroup()
# 

############################
# Donchian Channel
dcc <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low"),
                     n = 10, 
                     mutate_fun = DonchianChannel,
                     include.lag = TRUE,
                     col_rename = c("dcc_high", "dcc_mid", "dcc_low")) %>%
        ungroup() %>%
        inner_join(t0 %>% 
                           select(symbol, date, close), 
                   by = c("symbol", "date")) %>%
        group_by(symbol) %>%
        mutate(dcc_mid_lag1 = lag(dcc_mid, 1),
               dcc_mid_lag2 = lag(dcc_mid, 2),
               dcc_mid_lag3 = lag(dcc_mid, 3),
               dcc_mid_lag4 = lag(dcc_mid, 4),
               dcc_mid_lag5 = lag(dcc_mid, 5),
               dcc_mid_lag6 = lag(dcc_mid, 6),
               dcc_mid_lag7 = lag(dcc_mid, 7),
               dcc_mid_lag8 = lag(dcc_mid, 8),
               
               close_lag1 = lag(close, 1),
               close_lag2 = lag(close, 2),
               close_lag3 = lag(close, 3),
               close_lag4 = lag(close, 4),
               close_lag5 = lag(close, 5),
               close_lag6 = lag(close, 6),
               close_lag7 = lag(close, 7),
               close_lag8 = lag(close, 8)) %>%
        mutate(flag = case_when(close > dcc_mid & 
                                        close_lag1 < dcc_mid_lag1 &
                                        close_lag2 < dcc_mid_lag2 &
                                        close_lag3 < dcc_mid_lag3 &
                                        close_lag4 < dcc_mid_lag4 &
                                        close_lag5 < dcc_mid_lag5 &
                                        close_lag6 < dcc_mid_lag6 &
                                        close_lag7 < dcc_mid_lag7 &
                                        close_lag8 < dcc_mid_lag8 ~ 1,
                                close > dcc_mid & close_lag1 > dcc_mid_lag1 &
                                        close_lag2 < dcc_mid_lag2 &
                                        close_lag3 < dcc_mid_lag3 &
                                        close_lag4 < dcc_mid_lag4 &
                                        close_lag5 < dcc_mid_lag5 &
                                        close_lag6 < dcc_mid_lag6 &
                                        close_lag7 < dcc_mid_lag7 &
                                        close_lag8 < dcc_mid_lag8 ~ 1,
                                
                                close < dcc_mid &
                                        close_lag1 > dcc_mid_lag1 &
                                        close_lag2 > dcc_mid_lag2 &
                                        close_lag3 > dcc_mid_lag3 &
                                        close_lag4 > dcc_mid_lag4 &
                                        close_lag5 > dcc_mid_lag5 &
                                        close_lag6 > dcc_mid_lag6 &
                                        close_lag7 > dcc_mid_lag7 &
                                        close_lag8 > dcc_mid_lag8 ~ -1,
                                close < dcc_mid & close_lag1 < dcc_mid_lag1 &
                                        close_lag2 > dcc_mid_lag2 &
                                        close_lag3 > dcc_mid_lag3 &
                                        close_lag4 > dcc_mid_lag4 &
                                        close_lag5 > dcc_mid_lag5 &
                                        close_lag6 > dcc_mid_lag6 &
                                        close_lag7 > dcc_mid_lag7 &
                                        close_lag8 > dcc_mid_lag8 ~ -1,
                                TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, date, close, dcc_high, dcc_mid, dcc_low, flag) %>%
        arrange(symbol, date)

############################
# rsi
rsi <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = RSI) %>%
        dplyr::mutate(rsi_lag1 = dplyr::lag(rsi, 1),
                      rsi_lag2 = dplyr::lag(rsi, 2)) %>%
        dplyr::mutate(rsi_oversold_yn = dplyr::case_when(rsi < 30 &
                                                                 rsi > rsi_lag1 &
                                                                 rsi_lag1 > rsi_lag2 ~ 1,
                                                         rsi > 30 &
                                                                 rsi_lag1 <= 30 &
                                                                 rsi_lag2 <= 30 ~ 1,
                                                         rsi > 30 &
                                                                 rsi > rsi_lag1 &
                                                                 rsi_lag1 > 30 &
                                                                 rsi_lag2 <= 30 ~ 1,
                                                         TRUE ~ 0),
                      rsi_overbought_yn = dplyr::case_when(rsi > 70 &
                                                                   rsi < rsi_lag1 &
                                                                   rsi < rsi_lag2 ~ 1,
                                                           TRUE ~ 0),
                      # trend dir
                      rsi_trend_dir = case_when(rsi > rsi_lag1 & rsi_lag1 > rsi_lag2 ~ 1, 
                                                rsi < rsi_lag1 & rsi_lag1 < rsi_lag2 ~ -1,
                                                TRUE ~ 0),
                      # oversold
                      rsi_oversold_ma = pracma::movavg(rsi_oversold_yn, n = 5, type = "s"),
                      rsi_oversold_flag = case_when(rsi_oversold_ma >0 & rsi < 50 ~ 1, TRUE ~ 0),
                      rsi_oversold_flag = case_when((rsi_oversold_flag == 1 & rsi_trend_dir == 1) | rsi_oversold_yn == 1 ~ 1, TRUE ~ 0),
                      # overbought
                      rsi_overbought_ma = pracma::movavg(rsi_overbought_yn, n = 5, type = "s"),
                      rsi_overbought_flag = case_when(rsi_overbought_ma >0 & rsi > 50 ~ 1, TRUE ~ 0),
                      rsi_overbought_flag = case_when((rsi_overbought_flag == 1 & rsi_trend_dir == -1) | rsi_overbought_yn == 1 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        arrange(symbol, date)

############################
# cci
cci <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = CCI) %>%
        dplyr::mutate(cci_lag1 = dplyr::lag(cci, 1),
                      cci_lag2 = dplyr::lag(cci, 2)) %>%
        dplyr::mutate(cci_oversold_yn = dplyr::case_when(cci < -100 &
                                                                 cci > cci_lag1 &
                                                                 cci_lag1 > cci_lag2 ~ 1,
                                                         cci > -100 &
                                                                 cci_lag1 <= -100 &
                                                                 cci_lag2 <= -100 ~ 1,
                                                         cci > -100 &
                                                                 cci > cci_lag1 &
                                                                 cci_lag1 > -100 &
                                                                 cci_lag2 <= -100 ~ 1,
                                                         TRUE ~ 0),
                      cci_overbought_yn = dplyr::case_when(cci > 100 &
                                                                   cci < cci_lag1 &
                                                                   cci < cci_lag2 ~ 1,
                                                           TRUE ~ 0),
                      # trend dir
                      cci_trend_dir = case_when(cci > cci_lag1 & cci_lag1 > cci_lag2 ~ 1, 
                                                cci < cci_lag1 & cci_lag1 < cci_lag2 ~ -1,
                                                TRUE ~ 0),
                      # oversold
                      cci_oversold_ma = pracma::movavg(cci_oversold_yn, n = 5, type = "s"),
                      cci_oversold_flag = dplyr::case_when(cci_oversold_ma >0 & cci < 100 ~ 1, TRUE ~ 0),
                      cci_oversold_flag = case_when((cci_oversold_flag == 1 & cci_trend_dir == 1) | cci_oversold_yn == 1 ~ 1, TRUE ~ 0),
                      # overbought
                      cci_overbought_ma = pracma::movavg(cci_overbought_yn, n = 5, type = "s"),
                      cci_overbought_flag = dplyr::case_when(cci_overbought_ma >0 & cci > -100 ~ 1, TRUE ~ 0),
                      cci_overbought_flag = case_when((cci_overbought_flag == 1 & cci_trend_dir == -1) | cci_overbought_yn == 1 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        arrange(symbol, date)

############################
# obv
obv <- t0 %>%
        group_by(symbol) %>%
        tq_mutate_xy(close,
                     volume,
                     mutate_fun = OBV) %>%
        dplyr::mutate(close_lag1 = lag(close, 1),
                      close_lag2 = lag(close, 2),
                      volume_lag1 = lag(volume, 1),
                      volume_lag2 = lag(volume, 2)) %>%
        dplyr::mutate(volume_inconsistency_alert = case_when(close > close_lag1 & close_lag1 > close_lag2 &
                                                                     (volume < volume_lag1 | volume < volume_lag2) ~ "bullish inconsistency",
                                                             close < close_lag1 & close_lag1 < close_lag2 &
                                                                     (volume < volume_lag1 | volume < volume_lag2) ~ "bearish inconsistency",
                                                             TRUE ~ "neutral")) %>%
        dplyr::mutate(obv_lag4 = lag(obv, 4),
                      obv_diff = obv - obv_lag4,
                      obv_diff_lag1 = lag(obv_diff, 1),
                      obv_diff_lag2 = lag(obv_diff, 2),
                      obv_diff_lag3 = lag(obv_diff, 3),
                      obv_flag = case_when(obv_diff >0 & obv_diff_lag1 >0 & obv_diff_lag2 >0 & obv_diff_lag3 >0 ~ 1, 
                                           TRUE ~ 0)) %>%
        select(symbol, date, close, volume, obv, volume_inconsistency_alert, obv_flag) %>%
        ungroup()

############################
# vwap
# vwap <- t0 %>%
#         dplyr::mutate(typical_price = (high + low + close)/3) %>%
#         arrange(symbol, date) %>%
#         group_by(symbol) %>%
#         tq_mutate_xy(typical_price,
#                      volume,
#                      mutate_fun = VWAP,
#                      col_rename = "vwap") %>%
#         dplyr::mutate(close_lag1 = dplyr::lag(close, 1),
#                       close_lag2 = dplyr::lag(close, 2),
#                       close_lag3 = dplyr::lag(close, 3),
#                       vwap_lag1 = dplyr::lag(vwap, 1),
#                       vwap_lag2 = dplyr::lag(vwap, 2),
#                       vwap_lag3 = dplyr::lag(vwap, 3),
#                       flag = dplyr::case_when( (close >vwap) & (close_lag1 <vwap_lag1) & (close_lag2 <vwap_lag2) & (close_lag3 <vwap_lag3) ~ 1,
#                                                (close >vwap) & (close_lag1 >vwap_lag1) & (close_lag2 <vwap_lag2) & (close_lag3 <vwap_lag3) ~ 1,
#                                                
#                                                (close <vwap) & (close_lag1 >vwap_lag1) & (close_lag2 >vwap_lag2) & (close_lag3 >vwap_lag3) ~ -1,
#                                                (close <vwap) & (close_lag1 <vwap_lag1) & (close_lag2 >vwap_lag2) & (close_lag3 >vwap_lag3) ~ -1,
#                                                TRUE ~ 0 )) %>%
#         ungroup()

############################
# evwma
evwma <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_mutate_xy(close,
                     volume,
                     mutate_fun = EVWMA,
                     col_rename = "evwma") %>%
        mutate(evwma_lag1 = lag(evwma, 1),
               evwma_lag2 = lag(evwma, 2),
               evwma_lag3 = lag(evwma, 3),
               evwma_lag4 = lag(evwma, 4),
               evwma_lag5 = lag(evwma, 5),
               evwma_lag6 = lag(evwma, 6),
               evwma_lag7 = lag(evwma, 7),
               evwma_lag8 = lag(evwma, 8),
               
               close_lag1 = lag(close, 1),
               close_lag2 = lag(close, 2),
               close_lag3 = lag(close, 3),
               close_lag4 = lag(close, 4),
               close_lag5 = lag(close, 5),
               close_lag6 = lag(close, 6),
               close_lag7 = lag(close, 7),
               close_lag8 = lag(close, 8)) %>%
        mutate(flag = case_when(close > evwma & 
                                        close_lag1 < evwma_lag1 &
                                        close_lag2 < evwma_lag2 &
                                        close_lag3 < evwma_lag3 &
                                        close_lag4 < evwma_lag4 &
                                        close_lag5 < evwma_lag5 &
                                        close_lag6 < evwma_lag6 &
                                        close_lag7 < evwma_lag7 &
                                        close_lag8 < evwma_lag8 ~ 1,
                                close > evwma & close_lag1 > evwma_lag1 &
                                        close_lag2 < evwma_lag2 &
                                        close_lag3 < evwma_lag3 &
                                        close_lag4 < evwma_lag4 &
                                        close_lag5 < evwma_lag5 &
                                        close_lag6 < evwma_lag6 &
                                        close_lag7 < evwma_lag7 &
                                        close_lag8 < evwma_lag8 ~ 1,

                                close < evwma &
                                        close_lag1 > evwma_lag1 &
                                        close_lag2 > evwma_lag2 &
                                        close_lag3 > evwma_lag3 &
                                        close_lag4 > evwma_lag4 &
                                        close_lag5 > evwma_lag5 &
                                        close_lag6 > evwma_lag6 &
                                        close_lag7 > evwma_lag7 &
                                        close_lag8 > evwma_lag8 ~ -1,
                                close < evwma & close_lag1 < evwma_lag1 &
                                        close_lag2 > evwma_lag2 &
                                        close_lag3 > evwma_lag3 &
                                        close_lag4 > evwma_lag4 &
                                        close_lag5 > evwma_lag5 &
                                        close_lag6 > evwma_lag6 &
                                        close_lag7 > evwma_lag7 &
                                        close_lag8 > evwma_lag8 ~ -1,
                                TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, date, close, volume, evwma, flag)

############################
# overnight
overnight <- t0 %>%
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = lag(close, 1)) %>%
        dplyr::mutate(temp_flag = dplyr::case_when((open > close_lag1) & (close > open) ~ 1,
                                                   (open < close_lag1) & (close < open) ~ -1,
                                                   TRUE ~ 0),
                      temp_flag_lag1 = lag(temp_flag, 1),
                      flag = case_when(temp_flag == 1 & temp_flag_lag1 == 1 ~ 1,
                                       temp_flag == -1 & temp_flag_lag1 == -1 ~ -1,
                                       TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, date, close, flag)

############################
# sma5, sma8, sma13, sma50, sma200, ema10, ema30, ema100, zlema, proxy_flag
ma <- t0 %>%
        # very important for calculating SMA and EMA by having asec(date)
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        ############################### SMA, EMA ###############################
dplyr::mutate(sma5 = pracma::movavg(close, n = 5, type = "s"),
              sma8 = pracma::movavg(close, n = 8, type = "s"),
              sma13 = pracma::movavg(close, n = 13, type = "s"),
              
              sma50 = pracma::movavg(close, n = 50, type = "s"),
              sma200 = pracma::movavg(close, n = 200, type = "s"), 
              
              ema5 = pracma::movavg(close, n = 5, type = "e"),  
              ema20 = pracma::movavg(close, n = 20, type = "e"),                
              
              close_lead1 = dplyr::lead(close, 1),
              sma5_lead1 = dplyr::lead(sma5, 1)) %>%
        ############################### Zero Lag EMA ###############################
tq_mutate(select = close,
          mutate_fun = ZLEMA,
          col_rename = "zlema") %>%
        ungroup %>%
        ############################### a new target proxy variable ###############################
arrange(symbol, desc(date)) %>%
        group_by(symbol) %>%
        dplyr::mutate(index = row_number()) %>%
        dplyr::mutate(proxy_flag = dplyr::case_when(close_lead1 > sma5_lead1 ~1, TRUE~0),
                      proxy_vote = pracma::movavg(proxy_flag, n = 5, type = "s"),
                      proxy_flag = dplyr::case_when(proxy_vote >= 0.8 ~1, 
                                                    proxy_vote <= 0.2 ~-1,
                                                    TRUE ~0)) %>%
        ungroup %>%
        arrange(symbol, date) %>%
        dplyr::mutate(temp_flag = index>5,
                      proxy_vote = dplyr::case_when(temp_flag == 1 ~ proxy_vote),
                      proxy_flag = dplyr::case_when(temp_flag == 1 ~ proxy_flag)) %>%
        dplyr::select(-close_lead1, -sma5_lead1, -index) %>%
        ############################### sma5 flag ###############################        
arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = dplyr::lag(close, 1),
                      close_lag2 = dplyr::lag(close, 2),
                      close_lag3 = dplyr::lag(close, 3),
                      close_lag4 = dplyr::lag(close, 4),
                      
                      sma5_lag1 = dplyr::lag(sma5, 1),
                      sma5_lag2 = dplyr::lag(sma5, 2),
                      sma5_lag3 = dplyr::lag(sma5, 3),
                      sma5_lag4 = dplyr::lag(sma5, 4),
                      
                      ema5_lag1 = dplyr::lag(ema5, 1),
                      ema5_lag2 = dplyr::lag(ema5, 2),
                      ema5_lag3 = dplyr::lag(ema5, 3),
                      ema5_lag4 = dplyr::lag(ema5, 4),
                      
                      ema20_lag1 = dplyr::lag(ema20, 1),
                      ema20_lag2 = dplyr::lag(ema20, 2),
                      ema20_lag3 = dplyr::lag(ema20, 3),
                      ema20_lag4 = dplyr::lag(ema20, 4),
                      
                      sma5_flag = dplyr::case_when( (close >sma5) & (close_lag1 <sma5_lag1) & (close_lag2 <sma5_lag2) & (close_lag3 <sma5_lag3) & (close_lag4 <sma5_lag4) ~ 1, 
                                                    (close <sma5) & (close_lag1 >sma5_lag1) & (close_lag2 >sma5_lag2) & (close_lag3 >sma5_lag3) & (close_lag4 >sma5_lag4) ~ -1, 
                                                    TRUE ~ 0 ),
                      
                      ema5_flag = dplyr::case_when( (ema5 >ema20) & (ema5_lag1 <ema20_lag1) & (ema5_lag2 <ema20_lag2) & (ema5_lag3 <ema20_lag3) & (ema5_lag4 <ema20_lag4) ~ 1, 
                                                    (ema5 <ema20) & (ema5_lag1 >ema20_lag1) & (ema5_lag2 >ema20_lag2) & (ema5_lag3 >ema20_lag3) & (ema5_lag4 >ema20_lag4) ~ -1, 
                                                    TRUE ~ 0 )
        ) %>%
        dplyr::select(-close_lag1, -close_lag2, -close_lag3, -close_lag4,
                      -sma5_lag1, -sma5_lag2, -sma5_lag3, -sma5_lag4,
                      -ema5_lag1, -ema5_lag2, -ema5_lag3, -ema5_lag4,
                      -ema20_lag1, -ema20_lag2, -ema20_lag3, -ema20_lag4
        ) %>%
        ungroup

############################
# ATR, Chandelier Exit
atr14 <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     n = 14, 
                     mutate_fun = ATR,
                     col_rename = c("tr", "atr", "trueHigh", "trueLow")) %>%
        ungroup() %>%
        select(symbol, date, atr)

atr0 <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     #n = 22, 
                     n = 14,
                     mutate_fun = ATR,
                     col_rename = c("tr", "atr_ce", "trueHigh", "trueLow")) %>%
        ungroup()

higherH <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = "high",
                     mutate_fun = rollapply,
                     #width = 22,
                     width = 14,
                     FUN = max,
                     by.column = FALSE,
                     col_rename = "higherHigh") %>%
        ungroup()

lowerL <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = "low",
                     mutate_fun = rollapply,
                     #width = 22,
                     width = 14,
                     FUN = min,
                     by.column = FALSE,
                     col_rename = "lowerLow") %>%
        ungroup()

atr <- t0 %>%        
        dplyr::inner_join(atr0, by = c("symbol", "date")) %>%
        dplyr::inner_join(higherH, by = c("symbol", "date")) %>%
        dplyr::inner_join(lowerL, by = c("symbol", "date")) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(chanExit_long = higherHigh - atr_ce *3,
                      chanExit_short = lowerLow + atr_ce *3) %>%
        dplyr::inner_join(atr14, by = c("symbol", "date")) %>%
        ungroup()

chanExit <- atr %>%
        select(symbol, date, close, chanExit_long, chanExit_short) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = lag(close, 1),
                      close_lag2 = lag(close, 2),
                      close_lag3 = lag(close, 3),
                      close_lag4 = lag(close, 4),
                      close_lag5 = lag(close, 5),
                      close_lag6 = lag(close, 6),
                      close_lag7 = lag(close, 7),
                      close_lag8 = lag(close, 8),
                      
                      chanExit_long_lag1 = lag(chanExit_long, 1),
                      chanExit_long_lag2 = lag(chanExit_long, 2),
                      chanExit_long_lag3 = lag(chanExit_long, 3),
                      chanExit_long_lag4 = lag(chanExit_long, 4),
                      chanExit_long_lag5 = lag(chanExit_long, 5),
                      chanExit_long_lag6 = lag(chanExit_long, 6),
                      chanExit_long_lag7 = lag(chanExit_long, 7),
                      chanExit_long_lag8 = lag(chanExit_long, 8),
                      
                      chanExit_short_lag1 = lag(chanExit_short, 1),
                      chanExit_short_lag2 = lag(chanExit_short, 2),
                      chanExit_short_lag3 = lag(chanExit_short, 3),
                      chanExit_short_lag4 = lag(chanExit_short, 4),
                      chanExit_short_lag5 = lag(chanExit_short, 5),
                      chanExit_short_lag6 = lag(chanExit_short, 6),
                      chanExit_short_lag7 = lag(chanExit_short, 7),
                      chanExit_short_lag8 = lag(chanExit_short, 8)) %>%
        mutate(ce_long_dip_flag = case_when(close < chanExit_long & 
                                                    close_lag1 > chanExit_long_lag1 &
                                                    close_lag2 > chanExit_long_lag2 &
                                                    close_lag3 > chanExit_long_lag3 &
                                                    close_lag4 > chanExit_long_lag4 &
                                                    close_lag5 > chanExit_long_lag5 &
                                                    close_lag6 > chanExit_long_lag6 &
                                                    close_lag7 > chanExit_long_lag7 &
                                                    close_lag8 > chanExit_long_lag8 ~ 1,
                                            close < chanExit_long & close_lag1 < chanExit_long_lag1 &
                                                    close_lag2 > chanExit_long_lag2 &
                                                    close_lag3 > chanExit_long_lag3 &
                                                    close_lag4 > chanExit_long_lag4 &
                                                    close_lag5 > chanExit_long_lag5 &
                                                    close_lag6 > chanExit_long_lag6 &
                                                    close_lag7 > chanExit_long_lag7 &
                                                    close_lag8 > chanExit_long_lag8 ~ 1,
                                            TRUE ~ 0),
               ce_short_spike_flag = case_when(close > chanExit_short &
                                                       close_lag1 < chanExit_short_lag1 &
                                                       close_lag2 < chanExit_short_lag2 &
                                                       close_lag3 < chanExit_short_lag3 &
                                                       close_lag4 < chanExit_short_lag4 &
                                                       close_lag5 < chanExit_short_lag5 &
                                                       close_lag6 < chanExit_short_lag6 &
                                                       close_lag7 < chanExit_short_lag7 &
                                                       close_lag8 < chanExit_short_lag8 ~ 1,
                                               close > chanExit_short & close_lag1 > chanExit_short_lag1 &
                                                       close_lag2 < chanExit_short_lag2 &
                                                       close_lag3 < chanExit_short_lag3 &
                                                       close_lag4 < chanExit_short_lag4 &
                                                       close_lag5 < chanExit_short_lag5 &
                                                       close_lag6 < chanExit_short_lag6 &
                                                       close_lag7 < chanExit_short_lag7 &
                                                       close_lag8 < chanExit_short_lag8 ~ 1,
                                               TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, date, close, chanExit_long, chanExit_short, ce_long_dip_flag, ce_short_spike_flag) %>%
        arrange(symbol, date)

############################
# candlestick patterns
vector_of_symbols = unique(t0$symbol)

csp <- lapply(1:length(vector_of_symbols), function(x) candle_stick_pattern(t0 %>% filter(symbol == vector_of_symbols[x]))) %>%
        plyr::ldply() %>%
        dplyr::mutate(
                # candle stick signal
                candle_stick_signal = case_when(bullish_candle == 1 ~ 1,
                                                bearish_candle == 1 ~ -1,
                                                TRUE ~ 0),
                # candle stick pattern
                candle_stick_pattern = case_when( up_trend == 1 & candle_stick_signal == 1 ~ 1,
                                                  down_trend == 1 & candle_stick_signal == -1 ~ -1,
                                                  TRUE ~ 0 )) %>%
        arrange(symbol, date)

names(csp) = c("symbol", "date", paste0("csp_", names(csp)[names(csp) %nin% c("symbol", "date")]))

############################
# traffic light
traffic_light <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        mutate(close_lag4 = lag(close, 4),
               percent_change_lag4_day = (close - close_lag4)/ close_lag4,
               percent_change_lag4_day_lag1 = lag(percent_change_lag4_day, 1),
               percent_change_lag4_day_lag2 = lag(percent_change_lag4_day, 2),
               percent_change_lag4_day_lag3 = lag(percent_change_lag4_day, 3),
               
               red_flag = case_when(percent_change_lag4_day <0 &
                                            percent_change_lag4_day_lag1 <0 &
                                            percent_change_lag4_day_lag2 <0 &
                                            percent_change_lag4_day_lag3 <0 ~ 1,
                                    TRUE ~ 0),
               green_flag = case_when(percent_change_lag4_day >0 &
                                              percent_change_lag4_day_lag1 >0 &
                                              percent_change_lag4_day_lag2 >0 &
                                              percent_change_lag4_day_lag3 >0 ~ 1,
                                      TRUE ~ 0)) %>%
        ungroup() %>%
        select(symbol, date, close, red_flag, green_flag) %>%
        arrange(symbol, date)

############################
# demark9
demark_query = "
with sub
as (
        select symbol, date, close
        from t0
),

sub2
as (
        select symbol, date, close
        , lag(close, 4) over(partition by symbol order by date) as close_4
        from sub
),

sub3
as (
        select symbol, date, close
        , case when close < close_4 then -1
                when close > close_4 then 1
                else 0 
                end as setup_counter
        from sub2
),

sub4
as (
        select symbol, date, close, setup_counter
        , sum(setup_counter) over(partition by symbol order by date ROWS BETWEEN 8 preceding AND 0 preceding) as magic9
        from sub3
)

select symbol
, date
, close
, setup_counter
, magic9
, case when magic9 = 9 then -1 
        when magic9 = -9 then 1
        else 0 
        end as flag
from sub4
order by symbol, date
"

demark0 = sqldf(glue(demark_query)) 

demark_entry = demark0 %>%
        arrange(symbol, date) %>%
        # add is_demark_entry_yn flag, i.e., has demark entry signal been flagged in past n days?
        group_by(symbol) %>%
        tq_transmute(select = "flag",
                     mutate_fun = rollapply,
                     width = 12,
                     FUN = max,
                     by.column = FALSE,
                     col_rename = "is_demark_entry_yn") %>%
        ungroup()

demark_exit = demark0 %>%
        arrange(symbol, date) %>%
        # add is_demark_exit_yn flag, i.e., has demark exit signal been flagged in past n days?
        group_by(symbol) %>%
        tq_transmute(select = "flag",
                     mutate_fun = rollapply,
                     width = 12,
                     FUN = min,
                     by.column = FALSE,
                     col_rename = "is_demark_exit_yn") %>%
        ungroup()

demark <- demark0 %>%
        inner_join(demark_entry, by = c("symbol", "date")) %>%
        inner_join(demark_exit, by = c("symbol", "date")) %>%
        mutate(demark_signal_past_n_days = is_demark_entry_yn + is_demark_exit_yn,
               demark_signal_past_n_days_flag = case_when(demark_signal_past_n_days >= 1 ~ 1,
                                                          demark_signal_past_n_days <= -1 ~ -1,
                                                          TRUE ~ demark_signal_past_n_days)) %>%
        select(-is_demark_entry_yn, -is_demark_exit_yn, -demark_signal_past_n_days) %>%
        arrange(symbol, date)

#################################################
# combine them all
output <- atr %>%
        dplyr::select(symbol, date,
                      open, high, low, close, volume,
                      atr, chanExit_long, chanExit_short) %>%
        dplyr::inner_join(chanExit %>% select(symbol, date, ce_long_dip_flag, ce_short_spike_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(macd %>% select(symbol, date, macd_flag = flag, macd_diff = diff, macd_trend_dir = trend_dir), by = c("symbol", "date")) %>%
        dplyr::inner_join(ha %>% select(symbol, date, ha_real_flag = real_flag, ha_real2_flag = real2_flag, ha_smooth_flag = smooth_flag), by = c("symbol", "date")) %>%   
        dplyr::inner_join(dcc %>% select(symbol, date, dcc_high, dcc_mid, dcc_low, dcc_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(rsi %>% select(symbol, date, rsi, rsi_oversold_yn, rsi_oversold_flag, rsi_overbought_yn, rsi_overbought_flag, rsi_trend_dir), by = c("symbol", "date")) %>%
        dplyr::inner_join(cci %>% select(symbol, date, cci, cci_oversold_yn, cci_oversold_flag, cci_overbought_yn, cci_overbought_flag, cci_trend_dir), by = c("symbol", "date")) %>%
        dplyr::inner_join(obv %>% select(symbol, date, obv, volume_inconsistency_alert, obv_flag), by = c("symbol", "date")) %>%        
        dplyr::inner_join(evwma %>% select(symbol, date, evwma, evwma_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(overnight %>% select(symbol, date, overnight_flag = flag), by = c("symbol", "date")) %>%        
        dplyr::inner_join(ma %>% select(symbol, date, 
                                        sma5, sma8, sma13,
                                        sma50, sma200,                                         
                                        ema5, ema20,
                                        zlema, proxy_flag, 
                                        sma5_flag, ema5_flag), 
                          by = c("symbol", "date")) %>% 
        dplyr::inner_join(csp, by = c("symbol", "date")) %>%
        dplyr::inner_join(traffic_light %>% select(symbol, date, red_flag, green_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(demark %>% select(symbol, date, demark_flag = flag, demark_signal_past_n_days_flag), by = c("symbol", "date")) %>%
        dplyr::mutate(csp_trend_dir = case_when(csp_candle_stick_signal == 1 & macd_trend_dir == 1 ~ 1,
                                                csp_candle_stick_signal == -1 & macd_trend_dir == -1 ~ -1,
                                                TRUE ~ 0)) %>%
        arrange(symbol, date)

# final indicators
indicators <- output %>%
        dplyr::inner_join(output %>%                
                                  group_by(symbol) %>%
                                  summarise(max_date = max(date)) %>%
                                  ungroup,
                          by = "symbol") %>%
        dplyr::mutate(year = lubridate::year(date),
                      is_today = dplyr::case_when(max_date == date ~ 1, TRUE ~ 0)) %>%
        dplyr::select(symbol, 
                      date, 
                      year, 
                      is_today, 
                      volume, 
                      open, 
                      high, 
                      low, 
                      close, 
                      atr,
                      zlema, 
                      evwma, 
                      sma5, 
                      sma8,
                      sma13,
                      sma50, 
                      sma200, 
                      ema5, 
                      ema20,                       
                      chanExit_long, 
                      chanExit_short, 
                      dcc_high, 
                      dcc_mid, 
                      dcc_low, 
                      macd_diff, 
                      rsi, 
                      cci, 
                      obv, 
                      volume_inconsistency_alert, 
                      macd_trend_dir, 
                      rsi_trend_dir,
                      cci_trend_dir,
                      rsi_oversold_yn, 
                      rsi_overbought_yn, 
                      cci_oversold_yn, 
                      cci_overbought_yn, 
                      proxy_flag, 
                      macd_flag, 
                      rsi_oversold_flag, 
                      rsi_overbought_flag, 
                      cci_oversold_flag, 
                      cci_overbought_flag, 
                      ce_long_dip_flag, 
                      ce_short_spike_flag, 
                      dcc_flag, 
                      ha_real_flag, 
                      ha_real2_flag,
                      ha_smooth_flag,
                      evwma_flag, 
                      overnight_flag, 
                      sma5_flag, 
                      ema5_flag,
                      red_flag, 
                      green_flag, 
                      obv_flag,
                      demark_flag, 
                      demark_signal_past_n_days_flag,
                      csp_doji, 
                      csp_dragonfly_doji, 
                      csp_gravestone_doji, 
                      csp_hammer, 
                      csp_inverted_harmer, 
                      csp_bullish_engulfing, 
                      csp_bearish_engulfing, 
                      csp_bullish_harami, 
                      csp_bearish_harami, 
                      csp_piercing_line, 
                      csp_dark_cloud_cover, 
                      csp_kick_up, 
                      csp_kick_down, 
                      csp_three_white_soliders, 
                      csp_three_black_crows, 
                      csp_morning_star, 
                      csp_evening_star, 
                      csp_rising_three, 
                      csp_failling_three, 
                      csp_up_trend, 
                      csp_down_trend, 
                      csp_bullish_candle, 
                      csp_bearish_candle, 
                      csp_candle_stick_signal, 
                      csp_candle_stick_pattern, 
                      csp_trend_dir, 
                      max_date) %>%
        arrange(symbol, date)

indicators$symbol %>% unique() %>% length()

##############################################################################################################
############################ <<< save indicators >>> #############################
# # save tbl
# schema = "adhoc"
# tbl_name = "indicators"
# tbl = paste0(schema, ".", tbl_name)
# DB = "stg"
# 
# # create connection
# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)
# 
# # save_tbl_action
# dim(indicators)
# tic()
# save_tbl_action(tbl, indicators, overwrite = TRUE)
# toc()
# 
# # check table
# d(glue::glue("select count(1) from {tbl}"))
# #indicators <- d(glue::glue("select * from {tbl} order by symbol, date"))
# 
# # disconnect db
# dbDisconnect(con)
