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

# begin transformation
t0 <- df3 %>%        
        dplyr::filter(symbol %nin% exclude_symbols) %>%
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
                      flag_build1 = dplyr::case_when( diff >0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 <0 ~ 1,

                                                      diff <0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 >0 ~ -1,
                                                      TRUE ~ 0 ),
                      flag = dplyr::case_when ( flag_build1 == 1 & 
                                                        (diff > diff_lag1) & 
                                                        (diff_lag1 > diff_lag2) &
                                                        (diff_lag2 > diff_lag3) ~ 1,
                                                flag_build1 == -1 & 
                                                        (diff < diff_lag1) & 
                                                        (diff_lag1 < diff_lag2) &
                                                        (diff_lag2 < diff_lag3) ~ -1,
                                                TRUE ~ 0 ),
                      trend_dir = dplyr::case_when(diff > diff_lag1 &
                                                           diff_lag1 > diff_lag2 &
                                                           diff_lag2 > diff_lag3 ~ 1,
                                                   diff < diff_lag1 &
                                                           diff_lag1 < diff_lag2 &
                                                           diff_lag2 < diff_lag3 ~ -1,
                                                   TRUE ~ 0)
        ) %>%
        ungroup()

############################
# heikin ashi
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
                dplyr::mutate(is_intraday_green_yn = dplyr::case_when(intraday_volatility >0 ~ 1, TRUE ~ -1),
                              is_intraday_green_yn_lag1 = lag(is_intraday_green_yn, 1),
                              is_intraday_green_yn_lag2 = lag(is_intraday_green_yn, 2),
                              is_intraday_green_yn_lag3 = lag(is_intraday_green_yn, 3),
                              is_intraday_green_yn_lag4 = lag(is_intraday_green_yn, 4),
                              ema10 = pracma::movavg(close, n = 10, type = "e"),  
                              ema30 = pracma::movavg(close, n = 30, type = "e")) %>%
                dplyr::mutate(temp_flag = dplyr::case_when( is_intraday_green_yn >0 & is_intraday_green_yn_lag1 >0 & is_intraday_green_yn_lag2 <0 & is_intraday_green_yn_lag3 <0 & is_intraday_green_yn_lag4 <0 ~ 1,
                                                            is_intraday_green_yn >0 & is_intraday_green_yn_lag1 >0 & is_intraday_green_yn_lag2 >0 & is_intraday_green_yn_lag3 <0 & is_intraday_green_yn_lag4 <0 ~ 1,
                                                            is_intraday_green_yn >0 & is_intraday_green_yn_lag1 >0 & is_intraday_green_yn_lag2 >0 & is_intraday_green_yn_lag3 >0 & is_intraday_green_yn_lag4 <0 ~ 1,

                                                            is_intraday_green_yn <0 & is_intraday_green_yn_lag1 <0 & is_intraday_green_yn_lag2 >0 & is_intraday_green_yn_lag3 >0 & is_intraday_green_yn_lag4 >0 ~ -1,
                                                            is_intraday_green_yn <0 & is_intraday_green_yn_lag1 <0 & is_intraday_green_yn_lag2 <0 & is_intraday_green_yn_lag3 >0 & is_intraday_green_yn_lag4 >0 ~ -1,
                                                            is_intraday_green_yn <0 & is_intraday_green_yn_lag1 <0 & is_intraday_green_yn_lag2 <0 & is_intraday_green_yn_lag3 <0 & is_intraday_green_yn_lag4 >0 ~ -1,
                                                            TRUE ~ 0 ))
        
}) %>%
        plyr::ldply() %>%
        as.tibble() %>%
        select(symbol, 
               date, 
               open, 
               high, 
               low, 
               close, 
               ema10, 
               ema30,
               intraday_volatility, 
               is_intraday_green_yn, 
               temp_flag) %>%
        arrange(symbol, date)

# heikin ashi candle stick signal
ha1 <- lapply(1:length(vector_of_symbols), function(x) candle_stick_pattern(ha0 %>% filter(symbol == vector_of_symbols[x]), func_list = c("bullish.candle", "bearish.candle"))) %>%
        plyr::ldply() %>%
        dplyr::mutate(candle_stick_signal = case_when(bullish_candle == 1 ~ 1,
                                                      bearish_candle == 1 ~ -1,
                                                      TRUE ~ 0))

# merge together
ha <- dplyr::inner_join(ha0, ha1, by = c("symbol", "date")) %>%
        dplyr::mutate(flag = case_when(temp_flag == 1 & candle_stick_signal == 1 ~ 1,
                                       temp_flag == -1 & candle_stick_signal == -1 ~ -1,
                                       TRUE ~ 0)) %>%
        select(-bullish_candle, -bearish_candle, -temp_flag) %>%
        arrange(symbol, date)

############################
# macd - heikin ashi
macd_ha <- ha %>%
        dplyr::select(symbol, date, open, high, low, close) %>%
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
                      flag_build1 = dplyr::case_when( diff >0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 <0 & diff_lag4 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 <0 ~ 1,

                                                      diff <0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 >0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 >0 & diff_lag4 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 & diff_lag4 >0 ~ -1,
                                                      TRUE ~ 0 ),
                      flag = dplyr::case_when ( flag_build1 == 1 & 
                                                        (diff > diff_lag1) & 
                                                        (diff_lag1 > diff_lag2) &
                                                        (diff_lag2 > diff_lag3) ~ 1,
                                                flag_build1 == -1 & 
                                                        (diff < diff_lag1) & 
                                                        (diff_lag1 < diff_lag2) &
                                                        (diff_lag2 < diff_lag3) ~ -1,
                                                TRUE ~ 0 ),
                      trend_dir = dplyr::case_when(diff > diff_lag1 &
                                                           diff_lag1 > diff_lag2 &
                                                           diff_lag2 > diff_lag3 ~ 1,
                                                   diff < diff_lag1 &
                                                           diff_lag1 < diff_lag2 &
                                                           diff_lag2 < diff_lag3 ~ -1,
                                                   TRUE ~ 0)
        ) %>%
        ungroup()

############################
# ADX
adx <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     n = 14, 
                     mutate_fun = ADX,
                     col_rename = c("dmi_p", "dmi_n", "dx", "adx")) %>%
        ungroup() %>%
        arrange(symbol, date) %>%
        dplyr::mutate(dmi_p_cross_above_yn = dplyr::case_when(dmi_p > dmi_n ~ 1, TRUE ~ 0),
                      dmi_n_cross_above_yn = dplyr::case_when(dmi_n > dmi_p ~ 1, TRUE ~ 0)) %>%        
        ungroup()

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
                      # oversold
                      rsi_oversold_ma = pracma::movavg(rsi_oversold_yn, n = 10, type = "s"),
                      rsi_oversold_flag = dplyr::case_when(rsi_oversold_ma >0 & rsi < 50 ~ 1, TRUE ~ 0),
                      # overbought
                      rsi_overbought_ma = pracma::movavg(rsi_overbought_yn, n = 10, type = "s"),
                      rsi_overbought_flag = dplyr::case_when(rsi_overbought_ma >0 & rsi > 50 ~ 1, TRUE ~ 0)) %>%
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
                      # oversold
                      cci_oversold_ma = pracma::movavg(cci_oversold_yn, n = 10, type = "s"),
                      cci_oversold_flag = dplyr::case_when(cci_oversold_ma >0 & cci < 100 ~ 1, TRUE ~ 0),
                      # overbought
                      cci_overbought_ma = pracma::movavg(cci_overbought_yn, n = 10, type = "s"),
                      cci_overbought_flag = dplyr::case_when(cci_overbought_ma >0 & cci > -100 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>%
        arrange(symbol, date)

############################
# obv
# obv <- t0 %>%
#         group_by(symbol) %>%
#         tq_mutate_xy(close,
#                      volume,
#                      mutate_fun = OBV) %>%
#         dplyr::mutate(obv_lag1 = lag(obv, 1)) %>%
#         dplyr::mutate(flag = dplyr::case_when(obv > obv_lag1 ~1, TRUE ~-1)) %>%
#         ungroup()

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
        dplyr::mutate(close_lag1 = dplyr::lag(close, 1),
                      close_lag2 = dplyr::lag(close, 2),
                      close_lag3 = dplyr::lag(close, 3),
                      close_lag4 = dplyr::lag(close, 4),
                      evwma_lag1 = dplyr::lag(evwma, 1),
                      evwma_lag2 = dplyr::lag(evwma, 2),
                      evwma_lag3 = dplyr::lag(evwma, 3),
                      evwma_lag4 = dplyr::lag(evwma, 4),
                      flag = dplyr::case_when( (close >evwma) & (close_lag1 <evwma_lag1) & (close_lag2 <evwma_lag2) & (close_lag3 <evwma_lag3) & (close_lag4 <evwma_lag4) ~ 1,
                                               (close >evwma) & (close_lag1 >evwma_lag1) & (close_lag2 <evwma_lag2) & (close_lag3 <evwma_lag3) & (close_lag4 <evwma_lag4) ~ 1,
                                               (close >evwma) & (close_lag1 >evwma_lag1) & (close_lag2 >evwma_lag2) & (close_lag3 <evwma_lag3) & (close_lag4 <evwma_lag4) ~ 1,
                                               (close >evwma) & (close_lag1 >evwma_lag1) & (close_lag2 >evwma_lag2) & (close_lag3 >evwma_lag3) & (close_lag4 <evwma_lag4) ~ 1,

                                               (close <evwma) & (close_lag1 >evwma_lag1) & (close_lag2 >evwma_lag2) & (close_lag3 >evwma_lag3) & (close_lag4 >evwma_lag4)~ -1,
                                               (close <evwma) & (close_lag1 <evwma_lag1) & (close_lag2 >evwma_lag2) & (close_lag3 >evwma_lag3) & (close_lag4 >evwma_lag4)~ -1,
                                               (close <evwma) & (close_lag1 <evwma_lag1) & (close_lag2 <evwma_lag2) & (close_lag3 >evwma_lag3) & (close_lag4 >evwma_lag4)~ -1,
                                               (close <evwma) & (close_lag1 <evwma_lag1) & (close_lag2 <evwma_lag2) & (close_lag3 <evwma_lag3) & (close_lag4 >evwma_lag4)~ -1,
                                               TRUE ~ 0 )) %>%
        ungroup()

############################
# overnight
overnight <- t0 %>%
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = lag(close, 1)) %>%
        dplyr::mutate(flag = dplyr::case_when((open > close_lag1) & (close > open) ~1,
                                              (open < close_lag1) & (close < open) ~-1,
                                              TRUE ~0)) %>%
        ungroup()

############################
# sma5, sma50, sma200, ema10, ema30, ema100, zlema, proxy_flag
ma <- t0 %>%
        # very important for calculating SMA and EMA by having asec(date)
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        ############################### SMA, EMA ###############################
dplyr::mutate(sma5 = pracma::movavg(close, n = 5, type = "s"),
              sma50 = pracma::movavg(close, n = 50, type = "s"),
              sma200 = pracma::movavg(close, n = 200, type = "s"), 
              
              ema10 = pracma::movavg(close, n = 10, type = "e"),  
              ema30 = pracma::movavg(close, n = 30, type = "e"),  
              ema100 = pracma::movavg(close, n = 100, type = "e"),
              
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
                      
                      sma5_lag1 = dplyr::lag(sma5, 1),
                      sma5_lag2 = dplyr::lag(sma5, 2),
                      sma5_lag3 = dplyr::lag(sma5, 3),
                      
                      sma5_flag = dplyr::case_when( (close >sma5) & (close_lag1 <sma5_lag1) & (close_lag2 <sma5_lag2) & (close_lag3 <sma5_lag3) ~ 1,
                                                    (close >sma5) & (close_lag1 >sma5_lag1) & (close_lag2 <sma5_lag2) & (close_lag3 <sma5_lag3) ~ 1,
                                                    
                                                    (close <sma5) & (close_lag1 >sma5_lag1) & (close_lag2 >sma5_lag2) & (close_lag3 >sma5_lag3) ~ -1,
                                                    (close <sma5) & (close_lag1 <sma5_lag1) & (close_lag2 >sma5_lag2) & (close_lag3 >sma5_lag3) ~ -1,
                                                    TRUE ~ 0 )
        ) %>%
        dplyr::select(-close_lag1, -close_lag2, -close_lag3, 
                      -sma5_lag1, -sma5_lag2, -sma5_lag3
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
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = lag(close, 1),
                      close_lag2 = lag(close, 2),
                      close_lag3 = lag(close, 3),
                      close_lag4 = lag(close, 4),
                      ce_long_lag1 = lag(chanExit_long, 1),
                      ce_long_lag2 = lag(chanExit_long, 2),
                      ce_long_lag3 = lag(chanExit_long, 3),
                      ce_long_lag4 = lag(chanExit_long, 4),
                      ce_short_lag1 = lag(chanExit_short, 1),
                      ce_short_lag2 = lag(chanExit_short, 2),
                      ce_short_lag3 = lag(chanExit_short, 3),
                      ce_short_lag4 = lag(chanExit_short, 4)) %>%
        dplyr::mutate(ce_long_dip_flag = case_when(close < chanExit_long & 
                                                           close_lag1 > ce_long_lag1 & 
                                                           close_lag2 > ce_long_lag2 &
                                                           close_lag3 > ce_long_lag3 & 
                                                           close_lag4 > ce_long_lag4 ~ 1,
                                                   close < chanExit_long &
                                                           close_lag1 < ce_long_lag1 & 
                                                           close_lag2 > ce_long_lag2 &
                                                           close_lag3 > ce_long_lag3 & 
                                                           close_lag4 > ce_long_lag4 ~ 1,
                                                   TRUE ~ 0),
                      ce_short_spike_flag = case_when(close > chanExit_short & 
                                                              close_lag1 < ce_short_lag1 & 
                                                              close_lag2 < ce_short_lag2 &
                                                              close_lag3 < ce_short_lag3 & 
                                                              close_lag4 < ce_short_lag4 ~ 1,
                                                      close > chanExit_short &
                                                              close_lag1 > ce_short_lag1 & 
                                                              close_lag2 < ce_short_lag2 &
                                                              close_lag3 < ce_short_lag3 & 
                                                              close_lag4 < ce_short_lag4 ~ 1,
                                                      TRUE ~ 0)) %>%
        ungroup()

############################
### "tradable" conditions - safety measures ###

############################
# (random walk)

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

#################################################
# combination
output <- atr %>%
        dplyr::select(symbol, date,
                      open, high, low, close, volume,
                      atr, chanExit_long, chanExit_short) %>%
        dplyr::inner_join(chanExit %>% select(symbol, date, ce_long_dip_flag, ce_short_spike_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(macd %>% select(symbol, date, macd_flag = flag, macd_diff = diff, macd_trend_dir = trend_dir), by = c("symbol", "date")) %>%
        dplyr::inner_join(ha %>% select(symbol, date, ha_flag = flag, is_intraday_green_yn_ha = is_intraday_green_yn), by = c("symbol", "date")) %>%   
        dplyr::inner_join(macd_ha %>% select(symbol, date, macd_ha_flag = flag), by = c("symbol", "date")) %>%             
        dplyr::inner_join(evwma %>% select(symbol, date, evwma, evwma_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(overnight %>% select(symbol, date, overnight_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(ma %>% select(symbol, date, 
                                        sma5, sma50, sma200, 
                                        ema10, ema30, ema100, 
                                        zlema, proxy_flag, sma5_flag), 
                          by = c("symbol", "date")) %>%
        dplyr::inner_join(adx %>% select(symbol, date, dmi_p, dmi_n, adx), by = c("symbol", "date")) %>%
        dplyr::inner_join(rsi %>% select(symbol, date, rsi, rsi_oversold_yn, rsi_oversold_flag, rsi_overbought_yn, rsi_overbought_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(cci %>% select(symbol, date, cci, cci_oversold_yn, cci_oversold_flag, cci_overbought_yn, cci_overbought_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(csp, by = c("symbol", "date")) %>%
        dplyr::mutate(csp_trend_dir = case_when(csp_candle_stick_signal == 1 & macd_trend_dir == 1 ~ 1,
                                                csp_candle_stick_signal == -1 & macd_trend_dir == -1 ~ -1,
                                                TRUE ~ 0)) %>%
        dplyr::select(symbol, date,
              open, high, low, close, zlema, sma5, sma50, sma200, ema10, ema30, ema100, proxy_flag, volume, 
              macd_diff, atr, chanExit_long, chanExit_short, 
              dmi_p, dmi_n, adx, rsi, cci, rsi_oversold_yn, rsi_oversold_flag, rsi_overbought_yn, rsi_overbought_flag, cci_oversold_yn, cci_oversold_flag, cci_overbought_yn, cci_overbought_flag, ce_long_dip_flag, ce_short_spike_flag, macd_trend_dir,
              everything()) %>%
        arrange(symbol, date)

trend_situation <- output %>%
        dplyr::select(symbol, date, ema10, ema30, sma50, sma200) %>%
        arrange(symbol, date)

situation_df <- trend_situation %>%
        dplyr::mutate(short_trend = case_when(ema10 > ema30 ~ 1, TRUE ~ -1),
                      long_trend = case_when(sma50 > sma200 ~ 1, TRUE ~ -1),
                      equity_situation = case_when(short_trend == 1 & long_trend == 1 ~ "EBull-EBull",
                                                   short_trend == 1 & long_trend == -1 ~ "EBull-EBear",
                                                   short_trend == -1 & long_trend == 1 ~ "EBear-EBull",
                                                   short_trend == -1 & long_trend == -1 ~ "EBear-EBear")) %>%
        dplyr::inner_join(trend_situation %>%             
                                  filter(symbol == "SPY") %>%
                                  dplyr::select(date, 
                                                market_ema10 = ema10,
                                                market_ema30 = ema30,
                                                market_sma50 = sma50,
                                                market_sma200 = sma200) %>%
                                  dplyr::mutate(short_market_trend = case_when(market_ema10 > market_ema30 ~ 1, TRUE ~ -1),
                                                long_market_trend = case_when(market_sma50 > market_sma200 ~ 1, TRUE ~ -1),
                                                market_situation = case_when(short_market_trend == 1 & long_market_trend == 1 ~ "SBull-SBull",
                                                                             short_market_trend == 1 & long_market_trend == -1 ~ "SBull-SBear",
                                                                             short_market_trend == -1 & long_market_trend == 1 ~ "SBear-SBull",
                                                                             short_market_trend == -1 & long_market_trend == -1 ~ "SBear-SBear")), 
                          by = "date") %>%
        dplyr::mutate(situation = paste0(equity_situation, "::", market_situation)) %>%
        arrange(symbol, date)

indicators <- output %>%
        dplyr::inner_join(output %>%                
                                  group_by(symbol) %>%
                                  summarise(max_date = max(date)) %>%
                                  ungroup,
                          by = "symbol") %>%
        dplyr::inner_join(situation_df %>% select(symbol, date, situation), by = c("symbol", "date")) %>%
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
                      proxy_flag,
                      zlema,                      
                      evwma,
                      chanExit_long,
                      chanExit_short,
                      atr,                      
                      sma5,
                      sma50,
                      sma200,
                      ema10,
                      ema30,
                      ema100,
                      dmi_p,
                      dmi_n,
                      adx,
                      rsi,
                      cci,
                      macd_diff,
                      macd_trend_dir,
                      macd_flag,
                      macd_ha_flag,
                      is_intraday_green_yn_ha,
                      ha_flag,                      
                      evwma_flag,
                      overnight_flag,
                      sma5_flag,
                      rsi_oversold_yn,
                      rsi_oversold_flag,
                      rsi_overbought_yn, 
                      rsi_overbought_flag,
                      cci_oversold_yn, 
                      cci_oversold_flag,
                      cci_overbought_yn, 
                      cci_overbought_flag,
                      ce_long_dip_flag, 
                      ce_short_spike_flag,
                      situation,
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
