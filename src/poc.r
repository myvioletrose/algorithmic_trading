# initiate set up
source("~/algorithmic_trading/config/setup.R")
readRenviron("~/algorithmic_trading/config/.env")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
av_api_key(ALPHA_VANTAGE_API)

# symbols
symbols = c("SPY", "GOOGL", "META", "AMC", "TSLA",
            "UBER", "AMZN", "NIO", "PRPL", "PLUG") %>%
        sort()
s = symbols

# get data
df = symbols %>%
        tq_get(get = "alphavantage", 
               av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
               outputsize = "full") 

##################################################################
########################### SP500 ###############################
symbols = tq_index("SP500")$symbol %>% sort()
symbols = c(symbols, "SPY") %>% sort()
symbols = symbols[symbols %nin% "-"]  # remove "-" from result
s = symbols 

symbolDf = data.frame(symbol = symbols) %>%
        arrange(symbol) %>%
        dplyr::mutate(id = row_number(),
                      batch = dplyr::case_when(id >= 1 & id <= 100 ~ 1,
                                               id >= 101 & id <= 200 ~ 2,
                                               id >= 201 & id <= 300 ~ 3,
                                               id >= 301 & id <= 400 ~ 4,
                                               TRUE ~ 5))

dfList = vector(mode = "list", length = length(symbolDf$batch %>% unique()))

tic()
for(i in 1:length(dfList)){
        batch = symbolDf %>%
                dplyr::filter(batch == i) %>%
                .$symbol
        df = batch %>%
                tq_get(get = "alphavantage", 
                       av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
                       outputsize = "full")
        dfList[[i]] = df        
        Sys.sleep(15)
}
toc()

df <- dfList %>% 
        plyr::ldply() %>%
        as.tibble()
dim(df); head(df)

check = df %>%
        group_by(symbol) %>%
        summarise(n = n()) %>%
        arrange(n)
check %>% head()

symbols <- check %>%
        dplyr::filter(n >500) %>%
        arrange(symbol) %>%
        .$symbol
s = symbols 

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
# begin beta-testing
t0 <- df3 %>%
        dplyr::filter(symbol %in% s) %>%
        dplyr::select(symbol,
                      date, 
                      open = adj_open,
                      high = adj_high,
                      low = adj_low,
                      close = adj_close,
                      volume) %>%
        dplyr::arrange(symbol, date)

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
                      flag_build1 = dplyr::case_when( diff >0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 <0 & diff_lag3 <0 ~ 1,
                                                      diff <0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 >0 & diff_lag3 >0 ~ -1,
                                                      TRUE ~ 0 ),
                      flag = dplyr::case_when ( flag_build1 == 1 & 
                                                        (diff > diff_lag1) & 
                                                        (diff_lag1 > diff_lag2) &
                                                        (diff_lag2 > diff_lag3) ~ 1,
                                                flag_build1 == -1 & 
                                                        (diff < diff_lag1) & 
                                                        (diff_lag1 < diff_lag2) &
                                                        (diff_lag2 < diff_lag3) ~ -1,
                                                TRUE ~ 0 )
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

ha = lapply(1:length(vector_of_symbols), function(x){
        
        ha_transform(df = t0 %>% 
                             dplyr::filter(symbol == vector_of_symbols[x])) %>%
                dplyr::mutate(symbol = vector_of_symbols[x]) %>%
                dplyr::select(symbol, everything()) %>%
                dplyr::arrange(date) %>%
                dplyr::mutate(intraday_lag1 = lag(intraday_volatility, 1),
                              intraday_lag2 = lag(intraday_volatility, 2),
                              intraday_lag3 = lag(intraday_volatility, 3),                      
                              flag_build1 = dplyr::case_when( intraday_volatility >0 & intraday_lag1 <0 & intraday_lag2 <0 & intraday_lag3 <0 ~ 1,
                                                              intraday_volatility >0 & intraday_lag1 >0 & intraday_lag2 <0 & intraday_lag3 <0 ~ 1,
                                                              #intraday_volatility >0 & intraday_lag1 >0 & intraday_lag2 >0 & intraday_lag3 <0 ~ 1,
                                                              
                                                              intraday_volatility <0 & intraday_lag1 >0 & intraday_lag2 >0 & intraday_lag3 >0 ~ -1,
                                                              intraday_volatility <0 & intraday_lag1 <0 & intraday_lag2 >0 & intraday_lag3 >0 ~ -1,
                                                              #intraday_volatility <0 & intraday_lag1 <0 & intraday_lag2 <0 & intraday_lag3 >0 ~ -1,
                                                              TRUE ~ 0 ),
                              flag = dplyr::case_when ( flag_build1 == 1 & 
                                                                (intraday_volatility > intraday_lag1) & 
                                                                (intraday_lag1 > intraday_lag2) &
                                                                (intraday_lag2 > intraday_lag3) ~ 1,
                                                        flag_build1 == -1 & 
                                                                (intraday_volatility < intraday_lag1) & 
                                                                (intraday_lag1 < intraday_lag2) &
                                                                (intraday_lag2 < intraday_lag3) ~ -1,
                                                        TRUE ~ 0 )
                )}) %>%
        plyr::ldply() %>%
        as.tibble()

############################
# macd (heikin ashi)
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
                      flag_build1 = dplyr::case_when( diff >0 & diff_lag1 <0 & diff_lag2 <0 & diff_lag3 <0 ~ 1,
                                                      diff >0 & diff_lag1 >0 & diff_lag2 <0 & diff_lag3 <0 ~ 1,
                                                      diff <0 & diff_lag1 >0 & diff_lag2 >0 & diff_lag3 >0 ~ -1,
                                                      diff <0 & diff_lag1 <0 & diff_lag2 >0 & diff_lag3 >0 ~ -1,
                                                      TRUE ~ 0 ),
                      flag = dplyr::case_when ( flag_build1 == 1 & 
                                                        (diff > diff_lag1) & 
                                                        (diff_lag1 > diff_lag2) &
                                                        (diff_lag2 > diff_lag3) ~ 1,
                                                flag_build1 == -1 & 
                                                        (diff < diff_lag1) & 
                                                        (diff_lag1 < diff_lag2) &
                                                        (diff_lag2 < diff_lag3) ~ -1,
                                                TRUE ~ 0 )
        ) %>%
        ungroup()

############################
# ADX
adx <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     n = 15, 
                     mutate_fun = ADX,
                     col_rename = c("dmi_p", "dmi_n", "dx", "adx")) %>%
        ungroup() %>%
        arrange(symbol, date) %>%
        dplyr::mutate(dmi_p_cross_above_yn = dplyr::case_when(dmi_p > dmi_n ~ 1, TRUE ~ 0),
                      dmi_n_cross_above_yn = dplyr::case_when(dmi_n > dmi_p ~ 1, TRUE ~ 0)) %>%
        dplyr::mutate(dmi_p_cross_above_yn_lag1 = dplyr::lag(dmi_p_cross_above_yn, 1),
                      dmi_p_cross_above_yn_lag2 = dplyr::lag(dmi_p_cross_above_yn, 2),
                      dmi_p_cross_above_yn_lag3 = dplyr::lag(dmi_p_cross_above_yn, 3),
                      dmi_n_cross_above_yn_lag1 = dplyr::lag(dmi_n_cross_above_yn, 1),
                      dmi_n_cross_above_yn_lag2 = dplyr::lag(dmi_n_cross_above_yn, 2),
                      dmi_n_cross_above_yn_lag3 = dplyr::lag(dmi_n_cross_above_yn, 3)) %>%
        dplyr::mutate(dmi_entry_flag = dplyr::case_when(adx >= 25 &
                                                                dmi_p_cross_above_yn_lag3 == 0 &
                                                                dmi_p_cross_above_yn_lag2 == 0 &
                                                                dmi_p_cross_above_yn_lag1 == 0 &
                                                                dmi_p_cross_above_yn == 1 ~ 1,
                                                        adx >= 25 &
                                                                dmi_p_cross_above_yn_lag3 == 0 &
                                                                dmi_p_cross_above_yn_lag2 == 0 &
                                                                dmi_p_cross_above_yn_lag1 == 1 &
                                                                dmi_p_cross_above_yn == 1 ~ 1,
                                                        TRUE ~ 0),
                      dmi_exit_flag = dplyr::case_when(dmi_n_cross_above_yn_lag3 == 0 &
                                                               dmi_n_cross_above_yn_lag2 == 0 &
                                                               dmi_n_cross_above_yn_lag1 == 0 &
                                                               dmi_n_cross_above_yn == 1 ~ -1,
                                                       dmi_n_cross_above_yn_lag3 == 0 &
                                                               dmi_n_cross_above_yn_lag2 == 0 &
                                                               dmi_n_cross_above_yn_lag1 == 1 &
                                                               dmi_n_cross_above_yn == 1 ~ -1,
                                                       TRUE ~ 0),
                      flag = dplyr::case_when(dmi_entry_flag == 1 ~ 1, TRUE ~ dmi_exit_flag)) %>%
        ungroup()

############################
# rsi
rsi <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = RSI) %>%
        ungroup()

############################
# cci
cci <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     mutate_fun = CCI) %>%
        ungroup()

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
vwap <- t0 %>%
        dplyr::mutate(typical_price = (high + low + close)/3) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_mutate_xy(typical_price,
                     volume,
                     mutate_fun = VWAP,
                     col_rename = "vwap") %>%
        dplyr::mutate(close_lag1 = dplyr::lag(close, 1),
                      close_lag2 = dplyr::lag(close, 2),
                      close_lag3 = dplyr::lag(close, 3),
                      vwap_lag1 = dplyr::lag(vwap, 1),
                      vwap_lag2 = dplyr::lag(vwap, 2),
                      vwap_lag3 = dplyr::lag(vwap, 3),
                      flag = dplyr::case_when( (close >vwap) & (close_lag1 <vwap_lag1) & (close_lag2 <vwap_lag2) & (close_lag3 <vwap_lag3) ~ 1,
                                               (close >vwap) & (close_lag1 >vwap_lag1) & (close_lag2 <vwap_lag2) & (close_lag3 <vwap_lag3) ~ 1,
                                               (close <vwap) & (close_lag1 >vwap_lag1) & (close_lag2 >vwap_lag2) & (close_lag3 >vwap_lag3) ~ -1,
                                               (close <vwap) & (close_lag1 <vwap_lag1) & (close_lag2 >vwap_lag2) & (close_lag3 >vwap_lag3) ~ -1,
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
# sma5, ema10, ema30, ema50, ema100, ema200, zlema, proxy_flag
ma <- t0 %>%
        # very important for calculating SMA and EMA by having asec(date)
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        ############################### SMA, EMA ###############################
dplyr::mutate(sma5 = pracma::movavg(close, n = 5, type = "s"),
              ema10 = pracma::movavg(close, n = 10, type = "e"),  
              ema30 = pracma::movavg(close, n = 30, type = "e"),  
              ema50 = pracma::movavg(close, n = 50, type = "e"),
              ema100 = pracma::movavg(close, n = 100, type = "e"),
              ema200 = pracma::movavg(close, n = 200, type = "e"),
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
        ############################### ema50 flag ###############################
arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(close_lag1 = dplyr::lag(close, 1),
                      close_lag2 = dplyr::lag(close, 2),
                      close_lag3 = dplyr::lag(close, 3),
                      ema50_lag1 = dplyr::lag(ema50, 1),
                      ema50_lag2 = dplyr::lag(ema50, 2),
                      ema50_lag3 = dplyr::lag(ema50, 3),
                      ema50_flag = dplyr::case_when( (close >ema50) & (close_lag1 <ema50_lag1) & (close_lag2 <ema50_lag2) & (close_lag3 <ema50_lag3) ~ 1,
                                                     (close >ema50) & (close_lag1 >ema50_lag1) & (close_lag2 <ema50_lag2) & (close_lag3 <ema50_lag3) ~ 1,
                                                     (close <ema50) & (close_lag1 >ema50_lag1) & (close_lag2 >ema50_lag2) & (close_lag3 >ema50_lag3) ~ -1,
                                                     (close <ema50) & (close_lag1 <ema50_lag1) & (close_lag2 >ema50_lag2) & (close_lag3 >ema50_lag3) ~ -1,
                                                     TRUE ~ 0 )
        ) %>%
        ############################### ema10 flag ###############################
dplyr::mutate(ema10_lag1 = dplyr::lag(ema10, 1),
              ema10_lag2 = dplyr::lag(ema10, 2),
              ema10_lag3 = dplyr::lag(ema10, 3),
              ema30_lag1 = dplyr::lag(ema30, 1),
              ema30_lag2 = dplyr::lag(ema30, 2),
              ema30_lag3 = dplyr::lag(ema30, 3),
              ema10_flag = dplyr::case_when( (ema10 >ema30) & (ema10_lag1 <ema30_lag1) & (ema10_lag2 <ema30_lag2) & (ema10_lag3 <ema30_lag3) ~ 1,
                                             (ema10 >ema30) & (ema10_lag1 >ema30_lag1) & (ema10_lag2 <ema30_lag2) & (ema10_lag3 <ema30_lag3) ~ 1,
                                             (ema10 <ema30) & (ema10_lag1 >ema30_lag1) & (ema10_lag2 >ema30_lag2) & (ema10_lag3 >ema30_lag3) ~ -1,
                                             (ema10 <ema30) & (ema10_lag1 <ema30_lag1) & (ema10_lag2 >ema30_lag2) & (ema10_lag3 >ema30_lag3) ~ -1,
                                             TRUE ~ 0 )
) %>%
        ############################### sma5 flag ###############################
dplyr::mutate(sma5_lag1 = dplyr::lag(sma5, 1),
              sma5_lag2 = dplyr::lag(sma5, 2),
              sma5_lag3 = dplyr::lag(sma5, 3),
              sma5_flag = dplyr::case_when( (close >sma5) & (close_lag1 <sma5_lag1) & (close_lag2 <sma5_lag2) & (close_lag3 <sma5_lag3) ~ 1,
                                            (close >sma5) & (close_lag1 >sma5_lag1) & (close_lag2 <sma5_lag2) & (close_lag3 <sma5_lag3) ~ 1,
                                            (close <sma5) & (close_lag1 >sma5_lag1) & (close_lag2 >sma5_lag2) & (close_lag3 >sma5_lag3) ~ -1,
                                            (close <sma5) & (close_lag1 <sma5_lag1) & (close_lag2 >sma5_lag2) & (close_lag3 >sma5_lag3) ~ -1,
                                            TRUE ~ 0 )
) %>%
        dplyr::select(-close_lag1, -close_lag2, -close_lag3, 
                      -ema10_lag1, -ema10_lag2, -ema10_lag3,
                      -ema30_lag1, -ema30_lag2, -ema30_lag3,
                      -ema50_lag1, -ema50_lag2, -ema50_lag3
        ) %>%
        ungroup

############################
# ATR, Chandelier Exit
atr0 <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = c("high", "low", "close"),
                     n = 20, 
                     mutate_fun = ATR,
                     col_rename = c("tr", "atr", "trueHigh", "trueLow")) %>%
        ungroup()

higherH <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = "high",
                     mutate_fun = rollapply,
                     width = 20,
                     FUN = max,
                     by.column = FALSE,
                     col_rename = "higherHigh") %>%
        ungroup()

lowerL <- t0 %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        tq_transmute(select = "low",
                     mutate_fun = rollapply,
                     width = 20,
                     FUN = min,
                     by.column = FALSE,
                     col_rename = "lowerLow") %>%
        ungroup()

atr <- t0 %>%        
        dplyr::inner_join(atr0, by = c("symbol", "date")) %>%
        dplyr::inner_join(higherH, by = c("symbol", "date")) %>%
        dplyr::inner_join(lowerL, by = c("symbol", "date")) %>%
        arrange(symbol, date) %>%
        dplyr::mutate(chanExit_long = higherHigh - atr*3,
                      chanExit_short = lowerLow + atr*3) %>%
        dplyr::mutate(is_close_below_ce_long_yn = dplyr::case_when(close < chanExit_long ~ 1, TRUE ~ 0),
                      is_close_above_ce_short_yn = dplyr::case_when(close > chanExit_short ~ 1, TRUE ~ 0)) %>%
        dplyr::mutate(is_close_below_ce_long_yn_lag1 = dplyr::lag(is_close_below_ce_long_yn, 1),
                      is_close_below_ce_long_yn_lag2 = dplyr::lag(is_close_below_ce_long_yn, 2),
                      is_close_below_ce_long_yn_lag3 = dplyr::lag(is_close_below_ce_long_yn, 3),
                      is_close_above_ce_short_yn_lag1 = dplyr::lag(is_close_above_ce_short_yn, 1),
                      is_close_above_ce_short_yn_lag2 = dplyr::lag(is_close_above_ce_short_yn, 2),
                      is_close_above_ce_short_yn_lag3 = dplyr::lag(is_close_above_ce_short_yn, 3)) %>%
        dplyr::mutate(ce_entry_flag = dplyr::case_when(is_close_above_ce_short_yn_lag3 == 0 &
                                                               is_close_above_ce_short_yn_lag2 == 0 &
                                                               is_close_above_ce_short_yn_lag1 == 0 &
                                                               is_close_above_ce_short_yn == 1 ~ 1,
                                                       is_close_above_ce_short_yn_lag3 == 0 &
                                                               is_close_above_ce_short_yn_lag2 == 0 &
                                                               is_close_above_ce_short_yn_lag1 == 1 &
                                                               is_close_above_ce_short_yn == 1 ~ 1,
                                                       TRUE ~ 0),
                      ce_exit_flag =  dplyr::case_when(is_close_below_ce_long_yn_lag3 == 0 &
                                                               is_close_below_ce_long_yn_lag2 == 0 &
                                                               is_close_below_ce_long_yn_lag1 == 0 &
                                                               is_close_below_ce_long_yn == 1 ~ -1,
                                                       is_close_below_ce_long_yn_lag3 == 0 &
                                                               is_close_below_ce_long_yn_lag2 == 0 &
                                                               is_close_below_ce_long_yn_lag1 == 1 &
                                                               is_close_below_ce_long_yn == 1 ~ -1,
                                                       TRUE ~ 0),
                      flag = dplyr::case_when(ce_entry_flag == 1 ~ 1, TRUE ~ ce_exit_flag))

############################
### "tradable" conditions - safety measures ###
favorable_conditions <- atr %>%
        dplyr::select(symbol, date, close, chanExit_long) %>%        
        dplyr::inner_join(macd %>% dplyr::select(symbol, date, 
                                                 macd_diff = diff,
                                                 macd_diff_lag1 = diff_lag1,
                                                 macd_diff_lag2 = diff_lag2), 
                          by = c("symbol", "date")) %>%
        dplyr::inner_join(macd_ha %>% dplyr::select(symbol, date, 
                                                    macd_ha_diff = diff,
                                                    macd_ha_diff_lag1 = diff_lag1,
                                                    macd_ha_diff_lag2 = diff_lag2), 
                          by = c("symbol", "date")) %>%
        dplyr::inner_join(ha %>% dplyr::select(symbol, date, 
                                               intraday_volatility,
                                               intraday_lag1,
                                               intraday_lag2), 
                          by = c("symbol", "date")) %>%
        dplyr::inner_join(adx %>% dplyr::select(symbol, date, adx, dmi_p, dmi_n), by = c("symbol", "date")) %>%
        dplyr::mutate(                
                # drop below chanExit Long
                below_chanExit_long_flag = dplyr::case_when(close < chanExit_long ~ 1, TRUE ~ 0),
                # macd flag
                macd_flag = dplyr::case_when(macd_diff >0 &
                                                     macd_diff > macd_diff_lag1 &
                                                     macd_diff_lag1 > macd_diff_lag2 ~ 1, 
                                             macd_diff <0 & 
                                                     macd_diff < macd_diff_lag1 &
                                                     macd_diff_lag1 < macd_diff_lag2 ~ -1, 
                                             TRUE ~ 0),
                # macd (Heikin Ashi) flag
                macd_ha_flag = dplyr::case_when(macd_ha_diff >0 &
                                                        macd_ha_diff > macd_ha_diff_lag1 &
                                                        macd_ha_diff_lag1 > macd_ha_diff_lag2 ~ 1, 
                                                macd_ha_diff <0 &
                                                        macd_ha_diff < macd_ha_diff_lag1 &
                                                        macd_ha_diff_lag1 < macd_ha_diff_lag2 ~ -1, 
                                                TRUE ~ 0),
                # Heikin Ashi flag
                ha_flag = dplyr::case_when(intraday_volatility >0 &
                                                   intraday_volatility > intraday_lag1 &
                                                   intraday_lag1 > intraday_lag2 ~ 1, 
                                           intraday_volatility <0 &
                                                   intraday_volatility < intraday_lag1 &
                                                   intraday_lag1 < intraday_lag2 ~ -1, 
                                           TRUE ~ 0),
                # adx flag
                adx_flag = dplyr::case_when((adx >=25 & dmi_p > dmi_n) ~ 1, 
                                            dmi_p < dmi_n ~ -1,
                                            TRUE ~ 0)
        ) %>%
        dplyr::mutate(
                # buy condition(s)
                buy_condition = dplyr::case_when((macd_flag + macd_ha_flag + ha_flag) >=1 ~ 1, TRUE ~ 0),
                buy_condition = dplyr::case_when((buy_condition == 1 & adx_flag != -1) ~ 1, TRUE ~ 0),                
                # sell condition(s)
                sell_condition = dplyr::case_when((macd_flag + macd_ha_flag + ha_flag) <=-1 ~ 1, TRUE ~ 0),
                sell_condition = dplyr::case_when(sell_condition == 1 & adx_flag != 1 ~ 1, TRUE ~ 0),
                sell_condition = dplyr::case_when(below_chanExit_long_flag == 1 & adx_flag == -1 ~ 1, TRUE ~ sell_condition)
        ) %>%
        dplyr::arrange(symbol, date)

############################
# (random walk)

############################
# (candlestick patterns)

############################
# stock pick & generating additional signal/confirmatoins
stock_pick <- vwap %>%
        dplyr::select(symbol, date, close, vwap) %>%
        dplyr::mutate(is_close_above_vwap_yn = dplyr::case_when(close > vwap ~ 1, TRUE ~ -1)) %>%
        dplyr::inner_join(atr %>%
                                  dplyr::select(symbol, date, 
                                                is_close_below_ce_long_yn,
                                                is_close_above_ce_short_yn), by = c("symbol", "date")) %>%
        dplyr::inner_join(adx %>%
                                  dplyr::select(symbol, date,
                                                dmi_p_cross_above_yn,
                                                dmi_n_cross_above_yn,
                                                adx), by = c("symbol", "date")) %>%
        dplyr::inner_join(ma %>%
                                  dplyr::select(symbol, date,
                                                ema10, ema30) %>%
                                  dplyr::mutate(is_ema10_above_ema30_yn = dplyr::case_when(ema10 > ema30 ~ 1,
                                                                                           TRUE ~ -1)),
                          by = c("symbol", "date")) %>%
        dplyr::mutate(year = lubridate::year(date)) %>%
        dplyr::select(symbol, 
                      year, 
                      date, 
                      close, 
                      vwap, 
                      ema10, 
                      ema30, 
                      adx,
                      is_close_above_vwap_yn, 
                      is_close_below_ce_long_yn, 
                      is_close_above_ce_short_yn, 
                      is_ema10_above_ema30_yn, 
                      dmi_p_cross_above_yn, 
                      dmi_n_cross_above_yn) %>%
        arrange(symbol, date)

stock_pick2 = sqldf("
with sub 
as (
        select symbol, date, close, vwap, ema10, ema30, adx
        
        , is_close_above_vwap_yn
        , is_close_above_ce_short_yn
        , is_close_below_ce_long_yn
        , dmi_p_cross_above_yn
        , dmi_n_cross_above_yn
        , is_ema10_above_ema30_yn
        
        , sum(case when is_close_above_vwap_yn = 1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as vwap_up_flag
        , sum(case when is_close_above_vwap_yn = -1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as vwap_down_flag
        
        --, sum(case when is_close_above_ce_short_yn = 1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as ce_up_flag
        --, sum(case when is_close_below_ce_long_yn = 1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as ce_down_flag
        
        , sum(case when dmi_p_cross_above_yn = 1 and adx >25 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as adx_up_flag
        , sum(case when dmi_n_cross_above_yn = 1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as adx_down_flag
        
        , sum(case when is_ema10_above_ema30_yn = 1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as ema10_up_flag
        , sum(case when is_ema10_above_ema30_yn = -1 then 1 else 0 end) over(partition by symbol order by date ROWS BETWEEN 10 preceding AND 1 preceding) as ema10_down_flag
        from stock_pick
),

sub2
as (
        select symbol, date, close, vwap, ema10, ema30, adx
        
        , case when is_close_above_vwap_yn = 1 and vwap_down_flag = 10 then 1 else 0 end as vwap_up_flag
        , case when is_close_above_vwap_yn = -1 and vwap_up_flag = 10 then 1 else 0 end as vwap_down_flag
        
        --, case when is_close_above_ce_short_yn = 1 and ce_down_flag = 10 then 1 else 0 end as ce_up_flag
        --, case when is_close_below_ce_long_yn = 1 and ce_up_flag = 10 then 1 else 0 end as ce_down_flag        
        
        , case when dmi_p_cross_above_yn = 1 and adx > 25 and adx_down_flag = 10 then 1 else 0 end as adx_up_flag
        , case when dmi_n_cross_above_yn = 1 and adx_up_flag = 10 then 1 else 0 end as adx_down_flag                
        
        , case when is_ema10_above_ema30_yn = 1 and ema10_down_flag = 10 then 1 else 0 end as ema_up_flag
        , case when is_ema10_above_ema30_yn = -1 and ema10_up_flag = 10 then 1 else 0 end as ema_down_flag        
        from sub
)

select *
from sub2
order by symbol, date
")

# additional signal - vwap_breakout_flag
vwap2 <- stock_pick2 %>%
        dplyr::select(symbol, date, 
                      vwap_breakout_up_flag = vwap_up_flag, 
                      vwap_breakout_down_flag = vwap_down_flag) %>%
        dplyr::mutate(vwap_breakout_flag = dplyr::case_when(vwap_breakout_up_flag == 1 ~ 1,
                                                            TRUE ~ vwap_breakout_down_flag*-1)) %>%
        select(-vwap_breakout_up_flag, -vwap_breakout_down_flag) %>%
        arrange(symbol, date)

# additional confirmations - adx_breakout_flag, ema_breakout_flag
adx2 <- stock_pick2 %>%
        dplyr::select(symbol, date, 
                      adx_breakout_up_flag = adx_up_flag, 
                      adx_breakout_down_flag = adx_down_flag) %>%
        dplyr::mutate(adx_breakout_flag = dplyr::case_when(adx_breakout_up_flag == 1 ~ 1,
                                                           TRUE ~ adx_breakout_down_flag*-1)) %>%
        select(-adx_breakout_up_flag, -adx_breakout_down_flag) %>%
        arrange(symbol, date)

ema2 <- stock_pick2 %>%
        dplyr::select(symbol, date, 
                      ema_breakout_up_flag = ema_up_flag, 
                      ema_breakout_down_flag = ema_down_flag) %>%
        dplyr::mutate(ema_breakout_flag = dplyr::case_when(ema_breakout_up_flag == 1 ~ 1,
                                                           TRUE ~ ema_breakout_down_flag*-1)) %>%
        select(-ema_breakout_up_flag, -ema_breakout_down_flag) %>%
        arrange(symbol, date)

#################################################
# combination
output <- atr %>%
        dplyr::select(symbol, date,
                      open, high, low, close, volume,
                      atr, chanExit_long, chanExit_short,
                      ce_flag = flag) %>%
        dplyr::inner_join(macd %>% select(symbol, date, macd_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(ha %>% select(symbol, date, ha_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(macd_ha %>% select(symbol, date, macd_ha_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(vwap %>% select(symbol, date, vwap, vwap_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(overnight %>% select(symbol, date, overnight_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(ma %>% select(symbol, date, ema10, ema30, ema100, zlema, proxy_flag, 
                                        sma5_flag, ema10_flag, ema50_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(adx %>% select(symbol, date, dmi_p, dmi_n, adx, adx_flag = flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(rsi %>% select(symbol, date, rsi), by = c("symbol", "date")) %>%
        dplyr::inner_join(cci %>% select(symbol, date, cci), by = c("symbol", "date")) %>%
        dplyr::inner_join(vwap2 %>% select(symbol, date, vwap_breakout_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(adx2 %>% select(symbol, date, adx_breakout_flag), by = c("symbol", "date")) %>%
        dplyr::inner_join(ema2 %>% select(symbol, date, ema_breakout_flag), by = c("symbol", "date")) %>%
        dplyr::select(symbol, date,
                      open, high, low, close, zlema, vwap, ema10, ema30, ema100, proxy_flag, volume, 
                      atr, chanExit_long, chanExit_short, 
                      dmi_p, dmi_n, adx, rsi, cci, adx_flag,
                      everything()) %>%
        arrange(symbol, date)

output2 <- output %>%
        dplyr::mutate(signal = macd_flag + macd_ha_flag + ha_flag + vwap_flag + vwap_breakout_flag,
                      confirmation = macd_flag + macd_ha_flag + ha_flag + vwap_flag + vwap_breakout_flag +                     
                              ce_flag + adx_flag + overnight_flag + sma5_flag + ema10_flag + ema50_flag +
                              adx_breakout_flag + ema_breakout_flag) %>%
        arrange(symbol, date) %>%
        # rule, logic combine
        dplyr::mutate(signal_lag1 = dplyr::lag(signal,1),
                      signal_lag2 = dplyr::lag(signal,2),
                      signal_lag3 = dplyr::lag(signal,3),
                      signal_lag4 = dplyr::lag(signal,4),
                      confirmation_lag1 = dplyr::lag(confirmation, 1),
                      confirmation_lag2 = dplyr::lag(confirmation, 2),
                      confirmation_lag3 = dplyr::lag(confirmation, 3),
                      confirmation_lag4 = dplyr::lag(confirmation, 4),
                      # feedback loop message: -1, 0, 1
                      # signal confirmation
                      message = dplyr::case_when(
                              # buy
                              confirmation >0 & signal_lag1 >0 & confirmation_lag1 >=0 ~ 1,
                              confirmation >0 & signal_lag2 >0 & confirmation_lag1 >=0 ~ 1,
                              confirmation >0 & signal_lag3 >0 & confirmation_lag1 >=0 ~ 1,
                              confirmation >0 & signal_lag4 >0 & confirmation_lag1 >=0 ~ 1,
                              # sell
                              confirmation <0 & signal_lag1 <0 & confirmation_lag1 <=0 ~ -1,
                              confirmation <0 & signal_lag2 <0 & confirmation_lag1 <=0 ~ -1,
                              confirmation <0 & signal_lag3 <0 & confirmation_lag1 <=0 ~ -1,
                              confirmation <0 & signal_lag4 <0 & confirmation_lag1 <=0 ~ -1,
                              TRUE ~ 0),
                      # ensurance - zlema, vwap
                      message = dplyr::case_when(message == 1 & (close > zlema) & (close > vwap) ~ 1,
                                                 message == -1 & (close < zlema | close < vwap) ~ -1, 
                                                 TRUE ~ 0)) %>%
        # join with favorable conditions
        dplyr::inner_join(favorable_conditions %>%
                                  dplyr::select(symbol, date, buy_condition, sell_condition), 
                          by = c("symbol", "date")) %>%
        # join with market conditions
        dplyr::inner_join(favorable_conditions %>%
                                  dplyr::filter(symbol == "SPY") %>%
                                  dplyr::inner_join(ma %>% 
                                                            dplyr::filter(symbol == "SPY") %>%
                                                            dplyr::mutate(market_trend_ema10_flag = dplyr::case_when(ema10 > ema30 ~ 1, TRUE ~ -1)) %>%
                                                            dplyr::select(date, market_trend_ema10_flag),
                                                    by = "date") %>%
                                  dplyr::select(date, 
                                                market_trend_ema10_flag,
                                                market_buy_condition = buy_condition, 
                                                market_sell_condition = sell_condition), 
                          by = "date") %>%
        # update message with buy/sell condition(s)
        dplyr::mutate(message = dplyr::case_when(
                # buy if,
                message == 1 & 
                        buy_condition == 1 &
                        market_trend_ema10_flag == 1 &
                        market_sell_condition == 0 ~ 1,
                message == 1 &
                        buy_condition == 1 &
                        market_trend_ema10_flag == -1 &
                        market_buy_condition == 1 ~ 1,
                # sell if,
                signal <0 & 
                        signal_lag1 <0 &                        
                        (close < zlema | close < vwap) ~ -1,
                signal <0 &
                        confirmation_lag1 <0 &
                        (adx_flag == -1 | sell_condition == 1) ~ -1,
                confirmation <0 &
                        confirmation_lag1 <0 &                        
                        sell_condition == 1 &
                        (close < zlema | close < vwap) ~ -1,
                # with market downtrend
                signal <0 & 
                        market_trend_ema10_flag == -1 &
                        (close < zlema | close < vwap) ~ -1,
                confirmation <0 &
                        confirmation_lag1 <0 &
                        market_trend_ema10_flag == -1 &
                        (close < zlema | close < vwap) ~ -1,
                TRUE ~ 0)) %>%
        # trailing_stop_loss, profit protection
        dplyr::mutate(trailing_stop_loss = close - 2*atr,
                      trailing_stop_loss_yesterday = dplyr::lag(trailing_stop_loss, 1),
                      message = dplyr::case_when(close < trailing_stop_loss_yesterday ~ -1, 
                                                 close < chanExit_long & 
                                                         (close < zlema | close < vwap) ~ -1,
                                                 close < chanExit_long &
                                                         market_trend_ema10_flag == -1 ~ -1,
                                                 TRUE ~ message)) %>%
        dplyr::select(-signal_lag1, -signal_lag2, -signal_lag3, -signal_lag4, 
                      -confirmation_lag1, -confirmation_lag2, -confirmation_lag3, -confirmation_lag4, 
                      -trailing_stop_loss) %>%
        dplyr::select(symbol, date,
                      volume, open, high, low, close, message, proxy_flag, zlema, vwap, trailing_stop_loss_yesterday,
                      everything()) %>%
        arrange(symbol, date)

poc <- output2 %>%
        dplyr::inner_join(output2 %>%
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
                      message,
                      proxy_flag,
                      zlema,
                      vwap,
                      trailing_stop_loss_yesterday,
                      ema10,
                      ema30,
                      ema100,
                      atr,
                      chanExit_long,
                      chanExit_short,
                      dmi_p,
                      dmi_n,
                      adx,
                      rsi,
                      cci,
                      macd_flag,
                      macd_ha_flag,
                      ha_flag,
                      ce_flag,
                      vwap_flag,
                      vwap_breakout_flag,
                      adx_flag,
                      adx_breakout_flag,
                      overnight_flag,
                      sma5_flag,
                      ema10_flag,
                      ema50_flag,
                      ema_breakout_flag,
                      signal,
                      confirmation,
                      buy_condition,
                      sell_condition,
                      market_trend_ema10_flag,
                      market_buy_condition,
                      market_sell_condition
        ) %>%
        arrange(symbol, date)

########################################################################################################################################################################################
############################################################# backtest simulation #####################################################
########################################################################################################################

#symbols
#[1] "AMC"   "AMZN"  "GOOGL" "META"  "NIO"   "PLUG"  "PRPL"  "SPY"   "TSLA"  "UBER"

temp_df <- poc %>% dplyr::select(symbol, year) %>% distinct()
temp_list = vector(mode = "list", length = nrow(temp_df))

tic()
for(i in 1:nrow(temp_df)){
        
        s = temp_df$symbol[i]
        yr = temp_df$year[i]
        
        x <- poc %>%
                dplyr::filter(year >= yr & 
                                      year < yr+1 &
                                      symbol == s) %>%
                dplyr::select(symbol, date, open, high, low, close, message) %>%
                dplyr::mutate(message = case_when(message == 1 ~ "buy",
                                                  message == 0 ~ "hold",
                                                  message == -1 ~ "sell"))
        
        y1 <- strategyEval(fund_begin = 1000, x) %>% .$net_value
        
        z <- data.frame(symbol = s,
                        year = yr,
                        net_value = y1)
        
        temp_list[[i]] = z        
}
toc()

simOutput = temp_list %>% plyr::ldply()

#simOutput %>% write_clip

overview <- simOutput %>%
        dplyr::mutate(profit_flag = case_when(net_value >0 ~ "win", TRUE ~ "loss")) %>%
        group_by(symbol, profit_flag) %>%
        summarise(total = sum(net_value)) %>%
        tidyr::spread(profit_flag, total) %>%
        dplyr::mutate(win = dplyr::case_when(is.na(win) ~ 0, TRUE ~ win),
                      loss = dplyr::case_when(is.na(loss) ~ 0, TRUE ~ loss),
                      ratio = abs(win / loss),
                      net = win + loss) %>%
        arrange(desc(ratio)) %>%
        ungroup

overview
# win
w = overview$win %>% sum(); w
# loss
lo = overview$loss %>% sum(); lo
# ratio
rat = abs(overview$win %>% sum() / overview$loss %>% sum()); rat

############################################################################################
temp_list2 = vector(mode = "list", length = length(symbols))
yr = 2023

tic()
for(i in 1:length(symbols)){
        
        s = symbols[i]
        
        x <- poc %>%
                dplyr::filter(year >= yr & 
                                      symbol == s) %>%
                dplyr::select(symbol, date, open, high, low, close, message) %>%
                dplyr::mutate(message = case_when(message == 1 ~ "buy",
                                                  message == 0 ~ "hold",
                                                  message == -1 ~ "sell"))
        
        y1 <- strategyEval(fund_begin = 1000, x) %>% .$net_value
        
        z <- data.frame(symbol = s,
                        year = yr,
                        net_value = y1)
        
        temp_list2[[i]] = z        
}
toc()

simpoc2 = temp_list2 %>% plyr::ldply()

#simpoc2 %>% write_clip

simpoc2 %>% arrange(desc(net_value))

# win
w2 = sum(simpoc2$net_value[simpoc2$net_value >0]); w2
# loss
lo2 = sum(simpoc2$net_value[simpoc2$net_value <0]); lo2
# ratio
rat2 = abs(w2 / lo2); rat2



########################################################################
########################################################################


