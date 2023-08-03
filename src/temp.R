# initiate set up
source("~/algorithmic_trading/config/setup.R")
readRenviron("~/algorithmic_trading/config/.env")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
av_api_key(ALPHA_VANTAGE_API)

# symbols
symbols = c("SPY", "GOOGL", "META", "AMC", "TSLA")

# get data
df = symbols %>%
        tq_get(get = "alphavantage", 
               av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
               outputsize = "full")     

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

##########################################################################################################
# individual symbol
# parameters
windows()
# TA <- function(xtsList, 
#                start_day_minus = 730, 
#                end_day_minus = 0,
#                start_datekey,
#                end_datekey,
#                input_datekey_flag = FALSE,
#                save_plot_path = "~",
#                folder_name = "TA charts",
#                heikin_ashi = FALSE
# )

s = 'GOOGL'
start_date = '2022-10-01'
#end_date = '2020-10-10'
end_date = Sys.Date()

s1 = df3 %>%
        dplyr::filter(symbol == s) %>%
        dplyr::select(date, 
                      open = adj_open,
                      high = adj_high,
                      low = adj_low,
                      close = adj_close,
                      volume) %>%
        dplyr::filter(date >= start_date & 
                              date <= end_date)

x = timetk::tk_xts(s1)

x2 = x %>% heikin_ashi(., output_as_df = TRUE) %>%
        dplyr::mutate(date = as.Date(date, '%Y-%m-%d')) %>%
        inner_join(s1 %>% select(date, volume), by = "date") %>%
        timetk::tk_xts()

x2 %>%
        quantmod::chartSeries( name = paste0(s),
                               TA = c(addBBands(draw = 'bands'), 
                                      addMACD(),
                                      addZLEMA(col = 'purple'),
                                      addEVWMA(),
                                      #add_VWAP(),
                                      addADX(), 
                                      addCCI(), 
                                      addRSI()
                                      #addSMA(n = 50, col = 'red'),
                                      #addSMA(n = 200, col = 'blue')
                                      #addEMA(n = 10, col = 'red'),
                                      #addEMA(n = 30, col = 'blue')                                              
                               ))

# add Chandelier
addTA(chandelier(x), on = 1)

##########################################################################################################
# save as bunch of symbols

#focus_group = c()

df4 = df3 %>% 
        # filter(symbol %in% focus_group) %>%
        dplyr::select(symbol, date, 
                      open = adj_open,
                      high = adj_high,
                      low = adj_low,
                      close = adj_close,
                      volume) %>%
        arrange(symbol, date)

chart_symbols = df4$symbol %>% unique() %>% sort()
xtsList = lapply(1:length(df4$symbol %>% unique), function(x)timetk::tk_xts(df4 %>% filter(symbol == chart_symbols[x])))
names(xtsList) = chart_symbols

TA(xtsList, 
   start_day_minus = 200, 
   save_plot_path = PLOT_DIRECTORY, 
   folder_name = TA_FOLDER, 
   heikin_ashi = FALSE)



















