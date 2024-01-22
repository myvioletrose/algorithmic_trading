# initiate set up
source("~/algorithmic_trading/config/setup.R")
readRenviron("~/algorithmic_trading/config/.env")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
av_api_key(ALPHA_VANTAGE_API)

# symbols
symbols = c("SPY", "SNAP", "FLT", "PHM", "META")

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

# begin transformation
t0 <- df3 %>%
        dplyr::filter(symbol %in% symbols) %>%
        dplyr::select(symbol,
                      date, 
                      open = adj_open,
                      high = adj_high,
                      low = adj_low,
                      close = adj_close,
                      volume) %>%
        dplyr::arrange(symbol, date)        

# TA()
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

####################################################################
################# < individual chart > ###############################################

# window
windows()

# parameters
s = "CZR"

# look back: 504 (24M) / 252 (12M) / 189 (9M) / 126 (6M) / 63 (3M)
days_look_back = 252
end_date = Sys.Date()
#end_date = "2014-01-02"

unique_trading_date = t0 %>% select(date) %>% arrange(date) %>% distinct %>% .$date
target_date_index = which(unique_trading_date == end_date)
if(length(target_date_index) == 0){target_date_index = length(unique_trading_date)}
start_date = unique_trading_date[target_date_index -days_look_back]
start_date = as.Date(start_date)

#start_date = "2022-06-01"
#end_date = "2023-03-01"
print(start_date)
print(end_date)

# # save plot path, name
# plot_path = PLOT_DIRECTORY
# plot_name = paste0(s, "_", 
#                    gsub(pattern = "-", start_date, replacement = ""), 
#                    "_to_", 
#                    gsub(pattern = "-", end_date, replacement = ""), 
#                    ".png")

# xts object
c1 = t0 %>%
        dplyr::filter(symbol == s) %>%
        dplyr::filter(date >= start_date &
                              date <= end_date) %>%
        timetk::tk_xts()

# xts object - Heikin Ashi
c1ha = c1 %>% 
        heikin_ashi(., output_as_df = TRUE) %>%
        dplyr::mutate(date = as.Date(date, '%Y-%m-%d')) %>%
        inner_join(t0 %>% filter(symbol == s) %>% select(date, volume), by = "date") %>%
        timetk::tk_xts()

# the basic chart
v1 <- function(xts_type_index){
        switch(xts_type_index, c1, c1ha) %>%
                quantmod::chartSeries( name = s,
                                       #name = paste0(s, " (", start_date, " to ", end_date, ")"),
                                       TA = c(addBBands(draw = "bands"), 
                                              addMACD()
                                              #addEMA(n = 5, col = "red"),
                                              #addEMA(n = 20, col = "blue"),
                                              #addCCI(), 
                                              #addRSI(),
                                              #addZLEMA(col = "purple"),
                                              #addEVWMA(col = "darkorange"),
                                              #addADX(),
                                              #addOBV(),
                                              #addVo(),
                                              #addSAR(),
                                              #addZigZag(col = "yellow"),
                                       )
                )
}

########################### add indicators #####################################################
# visual 1 - trend, overbought/oversold
v1(1)
addEMA(n = 5, col = "red")
addEMA(n = 20, col = "blue")
addSMA(n = 50, col = "white")
addCCI() 
addRSI()
#addROC(n = 5, col = "yellow")
zooom()

# visual 2 - Heikin Ashi
v1(2)
addSAR()
addZLEMA(col = "purple")
#addEVWMA(col = "darkorange")
addCCI() 
addRSI()
zooom()

# visual 3 - price, resistance/support
v1(1)
#addTA(chandelier(c1, coef = 2, trend = "up"), col = "darkgreen", on = 1, lty = "longdash")
#addTA(chandelier(c1, coef = 2, trend = "down"), col = "darkviolet", on = 1, lty = "longdash")
addEVWMA(col = "darkorange")
addZLEMA(col = "purple")
#addZigZag(col = "yellow")
zooom()

# visual 4 - Chandelier Exit
v1(1)
addSAR()
addCCI() 
addTA(chandelier(c1, coef = 3, trend = "up"), col = "darkgreen", on = 1, lty = "longdash")
#addTA(chandelier(c1, coef = 3, trend = "down"), col = "darkviolet", on = 1, lty = "longdash")
#addEVWMA(col = "darkorange")
zooom()

# visual 5 - "the chart"
v1(1)
addSMA(n = 5, col = "red")
addEVWMA(col = "darkorange")
addZLEMA(col = "purple")
addCCI()
#addRSI()
#addVo()
addOBV()
zooom()

################### additonal features
#zoomChart("2022-10::")
#zooom()

#png(filename = paste(plot_path, plot_name, sep = "/"), width = 1500, height = 1200, res = 100)
#dev.off()

#setwd()
saveChart(.type = "png", width = 1800, height = 1500, res = 150)

#################################################################################################################################################
####################################################################
################# < mass downloads > ###############################################

# function to save all plots
TA2 <- function(df, s, start_date, end_date, version, xts_type_index, save_plot_path, folder_name){
        
        # xts object
        c1 = df %>%
                dplyr::filter(symbol == s) %>%
                dplyr::filter(date >= start_date &
                                      date <= end_date) %>%
                timetk::tk_xts()
        
        # xts object - Heikin Ashi
        c1ha = c1 %>% 
                heikin_ashi(., output_as_df = TRUE) %>%
                dplyr::mutate(date = as.Date(date, '%Y-%m-%d')) %>%
                inner_join(df %>% filter(symbol == s) %>% select(date, volume), by = "date") %>%
                timetk::tk_xts()
        
        # path for saving all the TA charts
        path <- paste(save_plot_path, folder_name, sep = "/")
        
        # delete folder if exists and then create one again; create it if it does not exist
        #if(dir.exists(path)){unlink(path, recursive = TRUE); dir.create(path)} else {dir.create(path)}
        
        # create dir if it does not exist
        if(!dir.exists(path)){dir.create(path)}
        
        # version 1
        if(version == 1){
                name = paste(s, "_v1.png", sep = "")
                png(filename = paste(path, name, sep = "/"), width = 1800, height = 1200, res = 120)
                switch(xts_type_index, c1, c1ha) %>%
                        quantmod::chartSeries( name = s,
                                               TA = c(addBBands(draw = "bands"), 
                                                      addMACD(),
                                                      addEMA(n = 5, col = "red"),
                                                      addEMA(n = 20, col = "blue"),
                                                      addZLEMA(col = "purple"),
                                                      addCCI(),
                                                      addRSI()
                                               )
                        )                
                dev.off()
        }
        
        # version 2
        if(version == 2){
                name = paste(s, "_v2.png", sep = "")
                png(filename = paste(path, name, sep = "/"), width = 1800, height = 1200, res = 120)
                switch(xts_type_index, c1, c1ha) %>%
                        quantmod::chartSeries( name = s,
                                               TA = c(addBBands(draw = "bands"), 
                                                      addMACD(),
                                                      addSAR(),
                                                      #addEVWMA(col = "darkorange"),
                                                      addZLEMA(col = "purple"),
                                                      addCCI(),
                                                      addRSI()
                                               )
                        )
                dev.off()
        }
        
        # version 3
        if(version == 3){
                name = paste(s, "_v3.png", sep = "")
                png(filename = paste(path, name, sep = "/"), width = 1800, height = 1200, res = 120)
                try(
                        switch(xts_type_index, c1, c1ha) %>%
                                quantmod::chartSeries( name = s,
                                                       TA = c(addBBands(draw = "bands"), 
                                                              addTA(chandelier(c1, coef = 2, trend = "up"), col = "darkgreen", on = 1, lty = "longdash"),
                                                              #addTA(chandelier(c1, coef = 2, trend = "down"), col = "darkviolet", on = 1, lty = "longdash"),
                                                              addMACD(),
                                                              addEVWMA(col = "darkorange")
                                                              #addZLEMA(col = "purple"),
                                                       )
                                ),
                        silent = TRUE
                )
                dev.off()
        }         
        
        # version 4
        if(version == 4){
                name = paste(s, "_v4.png", sep = "")
                png(filename = paste(path, name, sep = "/"), width = 1800, height = 1200, res = 120)
                switch(xts_type_index, c1, c1ha) %>%
                        quantmod::chartSeries( name = s,
                                               TA = c(addBBands(draw = "bands"), 
                                                      addMACD(),
                                                      addSAR(),
                                                      addCCI(),
                                                      addEVWMA(col = "darkorange")
                                                      #addTA(chandelier(c1, coef = 2, trend = "up"), col = "darkgreen", on = 1, lty = "longdash")
                                                      #addTA(chandelier(c1, coef = 2, trend = "down"), col = "darkviolet", on = 1, lty = "longdash")
                                               )
                        )
                dev.off()
        }
        
        # version 5
        if(version == 5){
                name = paste(s, "_v5.png", sep = "")
                png(filename = paste(path, name, sep = "/"), width = 1800, height = 1200, res = 120)
                switch(xts_type_index, c1, c1ha) %>%
                        quantmod::chartSeries( name = s,
                                               TA = c(addBBands(draw = "bands"), 
                                                      addSMA(n = 5, col = "red"),
                                                      addEVWMA(col = "darkorange"),
                                                      addZLEMA(col = "purple"),
                                                      addOBV(),
                                                      addMACD(),
                                                      addCCI()
                                                      #addRSI(),
                                                      #addVo(),                                                      
                                               )
                        )
                dev.off()
        }
        
}

###############################################################################
# look back: 504 (24M) / 252 (12M) / 189 (9M) / 126 (6M) / 63 (3M)
days_look_back = 252
days_look_back_flagged_buy_signal = 10
end_date = t0$date %>% max() 

unique_trading_date = t0 %>% filter(symbol == "SPY") %>% select(date) %>% arrange(date) %>% distinct %>% .$date
target_date_index = which(unique_trading_date == end_date)
if(length(target_date_index) == 0){target_date_index = length(unique_trading_date)}
start_date = unique_trading_date[target_date_index -days_look_back]
start_date = as.Date(start_date)

from_date = unique_trading_date[target_date_index -days_look_back_flagged_buy_signal]
from_date = as.Date(from_date)

# symbols signal buy
# s = poc %>% filter(grepl("buy", message_b, ignore.case = TRUE)) %>%
#         filter(date >= from_date & date <= end_date) %>%
#         #inner_join(first_buy, by = c("symbol" = "symbol", "date" = "first_buy_date")) %>%
#         filter(cci_overbought_flag == 0) %>%
#         .$symbol %>%
#         unique()
s = t0$symbol %>% unique()
#s = highlight_symbols2
s

# s = c('AIZ', 
#       'CNC', 
#       'J', 
#       'PTC', 
#       'SPY', 
#       'SNAP', 
#       'EBAY', 
#       'FRT', 
#       'FLT', 
#       'MRK')

#start_date = "2022-06-01"
#end_date = "2023-03-01"
print(start_date)
print(from_date)
print(end_date)

# charts parameters
#version = c(1, 2, 3, 4, 5)
#xts_type = c(1, 2, 1, 1, 1)
#version = c(2, 5)
#xts_type = c(2, 1)

version = c(5)
xts_type = c(1)
folder_name = "v5"
#folder_name = "highlight_symbols"
para_df <- data.frame(ver = version,
                      xts_type_ind = xts_type)

# for() loop
tic()
for(i in 1:length(s)){
        
        sapply(1:nrow(para_df), function(x) {TA2(t0,
                                                 s[i], 
                                                 start_date = start_date, 
                                                 end_date = end_date, 
                                                 version = para_df$ver[x], 
                                                 xts_type_index = para_df$xts_type_ind[x],
                                                 #folder_name = s[i],
                                                 folder_name = folder_name,
                                                 save_plot_path = PLOT_DIRECTORY
        )}) %>% 
                invisible()
        
        print(paste0(s[i], " done!"))
        
}
toc()