# initiate set up
source("~/algorithmic_trading/config/setup.R")
readRenviron("~/algorithmic_trading/config/.env")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
av_api_key(ALPHA_VANTAGE_API)

# set directory
setwd("src/prod/trading_2.0")

# overwrite the watchlist csv file or not
overwrite_watchlist_yn = FALSE

################################################ part I ###################################################################
############# > source indicators.R, messages.R
# current holding stocks
current_stocks = c("SNAP", "TSLA", "FLT", "PHM", "META")

# watchlist
wl = openxlsx::loadWorkbook(WATCHLIST_PATH)
watchlist_symbols = readWorkbook(wl, "watchlist") %>% select(symbol) %>% distinct() %>% .$symbol

# symbols
symbols = c("META", "AAPL", "AMZN", "NFLX", "GOOGL", "TSLA", "SPY", "QQQ", "GLD",
            "DKNG", "ANET", "CZR", "DASH", "JOBY", 
            "TGT", "ZS", "CRM", "EXPE", "PLNT",
            "SHOP", "NOW",
            "SNOW", "MDB", "CRWD", "SYM", "SNPS", "PATH",
            current_stocks) %>%
        sort()

sp500 = tq_index("SP500")
symbols = c("SPY",
            sp500$symbol,
            watchlist_symbols,
            symbols) %>%
        unique() %>%
        sort()

# subset data, symbol (for poc, smaller subset faster processing but less data for strategy evaluation)
subset_date = "2021-01-01"
subset_symbols = c(symbols, "BTC-USD") %>% sort()

tic("<<< ETL >>>")

source("a. indicators.R")
source("b. messages.R")

toc()

##################################### part II ###############################################
############## > backtest YTD
# look back: 504 (24M) / 252 (12M) / 189 (9M) / 126 (6M) / 63 (3M)
backtest_ytd_symbols = poc$symbol %>% unique()
#backtest_ytd_symbols = "SNAP"
backtest_days_look_back = 126
backtest_end_date = poc$date %>% max() 

unique_trading_date = poc %>% filter(symbol == "SPY") %>% select(date) %>% arrange(date) %>% distinct %>% .$date
end_date_index = which(unique_trading_date == backtest_end_date)
if(length(end_date_index) == 0){end_date_index = length(unique_trading_date)}
backtest_start_date = unique_trading_date[end_date_index -backtest_days_look_back]
backtest_start_date = as.Date(backtest_start_date)

#backtest_start_date
#backtest_end_date
print(backtest_start_date)
print(backtest_end_date)

tic()
source("c. backtest_ytd.R")
toc()

################################## part III ############################################
############### > backtest random evaluation
seed = 8892
n = 10
days_after_signal = 10
symbol_to_study = poc$symbol %>% unique()
rand_date_start = '1990-01-01'
rand_date_end = '2023-12-31'
first_buy_only = FALSE

tic()
source("d. backtest_rand.R")
toc()

################################## part IV #####################################
################## > equity trading analysis curve
# # date parameters
# etca_start = Sys.Date()
# etca_end = Sys.Date()
# etca_look_back_period = 63
# 
# # symbols to eval
# symbols_to_eval = poc$symbol %>% unique()
# #symbols_to_eval = c("SPY", "TSLA")
# 
# # message to eval
# message_to_eval = "message_s"
# 
# tic()
# source("e. backtest_etca.R")
# toc()
# 
# # etca_df2
# etca_df2 <- etca_df %>%
#         inner_join(poc %>% select(symbol, date, close, 
#                                   matches("message_[bs]"),
#                                   matches("message_e([[:digit:]]$)")), 
#                    by = c("symbol" = "symbol", "end_date" = "date")) %>%
#         select(symbol, year, date, index, start_date, end_date, 
#                close, starts_with("message_"),
#                everything()) %>%
#         arrange(symbol, index, end_date)

#######################################################################################
# major output
#indicators
#indicators_transformed
#target
#targetSubset2  # with daily_support_e, daily_target_e
#poc

##############################################
# major backtest output - YTD
#simpoc
#simpoc2  # <pay attention>
#simpoc3

# major backtest output - rand
#backtest_rand_evalDf  <pay attention>
#msg_summary
#overall_compare  # <pay attention>
#session_compare
#msg_compare  # <pay attention>
#msg_compare2

# major backtest output - etca
#date_df
#etca_df  # <pay attention>

####################################################################################################################################################
########################################################################################################
# overwrite watchlist or not
print(paste0("overwrite_watchlist_yn: ", overwrite_watchlist_yn))

if(overwrite_watchlist_yn){

        watchlist_today = poc %>%
                filter(is_today == 1 & 
                               #is_first_buy_yn == 1 &
                               (
                                       str_detect(tolower(message_b), 'buy') 
                               )
                ) %>%
                select(symbol) %>%
                inner_join(poc %>%
                                   arrange(symbol, desc(date)) %>%
                                   group_by(symbol) %>%
                                   mutate(index = row_number()) %>%
                                   ungroup() %>%
                                   filter(index <= 20) %>%
                                   select(symbol, date, is_today, 
                                          close, today_support, support, 
                                          message_s, message_e0, message_e1, message_e2, 
                                          rsi, cci, 
                                          sma5, sma50, sma200, ema5, ema20, 
                                          cci_oversold_flag, 
                                          rsi_oversold_flag, 
                                          obv_flag, 
                                          demark_flag, 
                                          ce_short_spike_flag, 
                                          dcc_flag, evwma_flag, 
                                          ha_real_flag, 
                                          ha_smooth_flag, 
                                          macd_flag, 
                                          sma5_flag),
                           by = "symbol", 
                           multiple = "all") %>%
                arrange(symbol, date)
        
} else {
        
        wl = openxlsx::loadWorkbook(WATCHLIST_PATH)
        existing_symbols_fr_watchlist = readWorkbook(wl, "watchlist") %>%
                select(symbol) %>%
                distinct() 
        today_symbols = poc %>%
                filter(is_today == 1 & 
                               #is_first_buy_yn == 1 &
                               (
                                       str_detect(tolower(message_b), 'buy') 
                               )
                ) %>%
                select(symbol)
        watchlist_today = rbind(existing_symbols_fr_watchlist, today_symbols) %>%
                distinct() %>%
                inner_join(poc %>%
                                   arrange(symbol, desc(date)) %>%
                                   group_by(symbol) %>%
                                   mutate(index = row_number()) %>%
                                   ungroup() %>%
                                   filter(index <= 20) %>%
                                   select(symbol, date, is_today, 
                                          close, today_support, support, 
                                          message_s, message_e0, message_e1, message_e2, 
                                          rsi, cci, 
                                          sma5, sma50, sma200, ema5, ema20, 
                                          cci_oversold_flag, 
                                          rsi_oversold_flag, 
                                          obv_flag, 
                                          demark_flag, 
                                          ce_short_spike_flag, 
                                          dcc_flag, evwma_flag, 
                                          ha_real_flag, 
                                          ha_smooth_flag, 
                                          macd_flag, 
                                          sma5_flag),
                           by = "symbol", 
                           multiple = "all") %>%
                arrange(symbol, date)
        
}

# print number of flagged symbols today
print(watchlist_today$symbol %>% unique() %>% length())

# print symbol(s)
highlight_symbols = watchlist_today$symbol %>% unique() %>% sort()
print(highlight_symbols)

# read template
wb <- openxlsx::loadWorkbook(WATCHLIST_TEMPLATE_PATH)

# write worksheet
openxlsx::writeData(wb = wb, 
                    sheet = "watchlist",
                    x = watchlist_today,
                    colNames = FALSE,
                    startRow = 2)

# save workbook
openxlsx::saveWorkbook(wb, WATCHLIST_PATH, overwrite = TRUE)


################################################# analysis begin here ###########################################
# simpoc2
# backtest_rand_evalDf
# overall_compare
# msg_compare
# etca_df

# daily highlight
highlight_symbols2 = c(highlight_symbols, current_stocks) %>% unique() %>% sort()

# quick take
quick_take <- poc %>%
        arrange(symbol, desc(date)) %>%
        group_by(symbol) %>%
        mutate(index = row_number()) %>%
        ungroup() %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        mutate(highlight_yn = case_when(symbol %in% highlight_symbols2 ~ 1,
                                        TRUE ~ 0),
               upside_opp = (support3_line - close) / close,
               downsize_risk = (stop_loss_base_line - close) / close,               
               close_lag1 = lag(close, 1),
               close_lag3 = lag(close, 3),
               close_lag5 = lag(close, 5),
               pct_chg1 = (close - close_lag1) / close_lag1,
               pct_chg3 = (close - close_lag3) / close_lag3,
               pct_chg5 = (close - close_lag5) / close_lag5,
               target_e0 = case_when(profit_target1_line < support1_line ~ profit_target1_line, TRUE ~ support1_line),
               target_e1 = profit_target2_line,
               target_e2 = profit_target3_line) %>%
        ungroup() %>%
        filter(index <= 20) %>%
        select(
                symbol,
                date,
                is_today,
                highlight_yn,
                rsi,
                cci,
                cci_oversold_flag,
                rsi_oversold_flag,
                obv_flag,
                demark_flag,
                ce_short_spike_flag,
                dcc_flag,
                evwma_flag,
                ha_real_flag,
                ha_smooth_flag,
                macd_flag,
                sma5_flag,
                volume,
                open,
                high,
                low,
                close,
                atr,
                sma5,
                sma50,
                sma200,
                ema5,
                ema20,
                zlema,
                evwma,
                chanExit_short,
                chanExit_long,
                macd_diff,
                red_flag,
                green_flag,
                pct_chg1,
                pct_chg3,
                pct_chg5,
                in_the_buy_yn,
                is_first_buy_yn,
                support1_line,
                support2_line,
                support3_line,
                profit_target1_line,
                profit_target2_line,
                profit_target3_line,
                stop_loss_base_line,
                trailing_stop_loss_yesterday,
                trailing_stop_loss,
                downsize_risk,
                upside_opp,
                message_b,
                message_s,
                message_e0,
                message_e1,
                message_e2,
                today_support,
                today_ceiling,
                support,
                target_e0,
                target_e1,
                target_e2
        ) %>%
        arrange(symbol, date)

############################
dim(quick_take)
quick_take %>% write_clip()