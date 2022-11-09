# creation of df5_detail, df5_simplified

##################################################################################################
#write.table(x, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)
##################################################################################################
############### backtesting

# df5a - data prep for backtesting
df5a <- dplyr::inner_join(df3, 
                          df4 %>% dplyr::select(symbol, date, 
                                                rf_v1_sig_flag = signal_v1,
                                                rf_v2_sig_flag = signal_v2,
                                                down_prob, stay_prob, up_prob),
                          by = c("symbol", "date")) %>%
    arrange(symbol, date)

dim(df5a)
str(df5a)
names(df5a)
# [1] "symbol"                   "date"                     "open"                     "high"                    
# [5] "low"                      "close"                    "adjusted"                 "volume"                  
# [9] "target_var"               "target_flag"              "nlcor_n30"                "macd_zone_in_out_flag"   
# [13] "rsi_zone_in_out_flag"     "cci_zone_in_out_flag"     "ha_zone_in_out_flag"      "macd_zone_direction_flag"
# [17] "cci_zone_direction_flag"  "rsi_zone_direction_flag"  "ha_zone_direction_flag"   "obv_flag"                
# [21] "overnight_flag"           "ema_flag"                 "sma_flag"                 "up_count"                
# [25] "down_count"               "market_sig_flag"          "macd_sig_flag"            "rsi_sig_flag"            
# [29] "cci_sig_flag"             "ha_sig_flag"              "rf_v1_sig_flag"           "rf_v2_sig_flag"          
# [33] "down_prob"                "stay_prob"                "up_prob"   

####################################################
########## for() loop init for backtesting ###############################
####################################################
list_of_symbols = unique(df5a$symbol)
eval_list <- vector(mode = "list", length = length(list_of_symbols))

tic()

for(i in 1:length(list_of_symbols)){
    
    s = list_of_symbols[i]
    
    sDf <- df5a %>%
        dplyr::filter(symbol == s) %>%
        dplyr::select(symbol, date, open, close = adjusted, contains("_sig_flag")) %>%
        dplyr::select(-market_sig_flag) %>%
        tidyr::gather(., "flag", "message", -symbol, -date, -open, -close) %>%
        dplyr::mutate(message = dplyr::case_when(message == -1 ~ 'sell',
                                                 message == 0 ~ 'hold', 
                                                 message == 1 ~ 'buy')) %>%
        arrange(symbol, flag, date)
    
    flags <- unique(sDf$flag)
    
    tempList <- vector(mode = "list", length = length(flags))
    
    for(j in 1:length(flags)){
        
        evalObj <- sDf %>%
            dplyr::filter(flag == flags[j]) %>%
            dplyr::select(-flag) %>%
            strategyEval(fund_begin = 10000, amountInvestedMax = 1000000) %>%
            .$evalDf %>%
            dplyr::mutate(flag = flags[j]) %>%
            dplyr::select(symbol, date, flag, fund, amountInvested, share, value, net_value)
        
        tempList[[j]] <- evalObj

    }
    
    tempDf <- tempList %>% 
        dplyr::bind_rows() %>%
        tidyr::gather(key, value, fund:net_value) %>%
        dplyr::mutate(flag = gsub(pattern = "_sig_flag", replacement = "", x = flag)) %>%
        tidyr::unite(col = "keys", c("flag", "key"), sep = "_") %>%
        tidyr::spread(keys, value) 
    
    eval_list[[i]] <- tempDf
    
    # print status
    print(paste0(i, ". ", list_of_symbols[i], " fimished at ", Sys.time()))
    
}

toc()

######################################################################
############### whne you
# df5b - detail backtesting result
df5b <- eval_list %>%
    dplyr::bind_rows() %>%
    dplyr::inner_join(., df5a %>%
                          dplyr::select(symbol, date, 
                                        market_sig_flag:up_prob),
                      by = c("symbol", "date")) %>%
    dplyr::inner_join(., df3 %>% 
                          dplyr::select(-contains("sig_flag")),
                      by = c("symbol", "date")) %>% 
    dplyr::select(wrapr::qc(symbol,
                            date,
                            open,
                            high,
                            low,
                            close,
                            adjusted,
                            volume,
                            target_var,
                            target_flag,
                            market_sig_flag,
                            nlcor_n30,
                            obv_flag,
                            overnight_flag,
                            ema_flag,
                            sma_flag,
                            up_count,
                            down_count,
                            cci_zone_in_out_flag,
                            cci_zone_direction_flag,
                            cci_sig_flag,
                            cci_amountInvested,
                            cci_fund,
                            cci_net_value,
                            cci_share,
                            cci_value,
                            ha_zone_in_out_flag,
                            ha_zone_direction_flag,
                            ha_sig_flag,
                            ha_amountInvested,
                            ha_fund,
                            ha_net_value,
                            ha_share,
                            ha_value,
                            macd_zone_in_out_flag,
                            macd_zone_direction_flag,
                            macd_sig_flag,
                            macd_amountInvested,
                            macd_fund,
                            macd_net_value,
                            macd_share,
                            macd_value,
                            rsi_zone_in_out_flag,
                            rsi_zone_direction_flag,
                            rsi_sig_flag,
                            rsi_amountInvested,
                            rsi_fund,
                            rsi_net_value,
                            rsi_share,
                            rsi_value,
                            rf_v1_sig_flag,
                            down_prob,
                            stay_prob,
                            up_prob,
                            rf_v1_amountInvested,
                            rf_v1_fund,
                            rf_v1_net_value,
                            rf_v1_share,
                            rf_v1_value,
                            rf_v2_sig_flag,
                            rf_v2_amountInvested,
                            rf_v2_fund,
                            rf_v2_net_value,
                            rf_v2_share,
                            rf_v2_value))
                      
# buy and hold strategy
buy_and_hold <- df5b %>%
    group_by(symbol) %>%
    summarise(min_date = min(date),
              max_date = max(date)) %>%
    ungroup() %>%
    tidyr::gather("key", "date", -symbol) %>%
    dplyr::inner_join(., df5a %>%
                          dplyr::select(symbol, date, adjusted),
                      by = c("symbol", "date")) %>%
    dplyr::select(-date) %>%
    tidyr::spread(key, adjusted) %>%
    dplyr::mutate(buy_and_hold_net_value = 10000 * (max_date - min_date) / min_date) %>%
    dplyr::select(symbol, min_date_adjusted = min_date, max_date_adjusted = max_date, buy_and_hold_net_value) %>%
    dplyr::inner_join(., df5b %>%
                          group_by(symbol) %>%
                          summarise(min_date = min(date),
                                    max_date = max(date)) %>%
                          ungroup(),
                      by = "symbol") %>%
    dplyr::select(symbol, min_date, max_date, 
                  min_date_adjusted, max_date_adjusted,
                  buy_and_hold_net_value) %>%
    arrange(desc(buy_and_hold_net_value))
    
# df5c - simplified backtesting result
df5c <- df5b %>%
    dplyr::inner_join(., df5b %>%
                          group_by(symbol) %>%
                          summarise(date = max(date)) %>%
                          ungroup,
                      by = c("symbol", "date")) %>%
    dplyr::select(symbol, date, contains("net_value")) %>%
    group_by(symbol) %>%
    dplyr::mutate(avg_net_value = mean(c_across(contains("net_value")))) %>%
    ungroup() %>%
    dplyr::inner_join(., buy_and_hold, by = "symbol") %>%
    dplyr::select(symbol,
                  min_date, max_date,
                  min_date_adjusted, max_date_adjusted,
                  everything()) %>%
    dplyr::select(-date) %>%
    arrange(desc(avg_net_value))
    
############################################
#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

###### write table
# backtesting - detail
tbl_bt_detail_name = "strategy_final_project_df5_backtest_detail"
tbl_bt_detail = paste0(schema, ".", tbl_bt_detail_name)
tbl_bt_detail

# backtesting - simplified
tbl_bt_simplified_name = "strategy_final_project_df5_backtest_simplified"
tbl_bt_simplified = paste0(schema, ".", tbl_bt_simplified_name)
tbl_bt_simplified

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl_bt_detail), df5b, overwrite = TRUE)
DBI::dbWriteTable(con, DBI::SQL(tbl_bt_simplified), df5c, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl_bt_detail}"))
dbGetQuery(con, glue::glue("select count(1) from {tbl_bt_simplified}"))

# disconnect db
dbDisconnect(con) 
















