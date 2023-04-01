tic()

temp_measures <- vector(mode = "list", length = length(XTSLIST))

for(i in 1:length(XTSLIST)){
    temp1 <- XTSLIST[[i]] %>%
        historicalTransformation(., transformOnly = TRUE) %>%
        dplyr::select(symbol, date, 
                      adjusted,
                      adj_lag_1,
                      adj_lag_2,
                      intraday_volatility,
                      cci = cci_close, 
                      rsi = rsi_close, 
                      macd = macd_close, 
                      macd_signal = signal_close, 
                      obv = obv_close) %>%
        dplyr::mutate(pct = (adjusted - adj_lag_1) / adj_lag_1,
                      pct2 = (adjusted - adj_lag_2) / adj_lag_2) %>%
        # very important for calculating SMA and EMA by having asec(date)
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(SMA_5 = pracma::movavg(adjusted, n = 5, type = "s"),
                      EMA_10 = pracma::movavg(adjusted, n = 10, type = "e"),
                      EMA_20 = pracma::movavg(adjusted, n = 20, type = "e"),
                      EMA_30 = pracma::movavg(adjusted, n = 30, type = "e"),
                      EMA_40 = pracma::movavg(adjusted, n = 40, type = "e"),
                      EMA_50 = pracma::movavg(adjusted, n = 50, type = "e"),
                      EMA_100 = pracma::movavg(adjusted, n = 100, type = "e"),
                      EMA_200 = pracma::movavg(adjusted, n = 200, type = "e")) %>%
        ungroup %>%
        ############################### a new target variable ###############################
    arrange(symbol, desc(date)) %>%
        group_by(symbol) %>%
        dplyr::mutate(index = row_number()) %>%
        dplyr::mutate(target_flag = dplyr::case_when(adjusted > SMA_5 ~1, TRUE~0),
                      target_var = pracma::movavg(target_flag, n = 5, type = "s"),
                      target_flag = dplyr::case_when(target_var >= 0.8 ~1, 
                                                     target_var <= 0.2 ~-1,
                                                     TRUE ~0)) %>%
        ungroup %>%
        arrange(symbol, date) %>%
        dplyr::mutate(temp_flag = index>=5,
                      target_var = dplyr::case_when(temp_flag == 1 ~ target_var),
                      target_flag = dplyr::case_when(temp_flag == 1 ~ target_flag))
    
    row.names(temp1) <- NULL
    
    temp2 <- temp1 %>%
        dplyr::select(-pct) %>%
        dplyr::inner_join(., rolling_window(temp1, col = "pct"), by = "date")
    
    temp_measures[[i]] <- temp2
    
}

temp_measures_Df <- temp_measures %>% dplyr::bind_rows()
temp_measures_Df %>% dim()
names(temp_measures_Df) <- tolower(names(temp_measures_Df))
    
toc()

#write.table(test, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)

################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

############### stock.stock_indicator

# write table
tbl_name = "stock_indicator"
tbl = paste0(schema, ".", tbl_name)
DBI::dbWriteTable(con, DBI::SQL(tbl), temp_measures_Df, overwrite = TRUE)

query = glue::glue("
select count(1)
from {tbl}
--where symbol = 'SPY'
")

test = dbGetQuery(con, query)

# disconnect db
dbDisconnect(con) 
