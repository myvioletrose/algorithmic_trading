# heikin_ashi - open, high, low, close
# intraday_volatility (close - open) / (high - low)
# rolling_window - intraday_volatility

tic()

# transform data
temp <- vector(mode = "list", length = length(XTSLIST))
for(i in 1:length(XTSLIST)){
    
    s <- stringr::str_extract_all(names(XTSLIST[[i]]), pattern = "^[[:alpha:]].*\\.") %>% 
        unlist %>% 
        stringr::str_to_lower(.) %>% 
        unique %>% 
        gsub("\\.", "", .) %>%
        toupper()
    
    temp_a <- XTSLIST[[i]] %>%
        heikin_ashi(., output_as_df = TRUE) %>%
        dplyr::mutate(symbol = s,
                      intraday_volatility_ha = (close - open) / (high - low)) %>%
        arrange(symbol, desc(date)) %>%
        dplyr::mutate(index = row_number()) %>%
        arrange(symbol, date)
    
    temp_b <- temp_a %>% rolling_window(., rolling_windows = 100, dataPartition = 2000, col = "intraday_volatility_ha")
    
    temp[[i]] <- dplyr::inner_join(temp_a, temp_b %>% dplyr::select(-intraday_volatility_ha), by = "date") %>%
        dplyr::select(symbol, date, open_ha = open, high_ha = high, low_ha = low, close_ha = close, 
                      intraday_volatility_ha, 
                      intraday_volatility_ha_mu_100, intraday_volatility_ha_sd_100, intraday_volatility_ha_percentile_100) %>%
        arrange(symbol, date)
    
}
tempDf <- temp %>% dplyr::bind_rows()
tempDf %>% dim()

######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

# write table
tbl_name = "heikin_ashi_fact"
tbl = paste0(schema, ".", tbl_name)
DBI::dbWriteTable(con, DBI::SQL(tbl), tempDf, overwrite = TRUE)

# data validation
query = glue::glue("
select count(1)
from {tbl}
")

dbGetQuery(con, query)
toc()

# disconnect db
dbDisconnect(con) 
