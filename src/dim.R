# initiate set up
source("~/algorithmic_trading/config/setup.R")

# parameters
APPEND = TRUE

# schema, table names
schema = "stock"
tbl_name = "dim"
tbl = paste0(schema, ".", tbl_name)

###############################################################################
# read csv
dimDf = read.csv(STOCK_DIM_PATH, header = TRUE)

#tq_index_options()
#tq_index("SP500") %>% View()

# filter data
dimDf2 <- dimDf %>%
    dplyr::filter(is.na(last_runtime) | nchar(last_runtime)==0) %>%
    dplyr::mutate(last_runtime = Sys.time())

dimDf2

# create connection
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)

# delete symbol(s), if any, from existing table
s = dimDf2$symbol %>% unique()
if(length(s)>0){
    
    s.update = sapply(1:length(s), function(x) paste0("'", s[x], "'")) %>% paste0(collapse = ", ")
    
    dbGetQuery(con, statement = glue::glue("delete from {tbl} where symbol in ({s.update})"))
    
}

# write table
if(APPEND){
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      dimDf2, 
                      append = TRUE)
} else {
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      dimDf2, 
                      overwrite = TRUE)
}

# check table
dbGetQuery(con, statement = glue::glue("select count(1) from {tbl}"))
dbGetQuery(con, statement = glue::glue("select symbol, count(1) from {tbl} group by 1 having count(1)>1 limit 5"))

#dbListTables(con)
dbGetQuery(con, glue::glue("SELECT 
   table_name, 
   column_name, 
   data_type 
FROM 
   information_schema.columns
WHERE 
   table_name = '{tbl_name}';"))

# write output table (overwrite csv)
dimDf3 = dbGetQuery(con, statement = glue::glue("select symbol, last_runtime from {tbl} order by symbol"))

dimDf <- dimDf %>%
    dplyr::select(-last_runtime) %>%
    dplyr::left_join(., dimDf3, by = "symbol")

write.csv(dimDf, STOCK_DIM_PATH, append = FALSE, row.names = FALSE)

# disconnect db
dbDisconnect(con) 
