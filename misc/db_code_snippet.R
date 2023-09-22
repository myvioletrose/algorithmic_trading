### code snippet

# schema, table names
schema = "adhoc"
tbl_name = "poc"
tbl = paste0(schema, ".", tbl_name)
DB = "stg"

# create connection
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)

# save_tbl_action
dim(poc)  # [1] 2648034      43
tic()
save_tbl_action(tbl, poc, overwrite = TRUE)
toc()

# check table
d(glue::glue("select count(1) from {tbl}"))

# alternatively,
if(APPEND){
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      df, 
                      append = TRUE)
} else {
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      df, 
                      overwrite = TRUE)
}

# disconnect db
dbDisconnect(con)