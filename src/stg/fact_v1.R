# initiate set up
source("~/algorithmic_trading/config/setup.R")

# Alpha Vantage API
av_api_key(ALPHA_VANTAGE_API)

# schema, table names
schema = "stock"
tbl_name = "fact"
tbl = paste0(schema, ".", tbl_name)

# create connection
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)

##############################################################################################################################
# get symbol, and then get data via API
get_data_from <- function(symbol = NA, process_time_from = NA, process_time_to = NA, status_from_dim = 1){
        # get symbol
        if(symbol %>% paste0(., collapse = ", ") == "NA"){              
                pull_status = status_from_dim 
                s = d(glue::glue("select distinct symbol 
                                 from stock.dim 
                                 where status = {pull_status} 
                                 order by symbol")) %>% .$symbol
        } else {
                s = symbol
        }
        
        # get data
        if(is.na(process_time_to)){process_time_to = Sys.Date()}
        if(is.na(process_time_from)){
                df = s %>%
                        tq_get(get = "alphavantage", 
                               av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
                               outputsize = "full")     
        } else {
                df = s %>%
                        tq_get(get = "alphavantage", 
                               av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
                               outputsize = "full") %>%
                        dplyr::filter(timestamp >= process_time_from & timestamp <= process_time_to)
        }
        
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
        
        return(df3)
}

# get, assign protocol values
get_protocol_value <- function(protocol_df, p){
        
        if(str_to_lower(p) %in% c("clean_slate_some", "delete_insert_update_some")){
                remove_cols = c("symbols_to_delete", "symbols_to_process")
        } else {
                remove_cols = NULL
        }
        
        df = protocol_df %>%
                dplyr::filter(protocol == str_to_lower(p)) %>%
                tidyr::gather() %>%
                dplyr::filter(key %nin% c(wrapr::qc(tbl, protocol, desc), remove_cols))
        
        # assign values to .GlobalEnv based on protocol
        sapply(1:nrow(df), function(x) assign(df$key[x], value = eval(parse(text = df$value[x])), envir = .GlobalEnv)) %>% invisible()
        
}

# print summary, get "signature_sign_off" for approval
input_value_signoff <- function(protocol_df, p, confirmed_with_signature = FALSE){
        
        if(!confirmed_with_signature){
                df = protocol_df %>%
                        dplyr::filter(protocol == str_to_lower(p)) %>%
                        tidyr::gather()
                
                cat(
                        cat(paste0(rep("#", 10), collapse = "")),
                        "> protocol: "
                ); cat(p)
                cat("\n")
                
                cat(
                        cat(paste0(rep("#", 10), collapse = "")),
                        "> description: "
                ); cat(df %>% dplyr::filter(key == "desc") %>% select(value) %>% .$value)
                cat("\n")
                
                cat(
                        cat(paste0(rep("#", 10), collapse = "")),
                        "> input protocol values: "
                ); cat("\n")
                
                df = df %>%
                        dplyr::filter(key %nin% c(wrapr::qc(tbl, protocol, desc)))
                
                values = sapply(1:nrow(df), function(x){
                        paste0(df$key[x], ": ", paste0(eval(sym(df$key[x])), collapse = ", "))
                })
                
                cat(paste0(values, collapse = "\n")); cat("\n")
                
                cat("\n")
                cat(
                        cat(paste0(rep("#", 10), collapse = "")),
                        paste0("> do you understand and agree with the setting? ", paste0(rep("#", 10), collapse = ""))
                ); cat("\n")
                
                cat("please sign off with initial as approval to run the program with the above selected protocol")
                cat("\n")
                
                if(!exists("JN")){JN <- "__"}
                
                assign("signature_sign_off", value = JN, envir = .GlobalEnv)
                cat(paste0("signature_sign_off: ", eval(signature_sign_off)))
        } else {
                if(!exists("JN")){JN <- "__"}
                
                assign("signature_sign_off", value = JN, envir = .GlobalEnv)
                cat(paste0("signature_sign_off: ", eval(signature_sign_off)))
        }
        
}

# main()
main <- function(){
        
        if(!exists("execute_pw")){
                cat("please enter a password to continue")
        } else {
                
                if(execute_pw != EXECUTE_PW){
                        cat("please enter the correct password for execution")
                        pass()
                } else {
                        if(str_to_lower(signature_sign_off) == "jn"){
                                # <<<<<<<<<<<<<<<<<<< steps >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> # 
                                # step 1: delete data
                                delete_action(tbl = tbl, symbol = symbols_to_delete, delete_time_from = delete_from, delete_time_to = delete_to, confirm_to_delete = confirm_to_delete); print("<<< delete action DONE >>>")
                                
                                # step 2: reload data
                                df <- get_data_from(symbol = symbols_to_process, process_time_from = process_from, process_time_to = process_to, status_from_dim = status_from_dim); print("<<< reload action DONE >>>")
                                
                                # step 3: save table 
                                save_tbl_action(tbl = tbl, df = df, overwrite = overwrite_yn); print("<<< save table action DONE >>>")

                                # remove credentials
                                JN <<- ""
                                execute_pw <<- ""
                                signature_sign_off <<- ""
                        } else {
                        cat("please confirm the protocol and sign off with initials before execution")
                        }
                }

        }
        
}

########################################################################################################################
### list of functions' arguments ###
# delete_action(
#         tbl,
#         symbol = NA,
#         delete_time_from = NA,
#         delete_time_to = NA,
#         confirm_to_delete = FALSE
#         )

# process_time_from(
#         tbl,
#         num_of_day = 0,
#         print_start_date = TRUE
#         )

# save_tbl_action(
#         tbl,
#         df,
#         overwrite = FALSE
#         )

# get_data_from(
#         symbol = NA, 
#         process_time_from = NA, 
#         process_time_to = NA, 
#         status_from_dim = 1
#         )

### protocol ###
# "clean_slate_all" - delete everything, reload everything (from dim)
# "clean_slate_some" - delete something, reload something
# "delete_insert_update_all" - delete (some old data), insert (some new data), update for all symbols
# "delete_insert_update_some" - delete (some old data), insert (some new data), update for some symbols

########################################################################################################################
### parameters needed

# symbols
symbols_to_delete = NA  # NA means to delete everything
symbols_to_process = NA  # NA means to reprocess everything from dim
status_from_dim = 1

# date_from, date_to
process_from = NA  # process from all time      
process_to = NA  # if NA, then Sys.Date()
delete_from = NA  # delete from all time
delete_to = NA  # if NA, then Sys.Date()

# actions
overwrite_yn = TRUE
confirm_to_delete = TRUE

###############################################################################################################
##################### begin execution below ###########################################################

# step 0: symbols
# symbols_to_delete = 
# symbols_to_process = 

# step 0: get protocol value
protocol_df = readxl::read_excel(ETL_PROTOCOL, sheet = "fact")
p = "clean_slate_all"

# step 1: assign values to global environment based on reading from the protocol xlsx
get_protocol_value(protocol_df, p)

# step 2a: print summary of the protocol values
input_value_signoff(protocol_df, p)

# step 2b: sign off with initial as approval to continue running the protocol
JN <<- ""
input_value_signoff(confirmed_with_signature = TRUE)

# step 3: main()
execute_pw <<- ""
print(con)  # print connection
print(tbl)  # print table name
tic()
main()
toc()

##############################################################################################################################
# disconnect db
dbDisconnect(con)