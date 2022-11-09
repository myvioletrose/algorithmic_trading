# initiate set up
source("~/algorithmic_trading/config/setup.R")

# parameters
INTRADAY = FALSE
REAL_TIME = TRUE
APPEND = TRUE

# schema, table names
schema = "stock"
tbl_name = "fact"
tbl = paste0(schema, ".", tbl_name)

# create connection
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)

# get symbols from dim
symbols = dbGetQuery(con, statement = glue::glue("select distinct symbol from stock.dim where status = 1 order by symbol")) %>% .$symbol
#symbols = c("META")

# disconnect db
dbDisconnect(con)

# part I: get raw data
# part II: adjust for OHLC
# part III: transform data to.period()
# part IV: load data into DW

###################################################################
############## part I + II #########################
###################################################################

# start timer
tic("ETL")

# detect, use multicores
numCores <- parallel::detectCores()

# create a simple cluster on the local machine using all available threads
cl <- parallel::makeCluster(detectCores(), methods = FALSE)

# register our cluster
doParallel::registerDoParallel(cl)

#<<<<<<<<<< begin for loop to get data
tic("part I: get raw data")

# return a single list of xts objects from the valid symbols
if(REAL_TIME){
    
    xtsList <- vector(mode = "list", length = length(symbols))
    
    if(INTRADAY){p = "intraday"} else {p = "daily"}
    
    for(i in 1:length(symbols)){
        
        xtsList[[i]] <- try({
            
            quantmod::getSymbols(
                
                symbols[i], 
                env = NULL,  # set env = NULL and that is equivalent to auto.assign = FALSE
                src = "av",
                periodicity = p,
                output.size = "full", 
                adjusted = !INTRADAY,
                api.key = ALPHA_VANTAGE_API
                
            )
            
        }, silent = TRUE)
        
        #Sys.sleep(12)
        
    }
    
} else {
    
    # loop through a list of stocks tickers - super fast! 5x faster than the traditional for-loop approach!!
    symbolsCheck <- foreach::foreach(i = 1:length(symbols), .errorhandling = 'remove') %dopar% { quantmod::getSymbols(symbols[i]) } %>% unlist  # change .errorhandling = 'pass' to see error
    
    # print out a list of invalid tickers
    if(length(setdiff(symbols, symbolsCheck)) >0){
        
        errorSymbols = setdiff(symbols, symbolsCheck)
        
        sapply(1:length(errorSymbols), function(x){
            print(paste0("the symbol ", errorSymbols[x], " cannot be fetched from quantmod"))
        })
        
        cat("###########################################\n###########################################\n")
        print(paste0("there are ", 
                     length(errorSymbols), 
                     " symbols that cannot be fetched from quantmod"))
        cat("###########################################\n###########################################\n")
        
    }
    
    symbols <- symbolsCheck
    
    # set env = NULL and that is equivalent to auto.assign = FALSE
    xtsList <- foreach::foreach(i = 1:length(symbols)) %dopar% {quantmod::getSymbols(symbols[i], env = NULL, adjusted = TRUE)}
    
}

# get names for xtsList
symbols <- stringr::str_replace_all(symbols, "\\^GSPC", "SP500")
names(xtsList) <- symbols

# drop retrieval errors from list
if(any(sapply(xtsList, class) == "try-error")){
    
    i = which(sapply(xtsList, class) == "try-error")
    
    errorSymbols = names(xtsList)[i]
    
    xtsList[i] <- NULL
    
    sapply(1:length(errorSymbols), function(x){
        print(paste0("the symbol ", errorSymbols[x], " cannot be fetched via quantmod"))
    })
    
    cat("###########################################\n###########################################\n")
    print(paste0("there are ", 
                 length(errorSymbols), 
                 " symbols that cannot be fetched via quantmod"))
    cat("###########################################\n###########################################\n")
    
}

#<<<<<<<<<< end of getting raw data
toc()

#<<<<<<<<<< begin to adjust for OHLC
tic("part II: adjust for OHLC")

adjustList <- vector(mode = "list", length = length(xtsList))
names(adjustList) = names(xtsList)

# adjust OHLC for each xts
for(j in 1:length(xtsList)){
    
    adjustList[[j]] <- try(
        {
            quantmod::adjustOHLC(xtsList[[j]], symbol.name = names(xtsList)[j])
        }, 
        silent = TRUE
    )
    
    #row.names(adjustList[[j]]) <- NULL
    print(names(adjustList[j]))
    if(class(adjustList[[j]]) == "try-error"){print(paste0("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< above symbol <<<<<<<<<<<< ", class(adjustList[j])))}
    
}

e = which(sapply(adjustList, class) == "try-error")

errorSymbols_fr_adjust = names(adjustList)[e]

# remove error
adjustList[e] <- NULL

# append xtsList to the adjustList
remainList = xtsList[errorSymbols_fr_adjust]
adjustList = c(adjustList, remainList)

#<<<<<<<<<< end of adjustOHLC
toc()

# stop the cluster
parallel::stopCluster(cl)

cat("###########################################\n###########################################\n")
print(paste0("data retrieval real time ", REAL_TIME))
print(Sys.time())

###################################################################
############## part III + IV #########################
###################################################################

#<<<<<<<<<< begin to transform data to.period()
tic("part III: transform data to.period()")

# adhoc functions to process different periods for each xts
adhocPeriod <- function(xts, period = "days"){

    symbol <- stringr::str_extract_all(names(xts), pattern = "^[[:alpha:]].*\\.") %>% unlist %>% stringr::str_to_upper(.) %>% unique %>% gsub("\\.", "", .)

    x2 <- xts %>% 
        xts::to.period(period = period, name = NULL) %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "date") %>%
        dplyr::mutate(symbol = symbol, 
                      period = period) %>%        
        janitor::clean_names()

    return(x2)

}

processPeriod <- function(xts){

    periods = c("days", "weeks", "months", "quarters", "years")

    outputDf = lapply(1:length(periods), function(x) adhocPeriod(xts, period = periods[x])) %>% 
        dplyr::bind_rows() %>%
        dplyr::select(period, symbol, date, everything()) %>%
        dplyr::arrange(period, date)

    return(outputDf)

}

# transform data
tempList <- vector(mode = "list", length = length(adjustList))
names(tempList) = names(adjustList)

# process each period for each xts
for(j in 1:length(adjustList)){
    
    tempList[[j]] <- try(
        {
            processPeriod(adjustList[[j]])
        }, 
        silent = TRUE
    )
    
    #row.names(tempList[[j]]) <- NULL
    print(names(tempList[j]))
    if(class(tempList[[j]]) == "try-error"){print(paste0("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< above symbol <<<<<<<<<<<< ", class(tempList[j])))}
    
}

e2 = which(sapply(tempList, class) == "try-error")

errorSymbols2 = names(tempList)[e2]

tempList[e2] <- NULL

tempDf <- tempList %>% dplyr::bind_rows()

# fix errorSymbols_fr_adjust 
tempDf2 <- tempDf %>%
    dplyr::filter(symbol %in% errorSymbols_fr_adjust) %>%
    dplyr::mutate(adj_coefficient = adjusted / close,
                  open = open * adj_coefficient,
                  high = high * adj_coefficient,
                  low = low * adj_coefficient,
                  close = close * adj_coefficient) %>%
    dplyr::select(period, symbol, date, open, high, low, close, volume, adjusted)

# concat df - factDf
factDf <- rbind(tempDf %>% dplyr::filter(symbol %nin% errorSymbols_fr_adjust), tempDf2) %>%
    arrange(period, symbol, date)

#<<<<<<<<<< end of transformation to.period()
toc()

#<<<<<<<<<< load data into DW
tic("part IV: load data into DW")

# create connection
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = DB, host = HOST_DB, port = DB_PORT, user = DB_USER, password = DB_PASSWORD)

# write table
if(APPEND){
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      factDf, 
                      append = TRUE)
} else {
    DBI::dbWriteTable(con, DBI::SQL(tbl), 
                      factDf, 
                      overwrite = TRUE)
}

#<<<<<<<<<< end of load data into DW
toc()

# stop timer: finish part I, II, III & IV
toc(log = TRUE)

##############################################################################################################################
######################################
# data validation
dbGetQuery(con, statement = glue::glue("select count(1) from {tbl}"))
dbGetQuery(con, statement = glue::glue("select period, symbol, date, count(1) from {tbl} group by 1, 2, 3 having count(1)>1 limit 5"))

dbGetQuery(con, glue::glue("
select period
, symbol
, count(1) as n
, min(date) as min_date
, max(date) as max_date
from {tbl}
group by 1, 2
order by 2, 1
")) -> x

dim(x); head(x)

# sample tbl
fact_output = dbGetQuery(con, statement = glue::glue("select * from {tbl} where symbol = 'SPY' order by period, date"))
#fact_output %>% write_clip()

# disconnect db
dbDisconnect(con)