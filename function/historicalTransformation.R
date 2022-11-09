historicalTransformation <- function(xts,        
                                     l = 1,
                                     n = 1,
                                     originalSymbol = FALSE,
                                     transformOnly = FALSE
){
        
        # check if packages available
        # packages <- c("tidyverse", "quantmod", "xts", "jayden", "sqldf")
        # invisible( sapply(packages, jayden::CheckAndLoad) )
        
        # transform() function
        transformation <- function(xts, l, n, originalSymbol){
                
                # check if packages available
                # packages <- c("tidyverse", "quantmod", "xts", "jayden")
                # invisible( sapply(packages, jayden::CheckAndLoad) )
                
                # symbol
                symbol <- stringr::str_extract_all(names(xts), pattern = "^[[:alpha:]].*\\.") %>% unlist %>% stringr::str_to_lower(.) %>% unique %>% gsub("\\.", "", .)
                
                # rename columns
                names(xts) <- gsub("^[[:alpha:]].*\\.", "", names(xts)) %>% stringr::str_to_lower(.)
                
                # turn it into data.frame
                xts <- as.data.frame(xts)
                
                # insert columns
                xts <- xts %>%
                        dplyr::mutate(
                                # symbol
                                symbol = toupper(symbol),
                                # date
                                date = row.names(xts) %>% lubridate::ymd(.),
                                year = lubridate::year(date),
                                quarter = lubridate::quarter(date),
                                month = lubridate::month(date),
                                weekday = base::weekdays(date),
                                day = lubridate::day(date),
                                # Lag
                                op_l = quantmod::Lag(xts[, "open"], l) %>% as.vector,
                                hi_l = quantmod::Lag(xts[, "high"], l) %>% as.vector,
                                lo_l = quantmod::Lag(xts[, "low"], l) %>% as.vector,
                                cl_l = quantmod::Lag(xts[, "close"], l) %>% as.vector,
                                vol_l = quantmod::Lag(xts[, "volume"], l) %>% as.vector,
                                adj_l = quantmod::Lag(xts[, "adjusted"], l) %>% as.vector,
                                adj_lag_1 = quantmod::Lag(xts[, "adjusted"], 1) %>% as.vector,
                                adj_lag_2 = quantmod::Lag(xts[, "adjusted"], 2) %>% as.vector,
                                # Next
                                op_n = lead(xts[, "open"], n),
                                hi_n = lead(xts[, "high"], n),
                                lo_n = lead(xts[, "low"], n),
                                cl_n = lead(xts[, "close"], n),
                                vol_n = lead(xts[, "volume"], n),
                                adj_n = lead(xts[, "adjusted"], n),
                                # daily "Close - Open", "Hi - Low"
                                cl_op_diff = close - open,
                                hi_lo_diff = high - low,
                                adj_op_diff = adjusted - open,
                                # Lag "diff"
                                op_l_diff = open - op_l,
                                hi_l_diff = high - hi_l,
                                lo_l_diff = low - lo_l,
                                cl_l_diff = close - cl_l,
                                vol_l_diff = volume - vol_l,
                                adj_l_diff = adjusted - adj_l,
                                # Next "diff"
                                op_n_diff = op_n - open,
                                hi_n_diff = hi_n - high,
                                lo_n_diff = lo_n - low,
                                cl_n_diff = cl_n - close,
                                vol_n_diff = vol_n - volume,
                                adj_n_diff = adj_n - adjusted,
                                # oscillator - RSI, CCI                                
                                rsi_close = TTR::RSI(xts[, "adjusted"]) %>% as.vector,
                                cci_close = TTR::CCI(xts[, "adjusted"]) %>% as.vector,
                                # momentum indicator
                                obv_close = TTR::OBV(xts$adjusted, xts$volume) %>% as.vector,
                                # intraday volatility
                                intraday_volatility = adj_op_diff / hi_lo_diff
                        )
                
                # oscillator - MACD
                macd <- MACD(xts$adjusted) %>% as.data.frame %>% dplyr::select(macd) %>% .$macd
                signal <- MACD(xts$adjusted) %>% as.data.frame %>% dplyr::select(signal) %>% .$signal
                
                # add MACD
                xts <- xts %>%
                        dplyr::mutate(macd_close = macd,
                                      signal_close = signal,
                                      macd_close_diff = macd_close - signal_close) %>%
                        dplyr::select(symbol, date, year, quarter, month, weekday, day,
                                      open, high, low, close, volume, adjusted, adj_lag_1, adj_lag_2, intraday_volatility,
                                      rsi_close, cci_close, macd_close, signal_close, macd_close_diff, obv_close,
                                      everything()) %>%
                        dplyr::arrange(date)
                
                # rename columns back with original symbol
                if(originalSymbol){names(xts) <- paste(symbol, names(xts), sep = "_")}
                
                # return xts
                return(xts)
                
        }
        
        # return a df object
        if(transformOnly){
                
                # return a transformed xts object
                transformObj <- transformation(xts, l, n, originalSymbol)
                return(transformObj)
                
        } else {
                
                # return the min and max of different variables associated with dates based on a transformed xts object
                SYMBOL <- transformation(xts, l, n, originalSymbol)
                
                minVector <- SYMBOL[complete.cases(SYMBOL), 6:ncol(SYMBOL)] %>% lapply(., min) %>% unlist
                maxVector <- SYMBOL[complete.cases(SYMBOL), 6:ncol(SYMBOL)] %>% lapply(., max) %>% unlist
                
                minList <- vector(mode = "list", length = length(minVector))
                for(i in 1:length(minList)){
                        minList[[i]] <- sqldf(sprintf("select date, %s from SYMBOL where floor(%s) = floor(%f)", names(minVector)[i], names(minVector)[i], minVector[i]))
                }
                
                maxList <- vector(mode = "list", length = length(maxVector))
                for(i in 1:length(maxList)){
                        maxList[[i]] <- sqldf(sprintf("select date, %s from SYMBOL where ceil(%s) = ceil(%f)", names(maxVector)[i], names(maxVector)[i], maxVector[i]))
                }
                
                minDf <- minList %>%
                        bind_rows() %>%
                        tidyr::gather(., key, value, -date) %>%
                        dplyr::filter(!is.na(value)) %>%
                        dplyr::mutate(type = "min") %>%
                        arrange(key, date)
                
                maxDf <- maxList %>%
                        bind_rows() %>%
                        tidyr::gather(., key, value, -date) %>%
                        dplyr::filter(!is.na(value)) %>%
                        dplyr::mutate(type = "max") %>%
                        arrange(key, date)
                
                historyObj <- bind_rows(minDf, maxDf) %>%
                        dplyr::select(., type, everything()) %>%
                        arrange(type, key, date)
                
                return(historyObj)
        }
        
}