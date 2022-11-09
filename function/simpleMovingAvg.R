simpleMovingAvg <- function(xts, x = "close", n = c(20, 50, 200), trading_days = 50, spread = FALSE){
        
        # symbol
        symbol <- stringr::str_extract_all(names(xts), pattern = "^[[:alpha:]].*\\.") %>% unlist %>% unique %>% gsub("\\.", "", .)
        
        # rename columns
        names(xts) <- gsub("^[[:alpha:]].*\\.", "", names(xts)) %>% stringr::str_to_lower(.)
        
        # turn it into data.frame
        xts <- as.data.frame(xts) %>% 
                tibble::rownames_to_column() %>%
                dplyr::mutate(index = nrow(.):1) %>%
                dplyr::select(date = rowname, everything())
        
        SMA_columns = paste0("SMA_", n)
        
        tempList = vector(mode = "list", length = length(n))
        
        for(i in 1:length(n)){
                
                tempList[[i]] = try({
                        
                        wrapr::let(c(col_x_name = x),
                                   xts %>%
                                           dplyr::select(date, index, all_of(x)) %>%
                                           dplyr::mutate(symbol = symbol, 
                                                         key = SMA_columns[i],
                                                         value = TTR::SMA(col_x_name, n[i])) %>%
                                           dplyr::filter(index <= trading_days)
                        )
                        
                }, silent = TRUE) 
                
        }

        if(any(sapply(tempList, class) == "try-error")){
                
                i = which(sapply(tempList, class) == "try-error")
                
                tempList[i] <- NULL
                
        }
        
        if(spread){
                tempDf = tempList %>%
                        dplyr::bind_rows() %>%
                        tidyr::spread(., key, value) %>%
                        dplyr::select(symbol, date, close, everything())
        } else {
                tempDf = tempList %>%
                        dplyr::bind_rows()         
        }

        return(tempDf)
        
}