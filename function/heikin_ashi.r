#https://github.com/pverspeelt/Quantfunctions/blob/main/R/heikin_ashi.R
#https://rdrr.io/github/pverspeelt/Quantfunctions/src/R/heikin_ashi.R
heikin_ashi <- function(x, output_as_df = TRUE) {
    
    if(!quantmod::is.OHLC(x)) 
        stop("x must contain OHLC columns", call. = FALSE)
    
    if(any(is.na(x))) 
        stop("x contains NA values, either remove these records or fix them",
             call. = FALSE)
    
    heikin_close <- xts::xts(rowMeans(quantmod::OHLC(x)), 
                             order.by = zoo::index(x))
    heikin_open  <- quantmod::Op(x)
    
    # need a loop: heiki ashi open is dependent on the previous value
    for(i in 2:nrow(x)) {
        heikin_open[i] <- (heikin_open[i-1] + heikin_close[i-1]) / 2
    }
    
    heikin_high <- xts::xts(apply(cbind(quantmod::Hi(x), heikin_open, heikin_close), 1, max), 
                            order.by = zoo::index(x))
    heikin_low <- xts::xts(apply(cbind(quantmod::Lo(x), heikin_open, heikin_close), 1, min), 
                           order.by = zoo::index(x))
    
    date <- index(x) %>% as.xts()
    
    out <- merge(date, heikin_open, heikin_high, heikin_low, heikin_close)
    names(out) <- c("open", "high", "low", "close")
    
    if(output_as_df){
        out <- out %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            dplyr::select(date = rowname, everything())
        return(out)
    } else {
        return(out)
    }
    
}
