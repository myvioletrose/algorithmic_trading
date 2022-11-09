strength <- function(x){
        
        # multiple seasonal decomposition
        decomposition <- x %>% forecast::mstl()
        
        # extract components from a time series decomposition
        t <- forecast::trendcycle(decomposition) %>% na.omit()
        s <- forecast::seasonal(decomposition) %>% na.omit()
        r <- forecast::remainder(decomposition) %>% na.omit()
        
        # strength
        t.strength <- max(0, 1 - if(is.nan(var(r) / var(t + r))){1} else {var(r) / var(t + r)})
        #s.strength <- max(0, 1 - if(is.nan(var(r) / var(s + r))){1} else {var(r) / var(s + r)}) 
        
        # result
        # return(list(trend = t.strength,
        #             seasonality = s.strength))
        
        return(t.strength)
        
}
