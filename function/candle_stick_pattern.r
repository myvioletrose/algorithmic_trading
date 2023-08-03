# https://kochiuyu.github.io/r-programming/package/
candle_stick_pattern <- function(df, func_list = c('doji', 'dragonfly.doji', 'gravestone.doji', 'hammer', 'inverted.hammer', 'bullish.engulf', 'bearish.engulf', 'bullish.harami', 'bearish.harami', 'piercing.line', 'dark.cloud.cover', 'kick.up', 'kick.down', 'three.white.soldiers', 'three.black.crows', 'morning.star', 'evening.star', 'rising.three', 'falling.three', 'up.trend', 'down.trend', 'bullish.candle', 'bearish.candle')){
        
        s = df$symbol %>% unique()
        
        xtsObj <- df %>%
                select(date, open, high, low, close) %>%
                arrange(date) %>%
                timetk::tk_xts()
        
        candle_stick_list <- vector(mode = "list", length = length(func_list))
        
        for(i in 1:length(candle_stick_list)){
                
                csp <- eval(sym(func_list[i]))(xtsObj) %>%
                        as.data.frame() %>%
                        tibble::rownames_to_column() %>%
                        select(date = rowname, everything()) %>%
                        dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
                        janitor::clean_names() 
                
                candle_stick_list[[i]] <- csp                        
                
        }
        
        candle_stick_df <- plyr::join_all(candle_stick_list, type = "inner", by = "date") %>%
                dplyr::mutate(symbol = s) %>%
                dplyr::mutate_if(is.logical, as.numeric) %>%
                select(symbol, date, everything())
                
        
        return(candle_stick_df)
        
}
