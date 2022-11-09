rolling_window <- function(df,
                           rolling_windows = 100,
                           dataPartition = 2000,
                           col){
    
    y <- wrapr::let(c(target_col = col),
                    df %>%
                        dplyr::mutate(stop_before = index + rolling_windows) %>%
                        dplyr::filter(index <= dataPartition) %>%
                        dplyr::select(date, start_at = index, stop_before, target_col))

    subsetList <- vector(mode = "list", length = nrow(y))
    
    for(i in 1:length(subsetList)){
        
        partition = y$date[[i]]
        start_at = y$start_at[[i]]
        stop_before = y$stop_before[[i]]
        
        tempDf <- df %>%
            dplyr::filter(index >= start_at & index < stop_before) %>%
            dplyr::mutate(partition = partition)
        
        subsetList[[i]] = tempDf

    }
    
    mu_col_name = paste0(col, "_mu_", rolling_windows)
    sd_col_name = paste0(col, "_sd_", rolling_windows)
    percentile_col_name = paste0(col, "_percentile_", rolling_windows)
    
    subsetDf <- wrapr::let(c(target_col = col,
                             mu_col = mu_col_name,
                             sd_col = sd_col_name,
                             percentile_col = percentile_col_name),
                           subsetList %>% 
                               dplyr::bind_rows() %>%
                               dplyr::select(partition, everything(), -index) %>%
                               arrange(partition, date) %>%
                               group_by(partition) %>%
                               summarise(
                                   mu = mean(target_col, na.rm = TRUE),
                                   sd = sd(target_col, na.rm = TRUE)) %>%
                               ungroup %>%
                               dplyr::select(date = partition, mu, sd) %>%
                               dplyr::inner_join(., y %>% dplyr::select(-start_at, -stop_before), by = "date") %>%
                               dplyr::mutate(percentile = pnorm(target_col, mu, sd)) %>%
                               dplyr::select(date, 
                                             target_col,
                                             mu_col = mu,
                                             sd_col = sd,
                                             percentile_col = percentile)
                           )
    
    return(subsetDf)
    
}

# example
# x = rolling_window(SPY %>% historicalTransformation(., transformOnly = TRUE) %>%
#                        dplyr::mutate(pct = (adjusted - adj_lag_1) / adj_lag_1) %>%
#                        arrange(symbol, desc(date)) %>%
#                        dplyr::mutate(index = row_number()), 
#                    rolling_windows = 100,  # number of days leading to each specific day   
#                    dataPartition = 2000,  # number of most recent days that we want to look at
#                    col = "pct"  # column that we want to transform
#                    )

#x  # return date, target column, mu, sd, percentile, e.g., date|pct|pct_mu_100|pct_sd_100|pct_percentile_100

# # A tibble: 2,000 x 5
# date             pct pct_mu_100 pct_sd_100 pct_percentile_100
# <date>         <dbl>      <dbl>      <dbl>              <dbl>
# 1 2014-04-29  0.00466    0.000561    0.00739              0.710
# 2 2014-04-30  0.00298    0.000592    0.00739              0.627
# 3 2014-05-01  0.000106   0.000637    0.00737              0.471
# 4 2014-05-02 -0.00143    0.000511    0.00730              0.395
# 5 2014-05-05  0.00191    0.000504    0.00730              0.577
# 6 2014-05-06 -0.00870    0.000453    0.00735              0.106
# 7 2014-05-07  0.00589    0.000624    0.00727              0.766
# 8 2014-05-08 -0.00106    0.000647    0.00726              0.407
# 9 2014-05-09  0.00149    0.000663    0.00726              0.545
# 10 2014-05-12  0.00974    0.000698    0.00730              0.892
# # ... with 1,990 more rows

