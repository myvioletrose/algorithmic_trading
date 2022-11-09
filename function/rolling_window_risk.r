rolling_window_risk <- function(df,
                                rolling_windows = 100,
                                dataPartition = 2000,                                
                                risk_free_rate = 0.02,
                                col_a,
                                col_b,
                                col_c,
                                col_d){
    
    df <- df %>%
        arrange(desc(date)) %>%
        dplyr::mutate(index = row_number()) %>%
        arrange(date)
    
    y <- wrapr::let(c(stock_col = col_a, market_col = col_b, market_return_col = col_c, actual_return_col = col_d),
                    df %>%
                        dplyr::mutate(stop_before = index + rolling_windows) %>%
                        dplyr::filter(index <= dataPartition) %>%
                        dplyr::select(symbol, date, start_at = index, stop_before, stock_col, market_col, market_return_col, actual_return_col))
        
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
    
    sharpe_ratio_n = paste0("sharpe_ratio_n", "_", rolling_windows)
    pct_sd_n = paste0("pct_sd_n", "_", rolling_windows)
    beta_n = paste0("beta_n", "_", rolling_windows)
    nlcor_n = paste0("nlcor_n", "_", rolling_windows)
    nlcor_pvalue_n = paste0("nlcor_pvalue_n", "_", rolling_windows)
    r_n = paste0("r_n", "_", rolling_windows)
    r_pvalue_n = paste0("r_pvalue_n", "_", rolling_windows)
    
    r_squared_n = paste0("r_squared_n", "_", rolling_windows)
    expected_return_n = paste0("expected_return_n", "_", rolling_windows)
    alpha_n = paste0("alpha_n", "_", rolling_windows)
    
    subsetDf <- wrapr::let(c(stock_col = col_a,
                             market_col = col_b,
                             market_return_col = col_c,
                             actual_return_col = col_d,
                             sharpe_ratio_col = sharpe_ratio_n,
                             pct_sd_col = pct_sd_n,
                             beta_col = beta_n,
                             nlcor_col = nlcor_n,
                             nlcor_pvalue_col = nlcor_pvalue_n,
                             r_col = r_n,
                             r_pvalue_col = r_pvalue_n,
                             r_squared_col = r_squared_n,
                             expected_return_col = expected_return_n,
                             alpha_col = alpha_n
    ),
    
    subsetList %>% 
        dplyr::bind_rows() %>%
        dplyr::select(partition, everything(), -index) %>%
        dplyr::mutate(return_pct = actual_return_col - risk_free_rate) %>%                              
        arrange(partition, date) %>%
        group_by(partition) %>%
        
        summarise(
            
            sharpe_ratio_col = mean(return_pct, na.rm = TRUE) / sd(return_pct, na.rm = TRUE),                                
            pct_sd_col = sd(stock_col, na.rm = TRUE),
            #beta_col = stats::cov(stock_col, market_col) / stats::var(stock_col),
            beta_col = stats::cov(actual_return_col, market_return_col) / stats::var(actual_return_col),
            nlcor_col = nlcor::nlcor(stock_col, market_col, plt = FALSE)$cor.estimate,
            nlcor_pvalue_col = nlcor::nlcor(stock_col, market_col, plt = FALSE)$adjusted.p.value,
            r_col = cor.test(stock_col, market_col)$estimate %>% as.vector(),
            r_pvalue_col = cor.test(stock_col, market_col)$p.value %>% as.vector()                                
            
        ) %>%
        
        ungroup %>%
        dplyr::inner_join(., y %>%                                                     
                              dplyr::select(partition = date, symbol, market_return_col, actual_return_col),
                          by = "partition") %>%
        dplyr::mutate(r_squared_col = r_col^2,
                      expected_return_col = risk_free_rate + beta_col * (market_return_col - risk_free_rate),
                      alpha_col = actual_return_col - expected_return_col) %>%
        dplyr::select(date = partition, everything())
    )
    
    return(subsetDf)
    
}