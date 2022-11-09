alert <- function(xtsObj,
                  chartSymbol,
                  dataPartition = 500,
                  rolling_windows = 20,
                  threshold = .01,
                  lday = 1,
                  column_a,
                  column_b,
                  column_trak,
                  MA_period = 2){
        
        x <- wrapr::let(c(col_a = column_a, 
                          col_b = column_b, 
                          col_trak = column_trak), 
                        historicalTransformation(xtsObj, l = lday, transformOnly = TRUE) %>%
                                dplyr::select(date, 
                                              all_of(column_a), 
                                              all_of(column_b)) %>%
                                dplyr::mutate(col_trak = round((col_a - col_b) / col_b, 4),
                                              index = nrow(.):1))
        
        y <- x %>%
                dplyr::select(date, index) %>%
                dplyr::mutate(stop_before = index + rolling_windows) %>%
                dplyr::filter(index <= dataPartition) %>%
                dplyr::select(date, start_at = index, stop_before)
        
        subsetList <- vector(mode = "list", length = nrow(y))
        
        for(i in 1:nrow(y)){
                
                partition = y$date[[i]]
                start_at = y$start_at[[i]]
                stop_before = y$stop_before[[i]]
                
                tempDf <- x %>%
                        dplyr::filter(index >= start_at & index < stop_before) %>%
                        dplyr::mutate(partition = partition)
                
                subsetList[[i]] <- tempDf
                
        }
        
        subsetDf <- wrapr::let(c(col_a = column_a, 
                                 col_b = column_b, 
                                 col_trak = column_trak), 
                               subsetList %>% 
                                       dplyr::bind_rows() %>%
                                       dplyr::select(partition, everything(), -index) %>%
                                       arrange(partition, date) %>%
                                       group_by(partition) %>%
                                       summarise(
                                               mean_change_trak = mean(col_trak),
                                               sd_change_trak = sd(col_trak),
                                               #lower_bound = round(quantile(col_trak, c(threshold, 1 - threshold), na.rm = TRUE)[1], 4),
                                               #upper_bound = round(quantile(col_trak, c(threshold, 1 - threshold), na.rm = TRUE)[2], 4)
                                               lower_bound = round(quantile(col_trak[col_trak %nin% boxplot(col_trak, plot = FALSE)$out], 
                                                                            c(threshold, 1 - threshold), na.rm = TRUE)[1], 4),
                                               upper_bound = round(quantile(col_trak[col_trak %nin% boxplot(col_trak, plot = FALSE)$out], 
                                                                            c(threshold, 1 - threshold), na.rm = TRUE)[2], 4)
                                       ) %>%
                                       ungroup %>%
                                       dplyr::inner_join(., x, by = c("partition" = "date")) %>%
                                       dplyr::mutate(date = partition,
                                                     col_a = round(col_a, 4),
                                                     col_b = round(col_b, 4),
                                                     symbol = chartSymbol,
                                                     dataPartition = dataPartition, 
                                                     rolling_windows = rolling_windows,
                                                     lday = lday, 
                                                     threshold = threshold))
        
        column_trak_name = paste0(column_trak, "_MA_n", MA_period)
                
        subsetVis <- wrapr::let(c(col_trak = column_trak, col_trak_name = column_trak_name),
                                subsetDf %>% 
                                        dplyr::select(date, 
                                                      lower_bound,
                                                      upper_bound,
                                                      all_of(column_trak)) %>%
                                        dplyr::mutate(col_trak_name = TTR::SMA(col_trak, MA_period)) %>%
                                        tidyr::gather(., key, value, -date) %>%
                                        dplyr::inner_join(., subsetDf %>% 
                                                                  select(date, index) %>% 
                                                                  distinct, by = "date")
        )
        
        return(list(df = subsetDf, vis = subsetVis))
        
}
