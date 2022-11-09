simulationEval <- function(sOutput, dataPartition = 20, rolling_windows = 20, printStatement = FALSE){
        
        symbol <- sOutput$symbol %>% unique()
        
        x <- sOutput %>% 
                dplyr::select(symbol, date, open, close, message, index)
        
        y <- x %>%
                dplyr::select(date, index, symbol) %>%
                dplyr::mutate(stop_before = index + rolling_windows) %>%
                dplyr::filter(index <= dataPartition) %>%
                dplyr::select(date, symbol, start_at = index, stop_before)
        
        subsetList <- vector(mode = "list", length = nrow(y))
        
        for(i in 1:nrow(y)){
                
                partition = y$date[[i]]
                start_at = y$start_at[[i]]
                stop_before = y$stop_before[[i]]
                
                tempDf <- x %>%
                        dplyr::filter(index >= start_at & index < stop_before) %>%
                        dplyr::mutate(symbol = symbol,
                                      partition = partition)
                
                subsetList[[i]] <- tempDf
                
        }
        
        nestedObj <- subsetList %>%
                dplyr::bind_rows() %>%
                dplyr::select(-index) %>%
                dplyr::mutate(partition = stringr::str_replace_all(.$partition, pattern = "-", replacement = "")) %>%
                tidyr::nest(data = c(symbol, date, open, close, message))
        
        # current value
        cv = rep(0, nrow(nestedObj))
        # net value
        nv = rep(0, nrow(nestedObj))
        # average value
        av = rep(0, nrow(nestedObj))
        # average volatility
        avo = rep(0, nrow(nestedObj))
        # skewness
        sk = rep(0, nrow(nestedObj))
        # kurtosis
        ku = rep(0, nrow(nestedObj))
        # trend.micro 
        t = rep(0, nrow(nestedObj))
        # direction
        dir = rep(0, nrow(nestedObj))
        # evaluation df
        edf = vector(mode = "list", length = nrow(nestedObj))
        names(edf) = nestedObj$partition 
        # number of 'buy', 'sell', 'hold' days
        num_of_buy = rep(0, nrow(nestedObj))
        num_of_sell = rep(0, nrow(nestedObj))
        num_of_hold = rep(0, nrow(nestedObj))

        for(i in 1:nrow(nestedObj)){
                
                d = nestedObj$data
                names(d) = nestedObj$partition 
                d = lapply(1:length(d), function(x) d[[x]] = d[[x]] %>% as.data.frame())
                names(d) = nestedObj$partition
                
                cv[i] <- strategyEval(d[[i]], printOption = FALSE)$current_value
                nv[i] <- strategyEval(d[[i]], printOption = FALSE)$net_value
                av[i] <- strategyEval(d[[i]], printOption = FALSE)$avg_value
                avo[i] <- strategyEval(d[[i]], printOption = FALSE)$avg_volatility
                sk[i] <- strategyEval(d[[i]], printOption = FALSE)$value_skewness
                ku[i] <- strategyEval(d[[i]], printOption = FALSE)$value_kurtosis
                t[i] <- try({
                        strategyEval(d[[i]], printOption = FALSE)$evalDf %>%
                        dplyr::select(net_value) %>%
                        .$net_value %>%
                        strength()
                }, silent = TRUE) 
                
                dir[i] <- suppressWarnings(
                        try({
                                net_value_vector = strategyEval(d[[i]], printOption = FALSE)$evalDf %>%
                                        dplyr::select(net_value) %>%
                                        .$net_value
                                
                                if(is.na(cor(net_value_vector, 1:length(net_value_vector)))){
                                        direction = NA
                                        } else {
                                                if(cor(net_value_vector, 1:length(net_value_vector)) >0){direction = 'up'} else {direction = 'down'}
                                        }
                                direction
                                }, 
                            silent = TRUE)
                        )
                
                edf[[i]] <- strategyEval(d[[i]], printOption = FALSE)$evalDf %>%
                        dplyr::mutate(partition = names(d)[i])                
                num_of_buy[i] = sum(edf[[i]]$message == 'buy')
                num_of_sell[i] = sum(edf[[i]]$message == 'sell')
                num_of_hold[i] = sum(edf[[i]]$message == 'hold')
                
        }
        
        # replace error value with NA for trend.micro score
        if(any(sapply(t, function(x) stringr::str_detect(x, "[Ee]rror")))){
                
                i = which(sapply(t, function(x) stringr::str_detect(x, "[Ee]rror")))
        
                t[i] = NA
                
                t = as.numeric(t)
        }
        
        evalObjBrief <- nestedObj %>%
                dplyr::select(-data) %>%
                as.data.frame() %>%
                dplyr::mutate(partition = partition %>% as.Date(., "%Y%m%d"),
                              symbol = symbol,
                              direction = as.character(dir),
                              trend_micro = t,
                              trend_micro = dplyr::case_when(direction == 'up' ~ t,
                                                             direction == 'down' ~ t * -1,
                                                             TRUE ~ t),
                              num_of_buy = num_of_buy,
                              num_of_sell = num_of_sell,
                              num_of_hold = num_of_hold,
                              current_value = cv,
                              net_value = nv,
                              avg_value = av,
                              value_skewness = sk,
                              value_kurtosis = ku,
                              avg_volatility = avo)
        
        evalObjDepth <- sOutput %>%
                dplyr::inner_join(., suppressWarnings(edf %>%
                                                              dplyr::bind_rows()) %>%
                                          dplyr::select(partition, 
                                                        date, 
                                                        symbol,
                                                        fund, 
                                                        amountInvested, 
                                                        share, 
                                                        value, 
                                                        net_value) %>%
                                          dplyr::mutate(symbol = as.character(symbol),
                                                        partition = as.integer(partition)),
                                  by = c("date", "symbol")) %>%
                dplyr::left_join(., evalObjBrief %>%
                                         dplyr::select(partition, 
                                                       trend_micro_partition = trend_micro) %>%
                                         dplyr::mutate(partition = stringr::str_replace_all(partition, "-", "") %>% as.integer),
                                 by = "partition") %>%
                dplyr::left_join(., evalObjBrief %>% 
                                         dplyr::select(date = partition, 
                                                       direction, 
                                                       trend_micro,
                                                       num_of_buy, 
                                                       num_of_sell,
                                                       num_of_hold), 
                                 by = "date")  %>%
                dplyr::select(partition, trend_micro_partition, index, date, symbol, value, net_value, direction, trend_micro, num_of_buy, num_of_sell, num_of_hold, signal, message, everything()) %>%
                arrange(partition, date)

        printOutput <- sapply(list(mean, sd), function(x) evalObjBrief %>% dplyr::filter(num_of_buy >0) %>% dplyr::select(current_value) %>% .$current_value %>% x)

        if(printStatement){

                print(paste0(symbol, ": the average current value in the past ", dataPartition, " trading days is $", round(printOutput[1], 1)))
                print(paste0(symbol, ": the sd of current value in the past ", dataPartition, " trading days is $", round(printOutput[2], 1)))

        }

        return(result = list(evalObjBrief = evalObjBrief,
                             evalObjDepth = evalObjDepth,
                             avg_current_value = printOutput[1],
                             avg_volatility = printOutput[2]))
        
}

##################################################
######## testing ########
# # step 1: strategy(), sOutput
# p = strategy(PRPL, "PRPL")
# p %>% tail
# 
# p # step 2: strategyEval(), strategyEval output
# pS = strategyEval(p)$evalDf
# pS %>% tail
# 
# # step 3: simulationEval(), evalObjBrief, evalObjDepth
# pSS = simulationEval(p)
# pSS$evalObjBrief %>% tail
# pSS$evalObjDepth %>% tail

###########################################################################

# test <- simulationEval(sOutput %>% dplyr::filter(symbol == "GOOGL"),
#                        dataPartition = 20,
#                        rolling_windows = 20,
#                        printStatement = TRUE)
# 
# a = test$evalObjBrief$net_value
# # b = sOutput %>%
# #         dplyr::filter(index <= 20) %>%
# #         dplyr::filter(symbol == "ZM") %>%
# #         dplyr::select(close) %>%
# #         .$close
# 
# b = 1:20
# cor(b, a)
# plot(b, a)
# 
# tempChart <- test$evalObjBrief %>%
# dplyr::select(partition, net_value) %>%
# ggplot(., aes(partition, net_value)) +
# geom_line() +
# bdscale::scale_x_bd(business.dates = test$evalObjBrief %>%
#                     lplyr::pull(partition) %>%
#                     unique,
#             max.major.breaks = 50,
#             labels = scales::date_format("%Y-%m-%d")) +
# scale_y_continuous(labels = scales::dollar) +
# theme(legend.position = "top",
#       plot.title = element_text(hjust = 0.5),
#       axis.text.x = element_text(hjust = 1, angle = 60),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       legend.title = element_blank())
# 
# chart4
# 
# grid.arrange(tempChart, chart4, nrow = 2)

###################################
