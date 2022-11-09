strategyEval <- function(s, fund_begin = 10000, amountInvestedMax = 10000, printOption = FALSE){
        
        # s must have date, close, and message column
        s = s %>%
                dplyr::arrange(date)
        
        # these are the required columns
        symbol = s$symbol
        date = s$date
        open = s$open
        close = s$close
        message = s$message        
        
        # use the next close price as the buy/sell price
        current_price = s$close[1]
        current_share = 0
        current_amountInvested = 0
        current_fund = fund_begin 
        
        ready_to_buy = TRUE
        ready_to_sell = FALSE
        initiation = TRUE
        
        while(initiation){
                
                fundList = rep(0, length(close))
                amountInvestedList = rep(0, length(close))
                shareList = rep(0, length(close))
                
                for(i in 1:length(close)){
                        
                        if(message[i] == 'hold'){
                                current_price = close[i]
                                fundList[i] = current_fund 
                                amountInvestedList[i] = current_amountInvested 
                                shareList[i] = current_share
                        } else {
                                if(message[i] == 'buy'){
                                        if(ready_to_buy){
                                                current_price = close[i]
                                                current_amountInvested = min(amountInvestedMax, current_fund)
                                                current_share = current_amountInvested / current_price
                                                current_fund = current_fund - current_amountInvested
                                                
                                                fundList[i] = current_fund 
                                                amountInvestedList[i] = current_amountInvested
                                                shareList[i] = current_share
                                                
                                                ready_to_buy = FALSE
                                                ready_to_sell = TRUE
                                        } else {
                                                current_price = close[i]
                                                fundList[i] = current_fund 
                                                amountInvestedList[i] = current_amountInvested
                                                shareList[i] = current_share
                                                
                                                ready_to_sell = TRUE
                                        }
                                } else {
                                        if(ready_to_sell){
                                                current_price = close[i]
                                                current_fund = current_fund + current_share * current_price
                                                current_share = 0 
                                                current_amountInvested = 0
                                                
                                                fundList[i] = current_fund 
                                                amountInvestedList[i] = current_amountInvested
                                                shareList[i] = current_share
                                                
                                                ready_to_sell = FALSE
                                                ready_to_buy = TRUE
                                        } else {
                                                current_price = close[i]
                                                current_share = 0 
                                                current_amountInvested = 0
                                                
                                                fundList[i] = current_fund
                                                shareList[i] = current_share
                                                amountInvestedList[i] = current_amountInvested
                                                
                                                ready_to_buy = TRUE
                                        }
                                }
                        }
                        
                }
                
                break
                
        }
        
        # set the latest close price to be the current price
        current_price = s$close[nrow(s)]
        
        # value is settled based on the close price of the trading day
        evalOut <- data.frame(symbol, date, open, close, message,
                              fund = fundList,
                              amountInvested = amountInvestedList,
                              share = shareList) %>%
                dplyr::mutate(value = fund + share * close, 
                              net_value = value - fund_begin)
        
        current_value = current_fund + current_price * current_share
        
        net_value = current_value - fund_begin
        
        avg_value = mean(evalOut$value)
        
        avg_volatility = sd(evalOut$value)
        
        value_skewness = e1071::skewness(evalOut$value)
        
        value_kurtosis = e1071::kurtosis(evalOut$value)
        
        symbol = unique(symbol)
        
        if(printOption){
                print(paste0(symbol, ": the number of current share is ", current_share))
                print(paste0(symbol, ": the current close price is $", current_price))
                print(paste0(symbol, ": the current amount invested is $", current_amountInvested))
                print(paste0(symbol, ": the current available fund is $", round(current_fund, 1)))
                print(paste0(symbol, ": the current value observed in the past ", nrow(s), " trading days is $", round(current_value, 1)))
                print(paste0(symbol, ": the avg value observed in the past ", nrow(s), " trading days is $", round(avg_value, 1)))
                print(paste0(symbol, ": the avg value volatility observed in the past ", nrow(s), " trading days is $", round(avg_volatility, 1)))
                print(paste0(symbol, ": the skewness of value observed in the past ", nrow(s), " trading days is ", round(value_skewness, 4)))
                print(paste0(symbol, ": the kurtosis value observed in the past ", nrow(s), " trading days is ", round(value_kurtosis, 4)))
                print(paste0(symbol, ": the net value observed in the past ", nrow(s), " trading days is ", round(100 * net_value / fund, 2), "%"))
        }
        
        return(evalOutputList = list(evalDf = evalOut, 
                                     current_value = current_value, 
                                     net_value = net_value,
                                     avg_value = avg_value,
                                     value_skewness = value_skewness,
                                     value_kurtosis = value_kurtosis,
                                     avg_volatility = avg_volatility))
        
}


######################################################################################
# # example 1
# x <- sOutput %>%
#         dplyr::filter(symbol == 'PRPL' & index <= 100) %>%
#         dplyr::select(symbol, date, open, close, message)
# 
# test = strategyEval(x, printOption = TRUE)
# tail(test$evalDf)
# test$current_value
# test$net_value
# test$avg_value
# test$value_skewness
# test$value_kurtosis
# test$avg_volatility

################################################
################################################

# # example 2
# nestedObj <- sOutput %>%
#         dplyr::select(symbol, date, open, close, message) %>%
#         tidyr::nest(data = c(date, open, close, message))
# 
# cv = rep(0, nrow(nestedObj))
# nv = rep(0, nrow(nestedObj))
# av = rep(0, nrow(nestedObj))
# avo = rep(0, nrow(nestedObj))
# sk = rep(0, nrow(nestedObj))
# ku = rep(0, nrow(nestedObj))
# edf = vector(mode = "list", length = nrow(nestedObj))
# names(edf) = nestedObj$symbol
# 
# tic()
# for(i in 1:nrow(nestedObj)){
# 
#         d = nestedObj$data
#         names(d) = nestedObj$symbol
#         d = lapply(1:length(d), function(x) d[[x]] = d[[x]] %>% as.data.frame() %>% dplyr::mutate(symbol = names(d)[x]))
#         names(d) = nestedObj$symbol
# 
#         cv[i] <- strategyEval(d[[i]], printOption = FALSE)$current_value
#         nv[i] <- strategyEval(d[[i]], printOption = FALSE)$net_value
#         av[i] <- strategyEval(d[[i]], printOption = FALSE)$avg_value
#         avo[i] <- strategyEval(d[[i]], printOption = FALSE)$avg_volatility
#         sk[i] <- strategyEval(d[[i]], printOption = FALSE)$value_skewness
#         ku[i] <- strategyEval(d[[i]], printOption = FALSE)$value_kurtosis
#         edf[[i]] <- strategyEval(d[[i]], printOption = FALSE)$evalDf %>%
#                 dplyr::mutate(symbol = names(edf)[i])
# 
# }
# toc()
# 
# evalObjBrief <- nestedObj %>%
#         dplyr::select(-data) %>%
#         as.data.frame() %>%
#         dplyr::mutate(current_value = cv,
#                       net_value = nv,
#                       avg_value = av,
#                       value_skewness = sk,
#                       value_kurtosis = ku,
#                       avg_volatility = avo)
# 
# evalObjDepth <- sOutput %>%
#         dplyr::inner_join(., edf %>%
#                                   dplyr::bind_rows() %>%
#                                   dplyr::select(date, symbol, fund, amountInvested, share, value, net_value),
#                           by = c("date", "symbol"))
# 
# # write.csv(evalObjBrief, "clipboard-16384", row.names = FALSE)





