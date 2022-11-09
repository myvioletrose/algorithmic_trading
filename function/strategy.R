strategy <- function(xtsObj,
                     chartSymbol,
                     dataPartition = 500,
                     rolling_windows = 20,
                     threshold = .01,
                     lday = 1,
                     MA_period = 2,
                     trading_days = 50,
                     column_a = "close",
                     column_b = "cl_l",
                     column_trak = "lag_change_trak",
                     rsi_support = 50,
                     rsi_resistance = 40,
                     cci_uptrend_limit = 150,
                     cci_downtrend_limit = -150){
        
        # alert() - get vis
        vis <- alert(xtsObj, 
                     chartSymbol, 
                     dataPartition, 
                     rolling_windows, 
                     threshold, 
                     lday, 
                     column_a, 
                     column_b, 
                     column_trak,
                     MA_period)$vis
        
        # signalDf
        column_trak_name = paste0(column_trak, "_MA_n", MA_period)
        
        signalDf <- wrapr::let(c(col_a = column_trak,
                                 col_b = column_trak_name),
                               vis %>%
                                       tidyr::spread(., key, value) %>%
                                       na.omit() %>%
                                       dplyr::mutate(aboveZero = dplyr::case_when(col_a >0 & col_b >0 ~ 1,
                                                                                  TRUE ~ 0),
                                                     sellCode = dplyr::case_when(aboveZero == 0 & col_a < col_b ~ 10,
                                                                                   TRUE ~ 0),
                                                     sellCode = dplyr::case_when(col_a < lower_bound | col_b < lower_bound ~ 10,
                                                                                 TRUE ~sellCode),
                                                     buyCode = dplyr::case_when(aboveZero == 1 & col_a > col_b ~ 100,
                                                                                  TRUE ~ 0),
                                                     signal = aboveZero + sellCode + buyCode) %>%
                                       dplyr::select(indx = index, signal, everything())
        )
        
        # buySignalDf
        buySignalDf <- sqldf::sqldf("
        with base
        as (
        	select indx_id, indx, signal
        	from (select distinct indx as indx_id from signalDf)
        	cross join (select distinct indx, signal from signalDf)
        ),
        
        sub
        as (
        	select b.indx_id 
        	, b.indx
        	, b.signal 
        	from base b 
        	join signalDf s on b.indx_id = s.indx
        	where b.indx >= s.indx 
        ), 
        
        m
        as (
        	select indx_id
        	, min(indx) as reset_indx
        	from sub
        	where signal in (0, 10)
        	group by 1
        ), 
        
        interim 
        as (
        	select m.indx_id 
        	, sum(case when b.signal = 1 then 1 else 2 end) as buySignal
        	from m 
        	join base b on m.indx_id = b.indx_id and m.indx_id <= b.indx and m.reset_indx >= b.indx
        	where b.signal in (1, 101)
        	group by 1
        )
        
        select s.indx 
        , s.signal 
        , case when s.signal in (1, 101) and i.buySignal >=2 then 1 else 0 end as buySignal 
        from signalDf s 
        left join interim i on s.indx = i.indx_id 
        ") %>%
                dplyr::select(index = indx, signal, buySignal)
        
        # sellSignalDf
        sellSignalDf <- sqldf::sqldf("
        with base
        as (
        	select indx_id, indx, signal
        	from (select distinct indx as indx_id from signalDf)
        	cross join (select distinct indx, signal from signalDf)
        ),
        
        sub
        as (
        	select b.indx_id 
        	, b.indx
        	, b.signal 
        	from base b 
        	join signalDf s on b.indx_id = s.indx
        	where b.indx >= s.indx 
        ), 
        
        m
        as (
        	select indx_id
        	, min(indx) as reset_indx
        	from sub
        	where signal in (1, 101)
        	group by 1
        ), 
        
        interim 
        as (
        	select m.indx_id 
        	, sum(case when b.signal = 0 then 1 else 2 end) as sellSignal
        	from m 
        	join base b on m.indx_id = b.indx_id and m.indx_id <= b.indx and m.reset_indx >= b.indx
        	where b.signal in (0, 10)
        	group by 1
        )
        
        select s.indx 
        , s.signal 
        , case when s.signal in (0, 10) and i.sellSignal >=2 then 1 else 0 end as sellSignal 
        from signalDf s 
        left join interim i on s.indx = i.indx_id
        ") %>%
                dplyr::select(index = indx, signal, sellSignal)
        
        # messageDf
        messageDf <- signalDf %>%
                dplyr::select(index = indx, everything()) %>%
                dplyr::inner_join(., buySignalDf, by = "index") %>%
                dplyr::inner_join(., sellSignalDf, by = "index") %>%
                dplyr::mutate(message = dplyr::case_when(buySignal == 1 ~ "buy",
                                                         sellSignal == 1 ~ "sell",
                                                         TRUE ~ "hold")) %>%
                dplyr::filter(index <= trading_days) %>%
                dplyr::select(-signal.x, -signal.y) %>%
                dplyr::select(date, index, everything(),
                              signal, buy = buySignal, sell = sellSignal, message)
        
        # historicalTransformation() - get indicators
        hT <- historicalTransformation(xtsObj, transformOnly = TRUE) %>%
                dplyr::select(date, open, close, rsi_close, cci_close, macd_close, signal_close, macd_close_diff)
        
        # set "buy", "sell", "hold" logic 
        strategyDf <- hT %>%
                dplyr::inner_join(., messageDf, by = "date") %>%
                dplyr::mutate(symbol = chartSymbol,
                              rolling_windows = rolling_windows,
                              rsi_flag = dplyr::case_when(message == "buy" & rsi_close >= rsi_support ~ 1,
                                                          message == "sell" & rsi_close < rsi_resistance ~ -1,
                                                          TRUE ~ 0),
                              cci_flag = dplyr::case_when(message == "buy" & cci_close >=0 & cci_close <= cci_uptrend_limit ~ 1,
                                                          message == "sell" & cci_close <0 & cci_close >= cci_downtrend_limit ~ -1,
                                                          TRUE ~ 0),
                              macd_flag = dplyr::case_when(message == "buy" & macd_close > 0 & macd_close > signal_close ~ 1,
                                                           message == "sell" & macd_close < signal_close ~ -1,
                                                           TRUE ~ 0),
                              message = dplyr::case_when(rsi_flag + cci_flag + macd_flag ==3 ~ "buy",
                                                         rsi_flag + cci_flag + macd_flag <=-2 ~ "sell",
                                                         TRUE ~ "hold")) %>%
                dplyr::select(index, date, symbol, open, close,
                              rsi_close, cci_close, macd_close, signal_close, macd_close_diff,
                              all_of(column_trak),
                              all_of(column_trak_name),
                              rolling_windows,
                              lower_bound, upper_bound,
                              aboveZero, sellCode, buyCode,
                              buy, sell,
                              signal, message)
        
        # return strategyDf
        return(strategyDf)

}
