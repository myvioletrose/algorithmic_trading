TA <- function(xtsList, 
               start_day_minus = 730, 
               end_day_minus = 0,
               start_datekey,
               end_datekey,
               input_datekey_flag = FALSE,
               save_plot_path = "~",
               folder_name = "TA charts",
               heikin_ashi = FALSE
){
        
        # check if packages available
        # packages <- c("tidyverse", "quantmod", "xts") 
        # invisible( sapply(packages, jayden::CheckAndLoad) )
        
        # path for saving all the TA charts
        path <- paste(save_plot_path, folder_name, sep = "/")
        
        # delete folder if exists and then create one again; create it if it does not exist
        if(dir.exists(path)){unlink(path, recursive = TRUE); dir.create(path)} else {dir.create(path)}
        
        # set time frame
        timeFrame <- function(days_behind){
                lookBack <- lubridate::today() -days_behind
                y <- lubridate::year(lookBack)
                m <- lubridate::month(lookBack) 
                m <- if(nchar(m) == 1){paste0("0", m)}
                timeFrameParameter <- paste0(y, m)
                return(timeFrameParameter)
        }
        
        # date range input
        if(input_datekey_flag){
                begin <- start_datekey
                end <- end_datekey
        } else {
                begin <- timeFrame(start_day_minus)
                end <- timeFrame(end_day_minus)        
        }
        
        # create TA charts using for() loop
        for(i in 1:length(xtsList)){
                symbol <- names(xtsList)[i]
                name <- paste0(symbol, "_", begin, "_to_", end, ".png")
                png(filename = paste(path, name, sep = "/"), width = 1500, height = 1200, res = 100)

                if(!heikin_ashi){
                        x <- xts::xts(xtsList[[symbol]])
                        quantmod::chartSeries( x[paste0(begin, "::", end)],                                 
                                       name = symbol,
                                       TA = c(addBBands(draw = 'bands'), 
                                              #addADX(), 
                                              addCCI(), 
                                              addRSI(),
                                              addMACD(),
                                              addEMA(n = 10, col = 'red'),
                                              addEMA(n = 30, col = 'blue'),
                                              addOBV(),
                                              #addCMF(), 
                                              #addBBands(draw = 'width'), 
                                              #addBBands(draw = 'percent'), 
                                              addVo()) )
                } else {
                        x <- xts::xts(xtsList[[symbol]]) %>% heikin_ashi(., output_as_df = FALSE)
                        quantmod::chartSeries( x[paste0(begin, "::", end)],                                 
                                       name = paste0(symbol, " (Heikin Ashi)"),
                                       TA = c(addBBands(draw = 'bands'), 
                                              #addADX(), 
                                              addCCI(), 
                                              addRSI(),
                                              addMACD(),
                                              addEMA(n = 10, col = 'red'),
                                              addEMA(n = 30, col = 'blue')                                              
                                              ))
                }
                
                dev.off()
        }
        
}