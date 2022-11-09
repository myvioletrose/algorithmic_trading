RETURN <- function(xtsList, time1 = 365, time2 = 0, manual_time1, manual_time2, manual_time = FALSE, 
               periodReturn = c("daily", "weekly", "monthly", "quarterly", "annual", "yearly")){
        
        # set wd
        setwd(here::here("plot"))   
        
        # delete folder if exists; create it if it does not
        if("return" %in% dir()){unlink(x = "return", recursive = TRUE); dir.create("return")} else {dir.create("return")}
        
        # dump return charts to the "return" folder
        setwd(here::here("plot", "return"))
        
        # set time frame
        timeFrame <- function(days_behind){
                lookBack <- lubridate::today() -days_behind
                y <- lubridate::year(lookBack)
                m <- lubridate::month(lookBack) 
                m <- if(nchar(m) == 1){paste0("0", m)}
                timeFrameParameter <- paste0(y, m)
                return(timeFrameParameter)
        }
        
        if(manual_time){
                begin <- manual_time1
                end <- manual_time2
        } else {
                begin <- timeFrame(time1)
                end <- timeFrame(time2)        
        }
        
        ################# returnCal() function #################
        returnCal <- function(x, periodReturn){
                switch(periodReturn,
                       daily = dailyReturn(x),
                       weekly = weeklyReturn(x),
                       monthly = monthlyReturn(x),
                       quarterly = quarterlyReturn(x),
                       annual = annualReturn(x),
                       yearly = yearlyReturn(x)
                )
        }
        
        ################# plot function - autoplot, line or bar #################
        chart <- function(return_cal_type){
                
                chart_type <- ifelse(return_cal_type == "daily", "autoplot",
                                     ifelse(return_cal_type == "weekly", "line",
                                            "bar")) 
                        
                switch(chart_type,
                       autoplot = function(xts){
                               ggplot2::autoplot( cumprod(1 + returnCal(xts[paste0(begin, "::", end)], "daily")) ) + 
                                       geom_line(y = 1, col = "red", linetype = "dashed") +
                                       labs(x = "", y = "") + 
                                       theme_minimal() + ggtitle(paste0(symbol, " - ", return_cal_type, " return")) +
                                       theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))
                       },
                       line = function(xts){
                               xts <- cumprod(1 + returnCal(xts[paste0(begin, "::", end)], return_cal_type)) %>%
                                       as.data.frame %>%
                                       dplyr::mutate(date = row.names(.))
                               ggplot2::ggplot( xts, aes(x = date, 
                                                       y = xts[, grep("return", names(xts), ignore.case = TRUE, value = TRUE)] )) +
                                       geom_point() +
                                       geom_hline(yintercept = 1, col = "red", linetype = "dashed") + 
                                       labs(x = "", y = "") +
                                       theme_classic() + ggtitle(paste0(symbol, " - ", return_cal_type, " return")) +
                                       theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))
                       },
                       bar = function(xts){
                               xts <- cumprod(1 + returnCal(xts[paste0(begin, "::", end)], return_cal_type)) %>%
                                       as.data.frame %>%
                                       dplyr::mutate(date = row.names(.))
                               ggplot2::ggplot( xts, aes(x = date, 
                                                         y = xts[, grep("return", names(xts), ignore.case = TRUE, value = TRUE)] )) +
                                       geom_bar(stat = "identity") +
                                       geom_hline(yintercept = 1, col = "red", linetype = "dashed") + 
                                       labs(x = "", y = "") +
                                       theme_bw() + ggtitle(paste0(symbol, " - ", return_cal_type, " return")) +
                                       theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))
                       }
                )
        }
        
        ################# list_of_functions #################
        list_of_functions <- vector(mode = "list", length = length(periodReturn))
        
        for(i in 1:length(periodReturn)){
                list_of_functions[[i]] <- assign(periodReturn[i], chart(return_cal_type = periodReturn[i]))
        }
        
        names(list_of_functions) <- periodReturn
        
        ################# for() to switch, save charts #################
        for(i in 1:length(names(xtsList))){
                
                symbol <- names(xtsList)[i]
                name <- paste0(symbol, "_", begin, "_", end, ".png")
                x <- xts(xtsList[[symbol]])
                
                # create a chart list to host individual sub chart, e.g. daily, weekly, monthly, etc.
                chartList <- vector(mode = "list", length = length(list_of_functions))
                
                if( class(
                        try(
                                # create and save individual chart
                                for(j in 1:length(list_of_functions)){
                                        chartList[[j]] <- assign(periodReturn[j], list_of_functions[[j]](x))
                                }
                        )
                )  == "try-error" ){
                        print(paste0(symbol, " has no data"))
                        next
                } else {
                        # switch, save chart
                        switch(length(chartList),
                               gridExtra::grid.arrange(chartList[[1]],
                                                       ncol = 1) %>% ggsave(filename = name, plot = ., width = 13, height = 10),
                               gridExtra::grid.arrange(chartList[[1]], chartList[[2]],
                                                       ncol = 2) %>% ggsave(filename = name, plot = ., width = 13, height = 10),
                               gridExtra::grid.arrange(chartList[[1]], chartList[[2]], chartList[[3]],
                                                       ncol = 3) %>% ggsave(filename = name, plot = ., width = 13, height = 10),
                               gridExtra::grid.arrange(chartList[[1]], chartList[[2]], chartList[[3]], chartList[[4]],
                                                       ncol = 2) %>% ggsave(filename = name, plot = ., width = 13, height = 10),
                               gridExtra::grid.arrange(chartList[[1]], chartList[[2]], chartList[[3]], chartList[[4]], chartList[[5]],
                                                       ncol = 3) %>% ggsave(filename = name, plot = ., width = 13, height = 10),
                               gridExtra::grid.arrange(chartList[[1]], chartList[[2]], chartList[[3]], chartList[[4]], chartList[[5]], chartList[[6]],
                                                       ncol = 3) %>% ggsave(filename = name, plot = ., width = 13, height = 10)
                        )
                }
                
        }
        
        # reset wd
        setwd(here::here("code"))
        
}

# example
# try(
#         RETURN(xtsList,
#                manual_time1 = 201709,
#                manual_time2 = 201909,
#                manual_time = TRUE,
#                periodReturn = c("daily", "weekly", "monthly", "quarterly")),
#         silent = FALSE
# )