ctrlChart <- function(x, trading_days = 50, chartSymbol, column_trak, lday, rolling_windows, savePlot = FALSE){
        
        chart <- x %>%
                dplyr::filter(index <= trading_days) %>%
                ggplot(., aes(date, value, col = key)) +
                geom_line(aes(col = key)) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                bdscale::scale_x_bd(business.dates = x %>% dplyr::filter(index <= trading_days) %>% lplyr::pull(date) %>% unique,
                                    max.major.breaks = trading_days,
                                    labels = scales::date_format("%Y-%m-%d")) +
                scale_y_continuous(labels = scales::percent) +
                #theme_minimal() +
                theme_lucid() +
                theme(legend.position = "top", 
                      plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(hjust = 1, angle = 60),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.title = element_blank()) +
                ggtitle(paste0(chartSymbol, 
                               " ", 
                               column_trak, 
                               " lday = ", 
                               lday, 
                               " and rolling-windows = ", 
                               rolling_windows))
        
        if(savePlot){
                ggsave(filename = paste0(paste(chartSymbol, column_trak, "lag", lday, rolling_windows, sep = "_"), ".png"), plot = chart)
        } else {
                return(chart)        
        }
        
}


