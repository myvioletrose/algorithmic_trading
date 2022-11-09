MaChart <- function(df, column_a = "close", first_sma = 5, second_sma = 20, trading_days = 50, savePlot = FALSE){
        
        first_sma_column = paste0("d", first_sma)
        second_sma_column = paste0("d", second_sma)

        chart <- wrapr::let(c(col_a = column_a,
                              first_sma_column_name = first_sma_column,
                              second_sma_column_name = second_sma_column),
                            df %>%
                                    dplyr::select(date, all_of(column_a)) %>%
                                    dplyr::mutate(first_sma_column_name = TTR::SMA(col_a, first_sma),
                                                  second_sma_column_name = TTR::SMA(col_a, second_sma),
                                                  index = nrow(.):1) %>%
                                    dplyr::filter(index <= trading_days) %>%
                                    dplyr::select(date, all_of(column_a), all_of(first_sma_column), all_of(second_sma_column)) %>%
                                    tidyr::gather(key, value, -date) %>%
                                    dplyr::mutate(key = factor(.$key, levels = c(column_a, first_sma_column, second_sma_column))) %>%
                                    ggplot(aes(date, value)) + 
                                    geom_line(aes(col = key)) +
                                    labs(x = "", y = "") +
                                    #scale_x_date(labels = scales::date_format("%Y-%m-%d"), date_breaks = date_breaks) +
                                    bdscale::scale_x_bd(business.dates = df %>% 
                                                                dplyr::filter(index <= trading_days) %>%
                                                                lplyr::pull(date) %>%
                                                                unique,
                                                        max.major.breaks = trading_days,
                                                        labels = scales::date_format("%Y-%m-%d")) +
                                    scale_y_continuous(labels = scales::dollar) +
                                    scale_color_manual(values = c("red", "blue", "yellow")) +
                                    #theme_bw() +
                                    #theme_modern() +
                                    theme_abyss() +
                                    #theme_blackboard() +
                                    #theme_linedraw() +
                                    #theme_lucid() +
                                    theme(legend.position = "top", 
                                          plot.title = element_text(hjust = 0.5),
                                          axis.text.x = element_text(hjust = 1, angle = 60),
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          legend.title = element_blank()) +
                                    ggtitle(paste0(chartSymbol, 
                                                   " Close ($) Moving Average(s) for past ", 
                                                   trading_days, 
                                                   " trading days")))
        
        if(savePlot){
                ggsave(filename = paste0(paste(chartSymbol, column_a, "moving_average", "for_past", trading_days, "trading_days", sep = "_"), ".png"), plot = chart)
        } else {
                return(chart)        
        }
        
}

