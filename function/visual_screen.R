visual_screen <- function(df, s, start_date, end_date, dollar_by, support, gchart_num){
        
        # "buy - dcc, buy - demark (v0), buy - demark (v1), buy - demark hybrid, buy - evwma (v0), buy - ha_real, buy - macd, buy - rising, buy - sma (v1), sma alert"
        buy_line_keywords = "buy - demark|buy - demark hybrid|buy - evwma|buy - ha_real|buy - macd|buy - sma"        
        
        #"down_alert - ce, down_alert - demark, down_alert - evwma, down_alert - ha_real, down_alert - macd, down_alert - overbought, sell - profit protect"
        sell_line_keywords = "down_alert - ce|down_alert - evwma|down_alert - ha_real|down_alert - macd|sell - profit protect"        
        
        rising_keyword = "buy - rising"
        alert_keywords = "near future spike alert|sma_alert"
        
        #######################################
        # visual object
        visObj = df %>%
                filter(symbol == s &
                               date >= start_date) %>%
                # join with ha to get ha time series
                inner_join(ha %>%
                                   filter(symbol == s & date >= start_date) %>%
                                   select(symbol, date, 
                                          # ha
                                          open_ha, high_ha, low_ha, close_ha,
                                          # ha - smoothed
                                          open_ha_ema, high_ha_ema, low_ha_ema, close_ha_ema),
                           by = c("symbol", "date"))
        
        #######################################
        # g1: basic - Candlestick + Donchian Channels
        g1 = ggplot(data = visObj, aes(x = date, y = close, group = symbol)) +
                geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                 colour_up = "darkgreen", colour_down = "darkred",
                                 fill_up  = "darkgreen", fill_down  = "darkred") +
                bdscale::scale_x_bd(business.dates = visObj %>% select(date) %>% distinct %>% .$date,
                                    max.major.breaks = length(unique(visObj$date)),
                                    labels = scales::date_format("%Y-%m-%d")) +
                geom_line(aes(x = date, y = dcc_high), color = "red", linetype = "dashed") +
                geom_line(aes(x = date, y = dcc_mid), color = "grey") +
                geom_line(aes(x = date, y = dcc_low), color = "red", linetype = "dashed") +
                labs(title = s,
                     x = "", 
                     y = "") +
                coord_x_date(xlim = c(start_date, end_date)) +
                scale_y_continuous(position = "right",
                                   labels = scales::dollar,
                                   breaks = ceiling(seq(min(visObj$low), max(visObj$high), 
                                                        by = dollar_by))) + 
                #theme_tq() +
                #theme_classic() +
                theme_bw() + 
                theme(legend.position = "top", 
                      plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(hjust = 1, angle = 60),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.title = element_blank(),
                      strip.background = element_blank())
        
        # g2: basic + Heikin Ashi smoothed
        g2 = g1 + geom_candlestick(aes(open = open_ha_ema, high = high_ha_ema, low = low_ha_ema, close = close_ha_ema), alpha = 0.2)
        
        # g3: basic + Heikin Ashi smoothed + ema5 + ema20
        g3 = g2 + geom_line(aes(x = date, y = evwma), col = "darkorange") +
                geom_line(aes(x = date, y = ema5), col = "darkred", linetype = "solid") +
                geom_line(aes(x = date, y = ema20), col = "blue", linetype = "longdash")
        #+ geom_line(aes(x = date, y = zlema), col = "purple")
        
        # g4: basic + Heikin Ashi smoothed + ema5 + ema20 + support/resistance line(s) + buy signal line(s)
        g4 = g3 + geom_vline(aes(xintercept = date),
                             data = visObj %>%
                                     filter(symbol == s) %>%
                                     filter(grepl(buy_line_keywords, message_s, ignore.case = TRUE)),
                             col = "green", 
                             alpha = 0.25) +
                geom_vline(aes(xintercept = date),
                           data = visObj %>%
                                   filter(symbol == s) %>%
                                   filter(grepl(sell_line_keywords, message_s, ignore.case = TRUE)),
                           col = "red", 
                           alpha = 0.25) +
                geom_vline(aes(xintercept = date),
                           data = visObj %>%
                                   filter(symbol == s) %>%
                                   filter(grepl(rising_keyword, message_s, ignore.case = TRUE)),
                           col = "purple", 
                           alpha = 0.25) +
                geom_vline(aes(xintercept = date),
                           data = visObj %>%
                                   filter(symbol == s) %>%
                                   filter(grepl(alert_keywords, message_s, ignore.case = TRUE)),
                           col = "gold", 
                           alpha = 0.25)
        
        # g5: basic + Heikin Ashi smoothed + ema5 + ema20 + support/resistance line(s)
        if(!any(support < 0)){
                g5 = g4 + geom_hline(yintercept = support, linetype = "dotted", linewidth = 0.5)
        } else {
                g5 = g4
        }
        
        # render chart
        output = switch(gchart_num, g1, g2, g3, g4, g5)
        
        return(output)
        
}