########################################################################
# look back: 504 (24M) / 252 (12M) / 189 (9M) / 126 (6M) / 63 (3M)
windows()
days_look_back = 252
days_look_back_flagged_buy_signal = 10
end_date = t0$date %>% max() 

unique_trading_date = t0 %>% filter(symbol == "SPY") %>% select(date) %>% arrange(date) %>% distinct %>% .$date
target_date_index = which(unique_trading_date == end_date)
if(length(target_date_index) == 0){target_date_index = length(unique_trading_date)}
start_date = unique_trading_date[target_date_index -days_look_back]
start_date = as.Date(start_date)

i = "HAL"
start_date
end_date
dollar_by = 1
support = 120

# visual object
visObj = poc %>%
        filter(symbol == i & 
                       date >= start_date) %>%
        # join with ha to get ha time series
        inner_join(ha %>%
                           filter(symbol == i & date >= start_date) %>%
                           select(symbol, date, 
                                  # ha
                                  open_ha, high_ha, low_ha, close_ha,
                                  # ha - smoothed
                                  open_ha_ema, high_ha_ema, low_ha_ema, close_ha_ema),
                   by = c("symbol", "date"))

# basic graph - g0
g0 = ggplot(data = visObj, aes(x = date, y = close, group = symbol)) +
        geom_candlestick(aes(open = open, high = high, low = low, close = close),
                         colour_up = "darkgreen", colour_down = "darkred",
                         fill_up  = "darkgreen", fill_down  = "darkred") +
        bdscale::scale_x_bd(business.dates = visObj %>% select(date) %>% distinct %>% .$date,
                            max.major.breaks = length(unique(visObj$date)),
                            labels = scales::date_format("%Y-%m-%d")) +
        geom_line(aes(x = date, y = dcc_high), color = "red", linetype = "dashed") +
        geom_line(aes(x = date, y = dcc_mid), color = "grey") +
        geom_line(aes(x = date, y = dcc_low), color = "red", linetype = "dashed") +
        labs(title = i,
             x = "", 
             y = "") +
        coord_x_date(xlim = c(start_date, end_date)) +
        scale_y_continuous(labels = scales::dollar,
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

g1 = g0 + geom_candlestick(aes(open = open_ha_ema, high = high_ha_ema, low = low_ha_ema, close = close_ha_ema), alpha = 0.2)
g2 = g1 + geom_line(aes(x = date, y = evwma), col = "darkorange")
        #+ geom_line(aes(x = date, y = zlema), col = "purple")

g2
#g2 + geom_hline(yintercept = support, linetype = "dotted", linewidth = 0.5)

#######################################################
# high = 130
# low = 52
# fibonacci_seq = fibonacci(high, low)$DR
# 
# g3 = g2 + geom_hline(yintercept = fibonacci_seq,
#                      linetype = "dotted",
#                      linewidth = 0.1,
#                      color = "darkblue")
# 
# g3













