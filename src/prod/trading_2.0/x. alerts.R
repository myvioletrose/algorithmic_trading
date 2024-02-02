# define risk tolerance factor (downside risk) for calculation of stop-loss and trailing stop-loss
risk_tolerance = 2

# subset data
subset_date = "2014-01-01"
#subset_symbols = symbols

tic()
# buy/sell alerts
alerts <- indicators %>%
        filter(date >= subset_date) %>%
        #filter(date >= '2011-01-01' & date <= '2020-12-31') %>%
        filter(symbol %in% subset_symbols) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(
                # close lag, lead, near_future_flags
                close_lag1 = lag(close, 1),
                close_next2 = lead(close, 2),
                close_next4 = lead(close, 4),
                close_next10 = lead(close, 10),
                near_future_flag2 = case_when(close_next2 > close ~ 1,
                                              close_next2 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag4 = case_when(close_next4 > close ~ 1,
                                              close_next4 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag10 = case_when(close_next10 > close ~ 1,
                                               close_next10 < close ~ -1,
                                               TRUE ~ 0),
                
                # trailing_stop_loss
                trailing_stop_loss = close - risk_tolerance*atr,
                trailing_stop_loss_yesterday = dplyr::lag(trailing_stop_loss, 1),
                
                # sma_trend_flag, ema_trend_flag
                sma_pos_trend_flag = case_when(close > sma5 & close > sma8 & close > sma13 ~ 1, TRUE ~ 0),                
                ema_pos_trend_flag = case_when(close > ema5 & close > ema20 ~ 1, TRUE ~ 0),
                
                # buy alerts
                #v2_ce_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & ce_short_spike_flag == 1 & close > zlema ~ 1, TRUE ~ 0),
                #v21_ce_buy_alert = case_when(ema5 > ema20 & ce_short_spike_flag == 1 & close > zlema & (demark_flag == 1 | dcc_flag == 1 | ha_real_flag == 1 | ha_smooth_flag == 1 | macd_flag == 1 | evwma_flag == 1) ~ 1, TRUE ~ 0),

                #v2_dcc_buy_alert = case_when(ema5 > ema20 & dcc_flag == 1 & close > zlema ~ 1, TRUE ~ 0),
                v0_dcc_buy_alert = case_when(demark_flag != -1 & ema5 > ema20 & rsi > 50 & dcc_flag == 1 & close > zlema ~ 1, TRUE ~ 0),
                v1_dcc_buy_alert = case_when(demark_flag != -1 & ema5 > ema20 & rsi > 50 & dcc_flag == 1 & close > zlema & obv_flag == 1 ~ 1, TRUE ~ 0),
                #v3_dcc_buy_alert = case_when(ema5 > ema20 & dcc_flag == 1 & close > zlema & sma5_flag == 1 ~ 1, TRUE ~ 0),
                #v0_dcc_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & dcc_flag == 1 & close > zlema & sma5_flag == 1 ~ 1, TRUE ~ 0),
                #v11_dcc_buy_alert = case_when(ema5 > ema20 & dcc_flag == 1 & close > zlema & green_flag == 1 & (macd_trend_dir == 1 | obv_flag == 1) ~ 1, TRUE ~ 0),

                v1_demark_buy_alert = case_when(ema5 > ema20 & demark_flag == 1 ~ 1, TRUE ~ 0),
                v14_demark_buy_alert = case_when(demark_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ 1, TRUE ~ 0),

                #v3_evwma_buy_alert = case_when(demark_flag != -1 & rsi > 50 & evwma_flag == 1 & close > zlema & sma5_flag == 1 ~ 1, TRUE ~ 0),                
                
                v17_evwma_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & evwma_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) & obv_flag == 1 ~ 1, TRUE ~ 0),
                v21_evwma_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & evwma_flag == 1 & close > zlema & (macd_flag == 1 | demark_flag == 1 | dcc_flag == 1 | ha_real_flag == 1 | ha_smooth_flag == 1) ~ 1, TRUE ~ 0),

                #v2_macd_buy_alert = case_when(ema5 > ema20 & macd_flag == 1 & close > zlema ~ 1, TRUE ~ 0),
                #v3_macd_buy_alert = case_when(demark_flag != -1 & rsi > 50 & macd_flag == 1 & close > zlema & sma5_flag == 1 ~ 1, TRUE ~ 0),
                v14_macd_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & macd_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ 1, TRUE ~ 0),
                v24_macd_buy_alert = case_when(demark_flag != -1 & ema5 > ema20 & macd_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ 1, TRUE ~ 0),

                v4_sma5_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ema5 > ema20 & sma5_flag == 1 & close > zlema & sma_pos_trend_flag == 1 ~ 1, TRUE ~ 0),
                #v14_sma5_buy_alert = case_when(sma5_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) ~ 1, TRUE ~ 0),
                v17_sma5_buy_alert = case_when(demark_flag != -1 & rsi > 50 & sma5_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) & obv_flag == 1 ~ 1, TRUE ~ 0),
                v21_sma5_buy_alert = case_when(demark_flag != -1 & rsi > 50 & sma5_flag == 1 & close > zlema & (macd_flag == 1 | demark_flag == 1 | dcc_flag == 1 | ha_real_flag == 1 | ha_smooth_flag == 1 | evwma_flag == 1) ~ 1, TRUE ~ 0),                
                #v22_sma5_buy_alert = case_when(sma5_flag == 1 & close > zlema & (cci_oversold_flag == 1 | rsi_oversold_flag == 1) & (demark_flag == 1 | dcc_flag == 1 | ce_short_spike_flag == 1 | ha_real_flag == 1 | ha_smooth_flag == 1 | evwma_flag == 1) ~ 1, TRUE ~ 0),                

                #v2_ha_real_buy_alert = case_when(ha_real_flag == 1 & ema5 > ema20 & close > zlema ~ 1, TRUE ~ 0),
                v0_ha_real_buy_alert = case_when(demark_flag != -1 & rsi > 50 & ha_real_flag == 1 & ema5 > ema20 & close > zlema & green_flag == 1 & (macd_trend_dir == 1 | obv_flag == 1) ~ 1, TRUE ~ 0),
                v99_ha_real_buy_alert = case_when(demark_flag != -1 & ha_real_flag == 1 & close > zlema & green_flag == 1 & (rsi_trend_dir == 1 | cci_trend_dir == 1 | macd_trend_dir == 1 | obv_flag == 1) ~ 1, TRUE ~ 0),

                #v2_ha_smooth_buy_alert = case_when(ha_smooth_flag == 1 & ema5 > ema20 & close > zlema ~ 1, TRUE ~ 0),
                #v0_ha_smooth_buy_alert = case_when(ha_smooth_flag == 1 & ema5 > ema20 & close > zlema & green_flag == 1 & (macd_trend_dir == 1 | obv_flag == 1) ~ 1, TRUE ~ 0),
                new_flag10 = case_when(ha_real_flag == 1 & is_demark_entry_yn == 1 & (macd_trend_dir == 1 | macd_flag == 1 | obv_flag == 1 | ce_short_spike_flag == 1 | ema5_flag == 1) ~ 1, TRUE ~ 0),
                new_flag11 = case_when(ha_real_flag == 1 & is_demark_entry_yn == 1 & intraday_volatility_flag == 1 & (macd_trend_dir == 1 | macd_flag == 1 | obv_flag == 1 | ce_short_spike_flag == 1 | ema5_flag == 1) ~ 1, TRUE ~ 0),

                # sell alerts                
                #v1_ce_sell_alert = case_when(ce_long_dip_flag == 1 & ema5 < ema20 ~ 1, TRUE ~ 0),
                v1_evwma_sell_alert = case_when(evwma_flag == -1 & demark_flag != 1 & close < zlema & ema5 < ema20 ~ 1, TRUE ~ 0),
                v1_macd_sell_alert = case_when(macd_flag == -1 & demark_flag != 1 & close < zlema & ema5 < ema20 ~ 1, TRUE ~ 0),
                v1_demark_sell_alert = case_when(demark_flag == -1 & close < zlema ~ 1, TRUE ~ 0),
                #v1_sma5_sell_alert = case_when(sma5_flag == -1 & ema5 < ema20 ~ 1, TRUE ~ 0),
                #v1_dcc_sell_alert = case_when(dcc_flag == -1 & ema5 < ema20 ~ 1, TRUE ~ 0),
                v1_ha_real_sell_alert = case_when(ha_real_flag == -1 & demark_flag != 1 & close < zlema & ema5 < ema20 ~ 1, TRUE ~ 0),
                #v1_ha_smooth_sell_alert = case_when(ha_smooth_flag == -1 & ema5 < ema20 ~ 1, TRUE ~ 0),     
                v1_overbought_sell_alert = case_when((cci_overbought_flag == 1 | rsi_overbought_flag == 1) & close < zlema & ema5 < ema20 & demark_flag != 1 ~ 1, TRUE ~ 0),
                v1_profit_protect_sell_alert = case_when(close < trailing_stop_loss_yesterday ~ 1, TRUE ~ 0)
                
        ) %>%
        ungroup() %>%
        filter(date < '2024-01-01') %>%
        select(symbol, date, close, 
               ema5, ema20,
               sma5, sma8, sma13, sma50, sma200, 
               matches("_flag|_alert$")) %>%
        arrange(symbol, date)

dim(alerts)
toc()

######################################################################################################3
buy_alerts = na.omit(alerts) %>%
        select(symbol, date, close, proxy_flag, near_future_flag2, near_future_flag4, near_future_flag10,
               contains("_buy_alert")) %>%
        mutate(proxy_flag = case_when(proxy_flag == -1 ~ 0, TRUE ~ proxy_flag),
               near_future_flag2 = case_when(near_future_flag2 == -1 ~ 0, TRUE ~ near_future_flag2),
               near_future_flag4 = case_when(near_future_flag4 == -1 ~ 0, TRUE ~ near_future_flag4),
               near_future_flag10 = case_when(near_future_flag10 == -1 ~ 0, TRUE ~ near_future_flag10)) %>%
        arrange(symbol, date)

dim(buy_alerts)

sell_alerts = na.omit(alerts) %>%
        select(symbol, date, close, proxy_flag, near_future_flag2, near_future_flag4, near_future_flag10,
               contains("_sell_alert")) %>%
        mutate(proxy_flag = case_when(proxy_flag == -1 ~ 1, TRUE ~ 0),
               near_future_flag2 = case_when(near_future_flag2 == -1 ~ 1, TRUE ~ 0),
               near_future_flag4 = case_when(near_future_flag4 == -1 ~ 1, TRUE ~ 0),
               near_future_flag10 = case_when(near_future_flag10 == -1 ~ 1, TRUE ~ 0)) %>%
        arrange(symbol, date)

dim(sell_alerts)

#############################
buy_alerts2 = buy_alerts %>%
        tidyr::gather(key, value, -symbol, -date, -close) %>%
        mutate(value = as.factor(value)) %>%
        tidyr::spread(key, value) %>%
        select(symbol, date, close, proxy_flag, near_future_flag2, near_future_flag4, near_future_flag10, everything()) %>%
        tidyr::gather(key, value, -symbol, -date, -close, -proxy_flag, -near_future_flag2, -near_future_flag4, -near_future_flag10) %>%
        mutate(value = as.factor(value))

sell_alerts2 = sell_alerts %>%
        tidyr::gather(key, value, -symbol, -date, -close) %>%
        mutate(value = as.factor(value)) %>%
        tidyr::spread(key, value) %>%
        select(symbol, date, close, proxy_flag, near_future_flag2, near_future_flag4, near_future_flag10, everything()) %>%
        tidyr::gather(key, value, -symbol, -date, -close, -proxy_flag, -near_future_flag2, -near_future_flag4, -near_future_flag10) %>%
        mutate(value = as.factor(value))

list_of_buy_alerts = grep("alert", unique(buy_alerts2$key), ignore.case = TRUE, value = TRUE)
list_of_sell_alerts = grep("alert", unique(sell_alerts2$key), ignore.case = TRUE, value = TRUE)

#################################################################################################################
######################################### <<< results >>> ########################################
#confusionMatrix(data=predicted_value, reference = expected_value)

# buy alerts
temp_buy_alert_list = vector(mode = "list", length = length(list_of_buy_alerts))

for(i in 1:length(list_of_buy_alerts)){
        
        cat(list_of_buy_alerts[i])
        cat("\n")
        cat("\n")
        cm = with(buy_alerts2 %>%
                          filter(key == list_of_buy_alerts[i]),
                  caret::confusionMatrix(data = value,
                                         #reference = proxy_flag,
                                         reference = near_future_flag10,
                                         positive = "1"))
        print(cm)        
        fp = cm$table[2, 1]
        tp = cm$table[2, 2]
        p = cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname == "Precision") %>% .$.
        
        result = data.frame(fp = fp,
                            tp = tp,
                            precision = p,
                            rule = list_of_buy_alerts[i])
        
        temp_buy_alert_list[[i]] = result
        
        cat("<<<<<<<<<< done >>>>>>>>>>")
        cat("\n")
        
}

alert_eval = temp_buy_alert_list %>% 
        plyr::ldply() %>%
        tidyr::separate(rule, into = c("version", "alert_type"), 
                        extra = "merge",
                        remove = TRUE) %>%
        mutate(version = str_remove(version, "v") %>% as.integer) %>%
        group_by(alert_type) %>%
        arrange(desc(precision)) %>%
        mutate(ratio = round(tp / fp, 3),
               rank = row_number()) %>%
        ungroup() %>%
        select(version, alert_type, tp, fp, ratio, precision, rank) %>%
        arrange(alert_type, version) 

alert_eval
# # A tibble: 10 × 7
# version alert_type          tp    fp ratio precision  rank
# <int> <chr>            <int> <int> <dbl>     <dbl> <int>
# 1       1 dcc_buy_alert      164   118  1.39     0.582     1
# 2       1 demark_buy_alert  1943  1770  1.10     0.523     1
# 3       1 evwma_buy_alert     78    37  2.11     0.678     1
# 4       1 ha_buy_alert       151   149  1.01     0.503     1
# 5       1 macd_buy_alert      50    31  1.61     0.617     1
# 6       1 sma_buy_alert      753   596  1.26     0.558     2
# 7       2 sma_buy_alert      215   183  1.18     0.540     3
# 8       3 sma_buy_alert       29    16  1.81     0.644     1
# 9       4 sma_buy_alert       30    29  1.03     0.508     5
# 10      5 sma_buy_alert      629   594  1.06     0.514     4

###################
# sell alert
temp_sell_alert_list = vector(mode = "list", length = length(list_of_sell_alerts))

for(i in 1:length(list_of_sell_alerts)){
        
        cat(list_of_sell_alerts[i])
        cat("\n")
        cat("\n")
        cm = with(sell_alerts2 %>%
                          filter(key == list_of_sell_alerts[i]),
                  caret::confusionMatrix(data = value,
                                         #reference = proxy_flag,
                                         reference = near_future_flag10,
                                         positive = "1"))
        print(cm)        
        fp = cm$table[2, 1]
        tp = cm$table[2, 2]
        p = cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname == "Precision") %>% .$.
        
        result = data.frame(fp = fp,
                            tp = tp,
                            precision = p,
                            rule = list_of_sell_alerts[i])
        
        temp_sell_alert_list[[i]] = result
        
        cat("<<<<<<<<<< done >>>>>>>>>>")
        cat("\n")
        
}

alert_eval2 = temp_sell_alert_list %>% 
        plyr::ldply() %>%
        tidyr::separate(rule, into = c("version", "alert_type"), 
                        extra = "merge",
                        remove = TRUE) %>%
        arrange(desc(precision)) %>%
        mutate(ratio = round(tp / fp, 3),
               rank = row_number()) %>%
        select(version, alert_type, tp, fp, ratio, precision, rank) %>%
        arrange(rank) 

alert_eval2
# version         alert_type   tp   fp ratio precision rank
# 1  profit protect_sell_alert  481  483 0.996 0.4989627    1
# 2      ce         sell_alert 2139 2217 0.965 0.4910468    2
# 3    macd         sell_alert 2154 2728 0.790 0.4412126    3
# 4    sma5         sell_alert  719  979 0.734 0.4234393    4

#################################################################################################
###################################################################

tic()
volume_alerts <- indicators %>%
        filter(date >= subset_date) %>%
        #filter(date >= '2011-01-01' & date <= '2020-12-31') %>%
        filter(symbol %in% subset_symbols) %>%
        arrange(symbol, date) %>%
        group_by(symbol) %>%
        dplyr::mutate(
                # close lag, lead, near_future_flags
                close_lag1 = lag(close, 1),
                close_next2 = lead(close, 2),
                close_next4 = lead(close, 4),
                close_next10 = lead(close, 10),
                near_future_flag2 = case_when(close_next2 > close ~ 1,
                                              close_next2 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag4 = case_when(close_next4 > close ~ 1,
                                              close_next4 < close ~ -1,
                                              TRUE ~ 0),
                near_future_flag10 = case_when(close_next10 > close ~ 1,
                                               close_next10 < close ~ -1,
                                               TRUE ~ 0),
                volume_buy_alert1 = case_when(volume_inconsistency_alert == "bearish inconsistency" & demark_flag == 1 ~ 1, TRUE ~ 0),
                volume_buy_alert2 = case_when(volume_inconsistency_alert == "bullish inconsistency" & demark_flag == 1 ~ 1, TRUE ~ 0),
                
                volume_sell_alert1 = case_when(volume_inconsistency_alert == "bullish inconsistency" & demark_flag == -1 ~ 1, TRUE ~ 0),
                volume_sell_alert2 = case_when(volume_inconsistency_alert == "bearish inconsistency" & demark_flag == -1 ~ 1, TRUE ~ 0),
        ) %>%
        ungroup() %>%
        select(symbol, date, is_today, 
               close, 
               #today_support, support, 
               csp_bullish_candle, volume_inconsistency_alert,
               #message_s, message_e0, message_e1, message_e2, 
               rsi, cci, 
               sma5, sma50, sma200, ema5, ema20, 
               cci_oversold_flag, 
               rsi_oversold_flag, 
               obv_flag, 
               demark_flag, 
               ce_short_spike_flag, 
               dcc_flag, evwma_flag, 
               ha_real_flag, 
               ha_smooth_flag, 
               macd_flag, 
               sma5_flag,
               ema5_flag,
               proxy_flag,
               matches("near_future|volume_buy_alert|volume_sell_alert")) %>%
        arrange(symbol, date)
toc()

##################################################
with(volume_alerts,
     prop.table(ftable(demark_flag, proxy_flag), margin = 1))

with(volume_alerts,
     prop.table(ftable(demark_flag, 
                       #proxy_flag
                       near_future_flag2
     ), margin = 1))

#####################################
with(volume_alerts,
     prop.table(ftable(volume_buy_alert2, 
                       proxy_flag), margin = 1))

with(volume_alerts,
     prop.table(ftable(volume_buy_alert1, 
                       #proxy_flag
                       near_future_flag2
                       ), margin = 1))

######################################################
with(volume_alerts,
     prop.table(ftable(volume_sell_alert2, 
                       proxy_flag), margin = 1))

with(volume_alerts,
     prop.table(ftable(volume_sell_alert2, 
                       #proxy_flag
                       near_future_flag2
     ), margin = 1))









