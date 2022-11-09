# Reward Risk Ratio

R3 <- function(df, col, col2, target = 0.05, stop = -0.05){
    
    d <- df$date %>% unique %>% max()
    
    s <- df$symbol %>% unique %>% as.character()
    
    m <- wrapr::let(c(target_column = col), 
                    df %>% select(target_column) %>%
                        .$target_column %>%
                        mean)
    
    sd <- wrapr::let(c(target_column = col), 
                     df %>% select(target_column) %>%
                         .$target_column %>%
                         sd)
    
    # col = "lag_change_trak"
    current_lag_change <- wrapr::let(c(target_column = col), 
                                     df %>% 
                                         arrange(desc(date)) %>%
                                         select(target_column) %>%
                                         .$target_column)
    
    c1 <- current_lag_change[1]
    c2 <- current_lag_change[2]
    acceleration1 = c1 + c2
    
    # col2 = "lag_change_trak_MA_n2"
    current_lag_change_n2 <- wrapr::let(c(target_column = col2), 
                                        df %>% 
                                            arrange(desc(date)) %>%
                                            select(target_column) %>%
                                            .$target_column)
    
    x1 <- current_lag_change_n2[1]
    x2 <- current_lag_change_n2[2]
    acceleration2 = x1 + x2
    
    up <- pnorm(target, m, sd, lower.tail = FALSE)
    
    down <- pnorm(stop, m, sd, lower.tail = TRUE)
    
    r3 = round(up / down, 4)
    
    parameters = list(d, s, m, sd, 
                      c1, acceleration1, 
                      x1, acceleration2, 
                      target, stop, up, down, r3)
    
    names(parameters) = c("date", "symbol", "mean", "sd", 
                          "current", "acceleration1",
                          "current_n2", "acceleration2", 
                          "target", "stop", "up", "down", "reward_risk_ratio")
    
    return(parameters)
    
}
