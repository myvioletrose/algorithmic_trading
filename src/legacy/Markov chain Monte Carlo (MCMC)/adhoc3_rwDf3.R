##############################################################################
################ Monte Carlo - get t.value for n = 1, 5, 20 ################
#################################################################

# get t.value for n = 1, 5, 20
temp.ttest.df <- rwDf %>% 
    dplyr::select(current_date, ticker, d, n, rw.mu, rw.sigma) %>%
    tidyr::gather(parameters, value, rw.mu:rw.sigma) %>%
    tidyr::unite(d, d, parameters, sep = " - ") %>%
    tidyr::spread(d, value)

# n == 1
short = temp.ttest.df %>%
    dplyr::filter(n == 1) %>%
    tidyr::gather(key, value, 4:11) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::spread(key, value)
names(short) <- c("current_date", "ticker", "n",
                  "1st.mu", "1st.sigma",
                  "2nd.mu", "2nd.sigma")

# n == 5
mid = temp.ttest.df %>%
    dplyr::filter(n == 5) %>%
    tidyr::gather(key, value, 4:11) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::spread(key, value)
names(mid) <- c("current_date", "ticker", "n",
                "1st.mu", "1st.sigma",
                "2nd.mu", "2nd.sigma")

# n == 20
long = temp.ttest.df %>%
    dplyr::filter(n == 20) %>%
    tidyr::gather(key, value, 4:11) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::spread(key, value)
names(long) <- c("current_date", "ticker", "n",
                 "1st.mu", "1st.sigma",
                 "2nd.mu", "2nd.sigma")

# combined
temp.ttest.df2 <- dplyr::bind_rows(short, mid, long) %>%
    dplyr::mutate(t.value = (`1st.mu` - `2nd.mu`) / sqrt((`1st.sigma`^2 / sim) + (`2nd.sigma`^2 / sim))) %>%
    dplyr::inner_join(rwDf %>% dplyr::select(current_date, ticker, current.p, current.p.down, current.p.up) %>% distinct(),
                      by = c("current_date", "ticker")) %>%
    dplyr::select(current_date, ticker, n, current.p, current.p.down, current.p.up, everything()) %>%
    dplyr::arrange(current_date, ticker, n)

col_names = sapply(temp.ttest.df2, class)[sapply(temp.ttest.df2, class) == "numeric"] %>% 
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    dplyr::select(col_names = rowname) %>%
    .$col_names

temp.ttest.df2 <- temp.ttest.df2 %>%
    dplyr::mutate_at(col_names, function(x) round(x, 3))

temp.ttest.df3 <- temp.ttest.df2 %>%
    dplyr::select(current_date, ticker, n, t.value) %>%
    tidyr::spread(n, t.value) %>%
    dplyr::select(ticker, 
                  t.value.n1 = `1`, 
                  t.value.n5 = `5`, 
                  t.value.n20 = `20`)

temp.ttest.df3 %>% dplyr::arrange(desc(t.value.n1))

#write.table(temp.ttest.df3, "clipboard-16384", sep = "\t", row.names = FALSE)


######################

##############################################################################
################ Markov Chain - chisq.test ################
#################################################################

end_index = c(21, 51, 101)
outputList2 <- list()

for(i in 1:length(chartSymbol)){
    
    mc <- chartSymbol[i]
    
    for(j in 1:length(end_index)){
        
        e = end_index[j]
        
        transition_matrix <- macd_flag2 %>%
            dplyr::filter(symbol == mc & index >= start_index & index <= e) %>%
            dplyr::select(status_current, status_next) %>%
            ftable(.$status_current, .$status_next) %>%
            prop.table(margin = 1) %>%
            as.matrix()
        
        if(class(try(
            output <- data.frame(ticker = mc,
                                 look_back_days = e,
                                 stationary_dist_label = c("down", "stay", "up")) %>%
            dplyr::mutate(stationary_dist = eigen(t(transition_matrix))$vectors[,1]/
                          sum(eigen(t(transition_matrix))$vectors[,1]))
        )) == "try-error"){
            next
        } else {
            output <- data.frame(ticker = mc,
                                 look_back_days = e,
                                 stationary_dist_label = c("down", "stay", "up")) %>%
                dplyr::mutate(stationary_dist = eigen(t(transition_matrix))$vectors[,1]/
                                  sum(eigen(t(transition_matrix))$vectors[,1]))
        }
        
        outputList2 <- c(outputList2, list(output))
        
    }
    
}

# chisq.test()$p.value
chisq.df <- outputList2 %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(stationary_dist = as.numeric(stationary_dist)) %>%
    tidyr::spread(stationary_dist_label, stationary_dist)

outputList3 <- list()

for(p in 1:length(chartSymbol)){
    
    s = chartSymbol[p]
    
    p.value <- chisq.df %>%
        dplyr::filter(ticker == s) %>%
        dplyr::mutate_at(c("down", "stay", "up"), function(x) ceiling(x*100)) %>%
        dplyr::select(down, stay, up) %>%
        as.matrix() %>% 
        as.table() %>%
        stats::chisq.test() %>%
        .$p.value
    
    output <- data.frame(ticker = s,
                         p.value = p.value)
    
    outputList3 <- c(outputList3, list(output))
    
}

chisq.df2 <- outputList3 %>%
    dplyr::bind_rows() %>%
    dplyr::inner_join(., chisq.df, by = "ticker") %>%
    tidyr::gather(key, value, down:up) %>%
    tidyr::unite(key, key, look_back_days, sep = " - ") %>%
    tidyr::spread(key, value) %>%
    dplyr::select(ticker, p.value,
                  `down - 21`,
                  `down - 51`,
                  `down - 101`,
                  `stay - 21`,
                  `stay - 51`,
                  `stay - 101`,
                  `up - 21`,
                  `up - 51`,
                  `up - 101`)

col_names2 <- c("p.value",
                "down - 21",
                "down - 51",
                "down - 101",
                "stay - 21",
                "stay - 51",
                "stay - 101",
                "up - 21",
                "up - 51",
                "up - 101")

chisq.df3 <- chisq.df2 %>%
    dplyr::mutate_at(col_names2, function(x) round(x, 3))

##############################################################################
################ rwDf3 ################
#################################################################

rwDf2 <- rwDf %>%
    tidyr::gather(key, value, current.p:reward_risk_ratio_log) %>%
    tidyr::unite(monte_carlo_combo, d, n, key, sep = "_") %>%
    tidyr::spread(monte_carlo_combo, value) %>%
    dplyr::select(current_date, ticker,
                  current.p = `11_1_current.p`, 
                  current.p.down = `11_1_current.p.down`, 
                  current.p.up = `11_1_current.p.up`,
                  `11_1_rw.mu`, `11_1_rw.sigma`, `11_1_rw.se`, `11_1_mu_range`, `11_1_dist_range`, `11_1_up`, `11_1_down`, 
                  `11_1_reward_risk_ratio_log`, 
                  `21_1_rw.mu`, `21_1_rw.sigma`, `21_1_rw.se`, `21_1_mu_range`, `21_1_dist_range`, `21_1_up`, `21_1_down`,
                  `21_1_reward_risk_ratio_log`, 
                  `21_5_rw.mu`, `21_5_rw.sigma`, `21_5_rw.se`, `21_5_mu_range`, `21_5_dist_range`, `21_5_up`, `21_5_down`,
                  `21_5_reward_risk_ratio_log`, 
                  `51_5_rw.mu`, `51_5_rw.sigma`, `51_5_rw.se`, `51_5_mu_range`, `51_5_dist_range`, `51_5_up`, `51_5_down`, 
                  `51_5_reward_risk_ratio_log`, 
                  `51_20_rw.mu`, `51_20_rw.sigma`, `51_20_rw.se`, `51_20_mu_range`, `51_20_dist_range`, `51_20_up`, `51_20_down`, 
                  `51_20_reward_risk_ratio_log`, 
                  `101_20_rw.mu`, `101_20_rw.sigma`, `101_20_rw.se`, `101_20_mu_range`, `101_20_dist_range`, `101_20_up`, `101_20_down`, 
                  `101_20_reward_risk_ratio_log` 
    ) %>%
    dplyr::mutate(mu = paste0("11_1: [$", round(as.numeric(`11_1_rw.mu`), 1), "], ",
                              "21_1: [$", round(as.numeric(`21_1_rw.mu`), 1), "], ",
                              "21_5: [$", round(as.numeric(`21_5_rw.mu`), 1), "], ",
                              "51_5: [$", round(as.numeric(`51_5_rw.mu`), 1), "], ",
                              "51_20: [$", round(as.numeric(`51_20_rw.mu`), 1), "], ",
                              "101_20: [$", round(as.numeric(`101_20_rw.mu`), 1), "]"),
                  dist_range = paste0("11_1: ", `11_1_dist_range`, ", ",
                                      "21_1: ", `21_1_dist_range`, ", ",
                                      "21_5: ", `21_5_dist_range`, ", ",
                                      "51_5: ", `51_5_dist_range`, ", ",
                                      "51_20: ", `51_20_dist_range`, ", ",
                                      "101_20: ", `101_20_dist_range`),
                  up.prob = paste0("11_1: [", round(as.numeric(`11_1_up`) * 100, 1), "%], ",
                                   "21_1: [", round(as.numeric(`21_1_up`) * 100, 1), "%], ",
                                   "21_5: [", round(as.numeric(`21_5_up`) * 100, 1), "%], ",
                                   "51_5: [", round(as.numeric(`51_5_up`) * 100, 1), "%], ",
                                   "51_20: [", round(as.numeric(`51_20_up`) * 100, 1), "%], ",
                                   "101_20: [", round(as.numeric(`101_20_up`) * 100, 1), "%]"),
                  down.prob = paste0("11_1: [", round(as.numeric(`11_1_down`) * 100, 1), "%], ",
                                     "21_1: [", round(as.numeric(`21_1_down`) * 100, 1), "%], ",
                                     "21_5: [", round(as.numeric(`21_5_down`) * 100, 1), "%], ",
                                     "51_5: [", round(as.numeric(`51_5_down`) * 100, 1), "%], ",
                                     "51_20: [", round(as.numeric(`51_20_down`) * 100, 1), "%], ",
                                     "101_20: [", round(as.numeric(`101_20_down`) * 100, 1), "%]"),
                  reward_risk_ratio = paste0("11_1: [", round(as.numeric(`11_1_reward_risk_ratio_log`), 2), "], ",
                                             "21_1: [", round(as.numeric(`21_1_reward_risk_ratio_log`), 2), "], ",
                                             "21_5: [", round(as.numeric(`21_5_reward_risk_ratio_log`), 2), "], ",
                                             "51_5: [", round(as.numeric(`51_5_reward_risk_ratio_log`), 2), "], ",
                                             "51_20: [", round(as.numeric(`51_20_reward_risk_ratio_log`), 2), "], ",
                                             "101_20: [", round(as.numeric(`101_20_reward_risk_ratio_log`), 2), "]"),
                  low_hanging_fruit = dplyr::case_when(as.numeric(`11_1_rw.mu`) > as.numeric(current.p.up) ~1, TRUE ~0),
                  expectation = dplyr::case_when(as.numeric(`11_1_rw.mu`) > as.numeric(current.p) ~1, TRUE ~0)) %>%
    dplyr::select(current_date, ticker, low_hanging_fruit, expectation, everything())

rwDf3 <- rwDf2 %>%
    dplyr::left_join(., temp.ttest.df3, by = "ticker") %>%
    dplyr::left_join(., chisq.df3, by = "ticker") %>%
    arrange(ticker)

#write.table(rwDf3, "clipboard-16384", sep = "\t", row.names = FALSE)

##########################################################################################
################## new_target_flag evaluation
SymbolList = macd_flag2$symbol %>% unique()

entry_col_for_eval = "entry_m"
exit_col_for_eval = "exit_m"

X = wrapr::let(c(entry_col = entry_col_for_eval,
                 exit_col = exit_col_for_eval),
               macd_flag2 %>%
                   dplyr::filter((symbol %in% SymbolList) & index >4) %>%
                   dplyr::select(symbol, entry1 = entry_col, exit1 = exit_col, new_target_flag)
)

Y = split(X, X$symbol)
Z1 = vector(mode = "list", length = length(Y))
Z2 = vector(mode = "list", length = length(Y))

for(i in 1:length(Y)){
    
    s = names(Y)[i]
    y = Y[[i]]
    Z1[[i]] = ftable(y$entry1, y$new_target_flag) %>%
        as.matrix() %>% 
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        dplyr::mutate(symbol = s) %>%
        dplyr::select(symbol, new_target_flag = rowname, everything(),
                      down = `-1`, stay = `0`, up = `1`)
    
    Z2[[i]] = ftable(y$exit1, y$new_target_flag) %>%
        as.matrix() %>% 
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        dplyr::mutate(symbol = s) %>%
        dplyr::select(symbol, new_target_flag = rowname, everything(),
                      down = `-1`, stay = `0`, up = `1`)
    
}

z1 = Z1 %>% dplyr::bind_rows() %>%
    dplyr::filter(new_target_flag == 1) %>%
    dplyr::mutate(percent_up = up / (down + stay + up),
                  percent_down = down / (down + stay + up),
                  type = "entry")
    
z2 = Z2 %>% dplyr::bind_rows() %>%
    dplyr::filter(new_target_flag == 0) %>%
    dplyr::mutate(percent_up = up / (down + stay + up),
                  percent_down = down / (down + stay + up),
                  type = "exit")

target_eval = union_all(z1, z2) %>%
    dplyr::arrange(symbol, type)

# z1b = z1 %>%
#     tidyr::gather(key, value, down:up) %>%
#     tidyr::spread(new_target_flag, value)

#write.table(target_eval, "clipboard-16384", sep = "\t", row.names = FALSE)












