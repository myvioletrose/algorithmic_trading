# creation of df4, df4_cm

################ ML ######################################
###################################################################################
# train_ctrl <- caret::trainControl(method = "cv", number = 10, savePredictions = "final")
# model_rf <- caret::train(target_flag ~.,
#                          data = train %>% dplyr::select(-symbol, -date),
#                          method = 'rf',
#                          metric = 'Accuracy',
#                          tuneLength = 5, 
#                          trControl = train_ctrl)
# 
# model_nb <- caret::train(target_flag ~ .,
#                      data = train %>% dplyr::select(-symbol, -date),
#                      method = 'nb',
#                      metric = 'Accuracy',
#                      trControl = train_ctrl)

########################
# randomForest::randomForest is extremely fast, i.e., it took 262 seconds to train and test for 459 stocks!!!
# continue from df3, i.e., stock.strategy_final_project_df3
list_of_symbols = unique(df3$symbol)
temp_cm_list <- vector(mode = "list", length = length(list_of_symbols))
temp_result_list <- vector(mode = "list", length = length(list_of_symbols))

tic()

for(i in 1:length(list_of_symbols)){
    
    s = list_of_symbols[i]
    
    mlDf <- df3 %>% 
        dplyr::filter(symbol == s) %>%
        dplyr::select(contains("_flag"), symbol, date) %>%
        dplyr::mutate_if(is.numeric, as.factor) 
    
    train = mlDf %>% dplyr::filter(date < '2018-01-01')
    test = mlDf %>% dplyr::filter(date >= '2018-01-01')
    
    set.seed(1234)
    
    # model fit
    modfit.rf <- randomForest::randomForest(target_flag ~. , data = train %>% dplyr::select(-symbol, -date))
    
    # make prediction
    class <- predict(modfit.rf, test, type = "class")
    prob <- predict(modfit.rf, test, type = "prob")
    
    # confusionMatrix
    cm <- caret::confusionMatrix(class, test$target_flag)    
    cmDf <- broom::tidy(cm) %>% dplyr::mutate(symbol = s)
    temp_cm_list[[i]] <- cmDf
    
    # test - output
    result <- cbind(test, prob, class) %>%
        dplyr::select(symbol, date, 
                      target_flag, market_sig_flag, 
                      down_prob = `-1`, 
                      stay_prob = `0`, 
                      up_prob = `1`, 
                      signal = class)
    
    temp_result_list[[i]] <- result
    
    # print status
    print(paste0(i, ". ", list_of_symbols[i], " fimished at ", Sys.time()))
    
}

toc()

################################################
version = Sys.time() %>% as.character()

cm_result <- temp_cm_list %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(method = 'random forest',
                  version = version)

dim(cm_result)
# [1] 16524     9

df4 <- temp_result_list %>%
    dplyr::bind_rows() %>%
    dplyr::inner_join(., df3 %>% dplyr::select(symbol, date, adjusted, nlcor_n30), by = c("symbol", "date")) %>%
    dplyr::inner_join(., cm_result %>% 
                          dplyr::filter(term %in% c("accuracy", "kappa")) %>% 
                          dplyr::select(symbol, term, estimate, method, version) %>%
                          tidyr::spread(term, estimate), 
                      by = "symbol") %>%
    # convert factor back to its original value
    dplyr::mutate(market_sig_flag = as.numeric(levels(market_sig_flag))[market_sig_flag],
                  target_flag = as.numeric(levels(target_flag))[target_flag],
                  signal = as.numeric(levels(signal))[signal],
                  ) %>%
    dplyr::mutate(year = lubridate::year(date),
                  signal_v2 = dplyr::case_when(signal == 0 &
                                                   nlcor_n30 > .5 &
                                                   market_sig_flag == -1 ~ -1,
                                               TRUE ~ signal)
                  ) %>%
    dplyr::select(symbol, year, date, adjusted, nlcor_n30,
                  market_sig_flag, target_flag, 
                  signal_v1 = signal, signal_v2,
                  down_prob, stay_prob, up_prob,
                  accuracy, kappa,
                  method, version) %>%
    arrange(symbol, date)

str(df4)
row.names(df4) <- NULL
dim(df4)
# [1] 490635     16

head(df4)

############################################
#################################################################################
######### postgresql
db <- "algo_trade"
schema <- "stock"
host_db <- "localhost"
db_port <- "8321"
db_user <- "postgres"
db_password <- "Wtf12345"
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

###### write table
# Confusion Matrix
tbl_cm_name = "strategy_final_project_df4_cm"
tbl_cm = paste0(schema, ".", tbl_cm_name)
tbl_cm

# Machine Learning model result - as df4
tbl_ml_name = "strategy_final_project_df4"
tbl_ml = paste0(schema, ".", tbl_ml_name)
tbl_ml

tic()
DBI::dbWriteTable(con, DBI::SQL(tbl_cm), cm_result, overwrite = TRUE)
DBI::dbWriteTable(con, DBI::SQL(tbl_ml), df4, overwrite = TRUE)
toc()

dbGetQuery(con, glue::glue("select count(1) from {tbl_cm}"))
dbGetQuery(con, glue::glue("select count(1) from {tbl_ml}"))

# disconnect db
dbDisconnect(con) 












