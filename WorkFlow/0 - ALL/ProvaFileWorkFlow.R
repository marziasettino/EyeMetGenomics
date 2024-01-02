library(tidymodels)     # packages for modeling and statistical analysis
library(tune)           # For hyperparemeter tuning
library(tictoc)         # for timimg
library(EyeMetGenomics)
#library(DMwR)           #balancing
library(vip)
library(caret)
library(pROC)
library(randomForest)
#library(ROSE)  #balancing**
library(gridExtra)
library(workflows)    # streamline process
library(ggplot2)
library(MLeval)
library(naivebayes)
#library(RWeka)
library(smotefamily)




#####################################
#         Create objects
#####################################


#Clear the Timing log
tic.clearlog()

#"continent", #no continent for rfe
targets <- c("smoking","computer","glasses","sex",
             "nationality","hemisphere", "ipertension",
             "reflux", "sport","age","bmi")


TargetmodelsAUCToCompare <- vector(mode = "list", length = length(targets))
names(TargetmodelsAUCToCompare) <-targets



TargetmodelsToCompare <- vector(mode = "list", length = length(targets))
names(TargetmodelsToCompare) <-targets



######.   setwd("../..")

path<-file.path(getwd())



filenmModels <- "RFE_TargetmodelsToCompare_02_Gen_2024.rda"
pathToSaveModels<-paste0(path,"/data/CompareModels/",filenmModels)  

filenmAUC <- "RFE_TargetmodelsAUCToCompare_02_Gen_2024.rda"
pathToSaveAUC<-paste0(path,"/data/CompareModels/",filenmAUC)  

#filenmBalencedList <- "balancedList.rda"
filenmBalencedList <- "balancedList_RFE_31_dic.rda"
pathToLoadbalancedList<-paste0(path,"/data/Split_RC/",filenmBalencedList) 


# load(pathToLoad)
# message("Dataset loaded from",  pathToLoad)
# 
# obj <- paste0("RC_DatasetsList")
# DF <- get(obj)


load(pathToLoadbalancedList)
message("balancedList loaded from",  pathToLoadbalancedList)
 
 
#obj <- paste0("balancedList")
obj <- paste0("balancedList_RFE")
balancedList <- get(obj)
 
 
 
##################################################################
 #  Balancing
###################################################################
# balancedList <- vector(mode = "list", length = length(targets))
# names(balancedList) <-targets
#  
# for(i in 1:length(DF)) { 
#   train_data <- DF[[i]][[1]] 
#   test_data <- DF[[i]][[2]] 
#  
# balancedList[[i]]<- balancing(train_data,1,3) 
#  
# 
# }
# 
# save(balancedList, file = pathToSavebalancedList)
# message(paste("balancedList saved in: ", pathToSavebalancedList))  


 
################# BALENCED IN OTHER FILE ######################################################################



 
#for(i in 1:length(balancedList)) { 
for(i in 1:length(targets)) { 
  
 # trg <- names(balancedList)[i]  
  trg <- targets[i]  
  train_data <-balancedList[[i]][["train_data"]]
  test_data <- balancedList[[i]][["test_data"]]


##################################################################
#  BUILD THE MODELS
###################################################################
 
  
    
    ctrl <- trainControl(method = "repeatedcv",
                         number = 10,             ## 10-fold CV
                         repeats = 5,
                         #summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         savePredictions=TRUE,
                         allowParallel = TRUE)
  
    
    
    




# --------------------------------------
# Random Forest (Ranger)
# --------------------------------------
  print(paste0("Random Forest running for Target=",trg,"\n"))
tic("Time - Random Forest") 
  rf_fit <- random_forest_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)



# print the trained model
rf_fit
plot(rf_fit)


test_data_with_pred <- get_prediction(rf_fit,test_data)

# Compute the AUC
auc_ranger_fit_grid <- test_data_with_pred%>% 
  yardstick::roc_auc(truth=Target, model_prob_ca)

cat("Random Forest model via Grid Search method scores", 
    auc_ranger_fit_grid$.estimate, "AUC on the testing data")


# --------------------------------------
# XGBoost
# --------------------------------------
print(paste0("XGBoost running for Target=",trg,"\n"))

tic("Time - XGBoost")
 xgtree_fit <- xgtree_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)

# print the trained model
xgtree_fit
plot(xgtree_fit)

test_data_with_pred <- get_prediction(xgtree_fit,test_data)



# Compute the AUC
auc_xgtree_model <- test_data_with_pred%>% yardstick::roc_auc(truth=Target, 
                                                              model_prob_ca)
cat("XGBoost model via Grid Search method scores", auc_xgtree_model$.estimate, 
    "AUC on the testing data")


# --------------------------------------
# k-Nearest Neighbors
# --------------------------------------
print(paste0("KNN running for Target=",trg,"\n"))

tic("Time - KNN")
 knn_fit <- knn_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)

# print the trained model
knn_fit
plot(knn_fit)

# Predict on the testing data
test_data_with_pred <- get_prediction(knn_fit,test_data)


# Compute the AUC
auc_knn_model <- test_data_with_pred%>% 
  yardstick::roc_auc(truth=Target, model_prob_ca)

cat("k-Nearest Neighbors model via Grid Search method scores", 
    auc_knn_model$.estimate, "AUC on the testing data")


# --------------------------------------
# svmRadial
# --------------------------------------
print(paste0("svmRadial running for Target=",trg,"\n"))

tic("Time - svmRadial")
 svmRadial_fit <- svmRadial_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)

# Predict on the testing data
test_data_with_pred <- get_prediction(svmRadial_fit,test_data)


# Compute the AUC value
auc_svmRadial_fit <- test_data_with_pred%>% 
  yardstick::roc_auc(truth=Target, model_prob_ca)

cat("svmRadial model via  Adaptive Resampling Method scores", 
    auc_svmRadial_fit$.estimate , " AUC on the testing data")
## 


# --------------------------------------
# Naive Bayes
# --------------------------------------
# print(paste0("Naive Bayes running for Target=",trg))
# 
# tic("Time - Naive_bayes")
#  naive_bayes_fit <- naive_bayes_model(train_data, ctrl)
# toc(log = TRUE, quiet = TRUE)
# 
# # Predict on the testing data
# test_data_with_pred <- get_prediction(naive_bayes_fit,test_data)
# 
# 
# 
# # Compute the AUC
# auc_NaiveBayesFit <- test_data_with_pred%>% 
#   yardstick::roc_auc(truth=Target, model_prob_ca)
# 
# cat("Naive Bayes model via Grid Search method scores", 
#     auc_NaiveBayesFit$.estimate, "AUC on the testing data")



# --------------------------------------
# GLMNET
# --------------------------------------
print(paste0("GLMNET running for Target=",trg,"\n"))

skip_to_next <- FALSE
tic("Time - Glmnet")
# tryCatch(glmnet_fit <- glmnet_model(train_data, ctrl),
#          error = function(e) 
#          { skip_to_next <- TRUE
#          print(paste0("Error for target=",trg))
#          
#          }
#          
# ) 
# 
# if(skip_to_next) { next } 
glmnet_fit <- glmnet_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)



# Predict on the testing data
test_data_with_pred <- get_prediction(glmnet_fit,test_data)



# Compute the AUC
auc_Glmnet_Fit <- test_data_with_pred%>% 
  yardstick::roc_auc(truth=Target, model_prob_ca)

cat("GLMNET model via Grid Search method scores", 
    auc_Glmnet_Fit$.estimate, "AUC on the testing data")


# --------------------------------------
# Extract the elapsed time 
# --------------------------------------
#Put the result in a log
log.txt <- tic.log(format = TRUE)

#Extract only the value
res <- gsub( " sec elapsed", "", unlist(log.txt))
res









# --------------------------------------
# COMPARE MODELS 
# --------------------------------------




compare_models <- tibble(Method = c(  "ranger",
                                      "xgbTree",
                                      "knn", 
                                      "svmRadial",
                                    #  "naive_bayes",
                                      "glmnet"
                                    ), 
                         AUC_value = c(auc_ranger_fit_grid$.estimate, 
                                       auc_xgtree_model$.estimate,
                                       auc_knn_model$.estimate,
                                       auc_svmRadial_fit$.estimate,
                                     #  auc_NaiveBayesFit$.estimate,
                                       auc_Glmnet_Fit$.estimate
                                       
                         ))

                                       



compare_models <- compare_models[order(-compare_models$AUC_value),]


compare_models%>%knitr::kable(
  caption = "AUC Values and the best hyperparameter combination")


TargetmodelsAUCToCompare[[i]] <- compare_models

modelsForTarget <- list(rf_fit,
                        xgtree_fit,
                        knn_fit,
                        svmRadial_fit,
                      #  naive_bayes_fit,
                       glmnet_fit)

TargetmodelsToCompare[[i]] <- modelsForTarget

} #end for


save(TargetmodelsToCompare, file = pathToSaveModels)
message(paste("TargetmodelsToCompare saved in: ", pathToSaveModels)) 

save(TargetmodelsAUCToCompare, file = pathToSaveAUC)
message(paste("TargetmodelsAUCToCompare saved in: ", pathToSaveAUC)) 

#########################################################

#                       Extract Time

#########################################################

# log.txt <- tic.log(format = TRUE)
# log.lst <- tic.log(format = FALSE)
# tic.clearlog()
# timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
# mean(timings)
# writeLines(unlist(log.txt))
# 
# library(purrr)
#   
# log.st %>% map_df(~as_data_frame(.x)) %>% mutate(elapsed=toc-tic) 
# 
#  library(tidyverse)
# log.lst %>% map_df(~as.data.frame(.x)) %>% mutate(elapsed=toc-tic)

