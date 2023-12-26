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
library(RWeka)
library(smotefamily)


path<-file.path(getwd())
pathToLoad <- paste0(path,"/data/Split_RC/RC_datasetList.rda") 
load(pathToLoad)
message("Dataset loaded from",  pathToLoad)

filenmBalencedList <- "BalancedList.rda"
pathToSaveBalancedList<-paste0(path,"/data/Split_RC/",filenmBalencedList) 


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent","ipertension",
             "reflux","sport","etnicity","age","bmi")

targets <- c("bmi")

obj <- paste0("RC_DatasetsList")
DF <- get(obj)

##################################################################
#  Balancing
###################################################################
balancedList <- vector(mode = "list", length = length(targets))
names(balancedList) <-targets

for(i in 1:length(DF)) { 
  train_data <- DF[[i]][[1]] 
  test_data <- DF[[i]][[2]] 
  
  balancedList[[i]]<- balancing(train_data,test_data,1,20) 
  
  
}
balancedList <-  balancedList[-5]#no allergy,
balancedList <-  balancedList[-8]#no ipertension,
balancedList <-  balancedList[-8]#no ipertension,
balancedList <-  balancedList[-10]#no etnicity,
balancedList <-  balancedList[-11]#no bmi,


# 


# train_data_smoking <- balancedList[["smoking"]][["train_data"]]
# test_data_smoking <- balancedList[["smoking"]][["test_data"]]
# balancedList[["smoking"]] <- balancing(train_data_smoking,test_data_smoking,1,20) 
# balancedList[["smoking"]][["IMR"]]
# #1.909091
# 
# 
# train_data_hemisphere <- balancedList[["hemisphere"]][["train_data"]]
# test_data_hemisphere <- balancedList[["hemisphere"]][["test_data"]]
# balancedList[["hemisphere"]] <- balancing(train_data_hemisphere,
#                                           test_data_hemisphere,1,20) 
# balancedList[["hemisphere"]][["IMR"]]
# #1.952381
# 
# 
# train_data_continent <- balancedList[["continent"]][["train_data"]]
# test_data_continent <- balancedList[["continent"]][["test_data"]]
# balancedList[["continent"]] <- balancing(train_data_continent,
#                                          test_data_continent,1,20) 
# balancedList[["continent"]][["IMR"]]
# #1.285714
# 
# #NOT Balanced
# # train_data_ipertension <- balancedList[["ipertension"]][["train_data"]]
# # test_data_ipertension <- balancedList[["ipertension"]][["test_data"]]
# # balancedList[["ipertension"]] <- balancing(train_data_ipertension,
# #                                            test_data_ipertension,1,30)
# # balancedList[["ipertension"]][["IMR"]]
# #32.9
# #21.4375
# #15.29167
# 
# 
# train_data_reflux <- balancedList[["reflux"]][["train_data"]]
# test_data_reflux <- balancedList[["reflux"]][["test_data"]]
# balancedList[["reflux"]] <- balancing(train_data_reflux,
#                                       test_data_reflux,1,20) 
# balancedList[["reflux"]][["IMR"]]
# #1.909091
# 
# train_data_sport <- balancedList[["sport"]][["train_data"]]
# test_data_sport <- balancedList[["sport"]][["test_data"]]
# balancedList[["sport"]] <- balancing(train_data_sport,
#                                      test_data_sport,1,20) 
# balancedList[["sport"]][["IMR"]]
# #1.285714
# 
# 
# train_data_age <- balancedList[["age"]][["train_data"]]
# test_data_age <- balancedList[["age"]][["test_data"]]
# balancedList[["age"]] <- balancing(train_data_age,test_data_age,1,20) 
# balancedList[["age"]][["IMR"]]
# #1.518519
# 
# 
# # train_data_bmi <- balancedList[["bmi"]][["train_data"]]
# # test_data_bmi <- balancedList[["bmi"]][["test_data"]]
# # balancedList[["bmi"]] <- balancing(train_data_bmi,test_data_bmi,1,7) 
# # balancedList[["bmi"]][["IMR"]]
# #27.5
# 
# 





save(balancedList, file = pathToSaveBalancedList)
message(paste("BalancedList saved in: ", pathToSaveBalancedList))  