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



obj <- paste0("RC_DatasetsList")
DF <- get(obj)

##################################################################
#  Balancing
###################################################################
balancedList <- vector(mode = "list", length = length(targets))
names(balancedList) <-targets

for(i in 1:length(DF)) { 
  print(paste0("Target=",names(DF[i])))
  train_data <- DF[[i]][[1]] 
  test_data <- DF[[i]][[2]] 
  
  balancedList[[i]]<- balancing(train_data,test_data,1,20) 
  
  
}






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

# train_data_allergy <- balancedList[["allergy"]][["train_data"]]
# test_data_allergy <- balancedList[["allergy"]][["test_data"]]
# balancedList[["allergy"]] <- balancing(train_data_allergy,
#                                       test_data_allergy,1,30)
# balancedList[["allergy"]][["IMR"]]


# 
# 
# vec<- seq(1, 10, by=1)
# res1 <- gtools::permutations(n=4,r=2,v=vec,repeats.allowed=T) 
# 
# 
# train_data <- DF[["allergy"]][[1]] 
# prop.table(table(train_data$Target)) 
# 
# test_data <- DF[["allergy"]][[2]] 
# 
# kappa <- 3
# dupSZ <-5
# 
# for(i in 1:length(res1)) { 
#   perm <- res1[i,]
#   kappa <- perm[1]
#   dupSZ <- perm[2]
#   print(paste0("kappa=",kappa," dupSZ=",dupSZ))
#   balancedList[["allergy"]]<- balancing(train_data,test_data,kappa,dupSZ) 
#   balancedList[["allergy"]][["IMR"]]
# }
# 
# 
# 
# 
# 
# 
# 
# 
# train_data_sport <- balancedList[["sport"]][["train_data"]]
# test_data_sport <- balancedList[["sport"]][["test_data"]]
# balancedList[["sport"]] <- balancing(train_data_sport,
#                                      test_data_sport,1,20)
# balancedList[["sport"]][["IMR"]]
# #1.285714
# 
# 





save(balancedList, file = pathToSaveBalancedList)
message(paste("BalancedList saved in: ", pathToSaveBalancedList))  