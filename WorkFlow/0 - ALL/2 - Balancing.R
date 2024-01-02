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
#pathToLoad <- paste0(path,"/data/Split_FS_Bal/RC_datasetList.rda") 
pathToLoad <- paste0(path,"/data/Split_FS_Bal/RFE_datasetList.rda") 



load(pathToLoad)
message("Dataset loaded from",  pathToLoad)

filenmBalencedList <- "RFE_balancedList.rda"
pathToSavebalancedList_RFE<-paste0(path,"/data/Split_FS_Bal/",filenmBalencedList) 


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent","ipertension",
             "reflux","sport","etnicity","age","bmi")



#obj <- paste0("RC_DatasetsList")
obj <- paste0("RFE_DatasetsList")
DF <- get(obj)

##################################################################
#  Balancing
###################################################################
balancedList_RFE <- vector(mode = "list", length = length(targets))
names(balancedList_RFE) <-targets

for(i in 1:length(DF)) { 
 
  train_data <- DF[[i]][[1]] 
  test_data <- DF[[i]][[2]] 
  print(paste0("Target=",names(DF[i]),"IMR BEFORE=",getIMR(train_data)))  
  balancedList_RFE[[i]]<- balancing(train_data,test_data,1,20) 
  
  
}





save(balancedList_RFE, file = pathToSavebalancedList_RFE)
message(paste("balancedList_RFE saved in: ", pathToSavebalancedList_RFE))  


# #NOT Balanced
# # train_data_ipertension <- balancedList_RFE[["ipertension"]][["train_data"]]
# # test_data_ipertension <- balancedList_RFE[["ipertension"]][["test_data"]]
# # balancedList_RFE[["ipertension"]] <- balancing(train_data_ipertension,
# #                                            test_data_ipertension,1,30)
# # balancedList_RFE[["ipertension"]][["IMR"]]
# #32.9
# #21.4375
# #15.29167
# 

# train_data_allergy <- balancedList_RFE[["allergy"]][["train_data"]]
# test_data_allergy <- balancedList_RFE[["allergy"]][["test_data"]]
# balancedList_RFE[["allergy"]] <- balancing(train_data_allergy,
#                                       test_data_allergy,1,30)
# balancedList_RFE[["allergy"]][["IMR"]]


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
#   balancedList_RFE[["allergy"]]<- balancing(train_data,test_data,kappa,dupSZ) 
#   balancedList_RFE[["allergy"]][["IMR"]]
# }
# 
# 
# 
# 
# 
# 
# 
# 
# train_data_sport <- balancedList_RFE[["sport"]][["train_data"]]
# test_data_sport <- balancedList_RFE[["sport"]][["test_data"]]
# balancedList_RFE[["sport"]] <- balancing(train_data_sport,
#                                      test_data_sport,1,20)
# balancedList_RFE[["sport"]][["IMR"]]
# #1.285714
# 
# 



