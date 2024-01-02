library(EyeMetGenomics)
library(dplyr)
library(caret)
library(openxlsx)
library(rsample)
#library(recipes)






#-------- LOAD all preprocessed datasets ------
setwd("/home/msettino/workspace/EyeMetGenomics/")
path<-file.path(getwd())


filenmToLoadPreprocessedSplit <- "PreprocessedSplit_02_Gen_23.RData"
pathToLoadPreprocessedSplit<-paste0(path,"/data/Preprocessed/",filenmToLoadPreprocessedSplit)  



filenmRFE <- "RFE_datasetList.rda"
pathToSaveFS <- paste0(path,"/data/Split_FS_Bal/",filenmRFE)

load(pathToLoadPreprocessedSplit)


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent",
             "bmi","ipertension","reflux","sport","etnicity","age")


RFE_DatasetsList <- vector(mode = "list", length(targets))
names(RFE_DatasetsList) <-targets



for(i in 1:length(targets)) {
  trg <- targets[i]
  
  cat(paste0("\t", "Feature selection of ", trg, "...\n"))
  obj <- paste0("tar.bact.",trg)
  data_in_scope <- get(obj) 
  
  
  #-----------------------STEP 3 - Get train and test ---------
  
  train_data <- data_in_scope$train_data
  test_data <- data_in_scope$test_data
  

#-----------------------STEP 2 -  FEATURE SELECTION - ----------------------


result_rfe<-RFE_FeatureSelection(train_data,test_data,trg)

# all the features selected by rfe
rfe_selected <- predictors(result_rfe)
  


train_data <- train_data[,c(rfe_selected,"Target")]
test_data <- test_data[,c(rfe_selected,"Target")]



#path_to_save_split <- paste0("~/MyDir/EyeMetGenomics/data/Split_FS_Bal/RC_",trg,".rda")
#save(train_data,test_data, file = path_to_save_split)

RFE_dataset <- list(train_data,test_data)
names(RFE_dataset) <-c("train_data","test_data")

RFE_DatasetsList[[i]] <- RFE_dataset
}



save(RFE_DatasetsList, file = pathToSaveFS)
message(paste("RFE_DatasetsList saved in: ", pathToSaveFS)) 






