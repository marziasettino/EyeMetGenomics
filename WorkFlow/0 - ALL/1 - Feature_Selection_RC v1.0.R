library(EyeMetGenomics)
library(dplyr)
library(caret)
library(openxlsx)
library(rsample)
#library(recipes)





#-------- LOAD all preprocessed datasets ------

path<-file.path(getwd())

filenmPrepocessed <- "preprocessedDataset.RData"
pathToLoadPreprocessed<-paste0(path,"/data/Preprocessed/",filenmPrepocessed)  

filenmFS <- "RC_datasetList.rda"
pathToSaveFS <- paste0(path,"/data/Split_RC/",filenmFS)

load(pathToLoadPreprocessed)


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent",
             "bmi","ipertension","reflux","sport","etnicity","age")


RC_DatasetsList <- vector(mode = "list", length(targets))
names(RC_DatasetsList) <-targets



for(i in 1:length(targets)) {
  trg <- targets[i]
 
  obj <- paste0("tar.bact.",trg)
  data_in_scope <- get(obj) 
  
  
  #-----------------------STEP 3 - Get train and test ---------
  
  train_data <- data_in_scope[[1]]
  test_data <- data_in_scope[[2]] 
  


#-----------------------STEP 2 -  FEATURE SELECTION - Remove CORRELATED feautures ----------------------
#Many methods perform better if highly correlated attributes are removed
#Generally, you want to remove attributes with an absolute correlation of 0.75 or higher.


highlyCorrelated <- Get_correlated(train_data)
train_data <- train_data[, !(colnames(train_data) %in% highlyCorrelated)]
test_data <- test_data[, !(colnames(test_data) %in% highlyCorrelated)]



#path_to_save_split <- paste0("~/MyDir/EyeMetGenomics/data/Split_RC/RC_",trg,".rda")
#save(train_data,test_data, file = path_to_save_split)

RC_dataset <- list(train_data,test_data)
names(RC_dataset) <-c("train_data","test_data")

RC_DatasetsList[[i]] <- RC_dataset
}



save(RC_DatasetsList, file = pathToSaveFS)
message(paste("RC_DatasetsList saved in: ", pathToSaveFS)) 






