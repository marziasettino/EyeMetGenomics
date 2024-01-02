library(EyeMetGenomics)
library(dplyr)
library(caret)
library(openxlsx)
library(rsample)
#library(recipes)




#-------- LOAD all preprocessed datasets ------

data_to_load_path_ALL <- paste0("~/MyDir/EyeMetGenomics/data/Preprocessed/preprocessedDataset.RData")

load(data_to_load_path_ALL)

########## OR Select the SINGLE dataset ############
# trg <- "smoking"
# 
# data_to_load_path <- paste0("~/MyDir/EyeMetGenomics/data/tar.bact.",trg,".rda")
# 
# obj <- load(data_to_load_path)
# 
# data_in_scope <- get(obj)


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent",
             "ipertension","sport","etnicity")



#removed "diabete","reflux","bmi","age" 



RFE_DatasetsList <- vector(mode = "list", length = 11)
names(RFE_DatasetsList) <-targets


target_col <- "Target"



for(i in 1:length(targets)) {
  trg <- targets[i]
  print(paste0("Target=",trg))
  obj <- paste0("tar.bact.",trg)
  data_in_scope <- get(obj) 
  
  
  #-----------------------STEP 3 - Get train and test ---------
  
  train_data <- data_in_scope[[1]]
  test_data <- data_in_scope[[2]] 
  
  

#-----------------------STEP 2 -  FEATURE SELECTION - Recursive Feature elimination ---------

  
  
  
  for (i in 1:length(targets)) {
    
    skip_to_next <- FALSE
    
    
    
    tryCatch(result_rfe<- FS_Recursive_Elimination(train_data,test_data,target_col), 
             error = function(e) { 
               skip_to_next <- TRUE
               Print(paste0("*******Error=",trg))
               })
    
    if(skip_to_next) { 
       next 
      print(paste0("*****i=",i))
      }     
  } 
  
  
  




selected_features <- predictors(result_rfe)
print(selected_features)
train_data <- data.frame(train_data[, c(selected_features, target_col)])
test_data <-data.frame(test_data[, c(selected_features, target_col)])


RFE_dataset <- list(train_data,test_data)
RFE_DatasetsList[[i]] <- RFE_dataset
 print(paste0("Saved ", trg))

}

path <- paste0("~/MyDir/EyeMetGenomics/data/Split_RFE/RFE_dataset_29_Novembre.rda")

save(RFE_DatasetsList, file = path)


#----- VISUALIZE RESULTS OF RFE -------------

# Print the selected features
selected_features <- predictors(result_rfe)
print(selected_features)

# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()



# Next, we will visually examine variable importance for the selected features and 
# see which features are more important for predicting the target variable.

n <- ncol(selected_features)


varimp_data <- data.frame(feature = row.names(varImp(result_rfe)[1:n], 
                                              importance = varImp(result_rfe)[1:n, 1]))

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")















