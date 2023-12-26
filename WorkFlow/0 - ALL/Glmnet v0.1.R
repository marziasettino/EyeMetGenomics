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



TargetmodelsAUCToCompare <- vector(mode = "list", length = length(targets))
names(TargetmodelsAUCToCompare) <-targets



TargetmodelsToCompare <- vector(mode = "list", length = length(targets))
names(TargetmodelsToCompare) <-targets

#Let's choose the feature Selection method 
# featureSelection <- "RC"
# pathToSavePlots <- paste0("~/MyDir/EyeMetGenomics/ResultsPlot/",featureSelection,"_")
# pathToSaveModels <- paste0("~/MyDir/EyeMetGenomics/data/CompareModels/TargetmodelsToCompare_RC.rda")
# pathToSaveAUC <- paste0("~/MyDir/EyeMetGenomics/data/CompareModels/TargetmodelsAUCToCompare_RC.rda")
# pathToLoad <- paste0("~/MyDir/EyeMetGenomics/data/Split_RC/RC_dataset.rda")

path<-file.path(getwd())

pathToLoad <- paste0(path,"/data/Split_RC/RC_datasetList.rda")  

filenmModels <- "TargetmodelsToCompare_20_dic_2023.rda"
pathToSaveModels<-paste0(path,"/data/CompareModels/",filenmModels)  

filenmAUC <- "TargetmodelsAUCToCompare_20_dic_2023.rda"
pathToSaveAUC<-paste0(path,"/data/CompareModels/",filenmAUC)  

filenmBalencedList <- "BalancedList.rda"
pathToLoadBalancedList<-paste0(path,"/data/Split_RC/",filenmBalencedList)


load(pathToLoad)
message("Dataset loaded from",  pathToLoad)


obj <- paste0("RC_DatasetsList")
DF <- get(obj)



load(pathToLoadBalancedList)
message("balancedList loaded from",  pathToLoadBalancedList)


obj <- paste0("balancedList")
balancedList <- get(obj)

#####################################
#         Create objects
#####################################


#Clear the Timing log
tic.clearlog()


targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent","ipertension",
             "reflux","sport","etnicity","age","bmi")


targets <- c("smoking")





for(i in 1:length(balancedList)) { 
  
  trg <- names(balancedList)[i]  
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
# GLMNET
# --------------------------------------



tic("Time - Glmnet")
glmnet_fit <- glmnet_model(train_data, ctrl)
toc(log = TRUE, quiet = TRUE)



# Predict on the testing data
test_data_with_pred <- get_prediction(glmnet_fit,test_data)



# Compute the AUC
auc_Glmnet_Fit <- test_data_with_pred%>% 
  yardstick::roc_auc(truth=Target, model_prob_ca)

cat("GLMNET model via Grid Search method scores", 
    auc_Glmnet_Fit$.estimate, "AUC on the testing data")

}
