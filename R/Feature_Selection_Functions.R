
#' @title Get_correlated
#' @description remove high correlated attributes (i.e., features).
#' Many methods perform better if highly correlated attributes are removed
#  Generally, you want to remove attributes with an absolute correlation of 0.75 or higher.
#' @param dataset is the dataset of the attributes with respect to a target 
#' @export
#' @return dataset with the highly Correlated features 

Get_correlated<- function(dataset){
  #Remove columns with standard deviation is zero 
  #df <- Filter(function(x) sd(x) != 0, dataset[,2:ncol(dataset)])
  n <- ncol(dataset)
  correlationMatrix <- cor(dataset[,-n])
  # summarize the correlation matrix
  #print(correlationMatrix)
  
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7,name=TRUE)
  highlyCorrelated
  
  
  

    return(highlyCorrelated)
  
}






#' @title RFE_FeatureSelection 
#' @description Feature Selection - Recursive Feature Elimination 
#' @param train_data is train set
#' @param test_data is test set
#' @param trg is target (i.e., outcome)
#' @param sd is the seed
#' @import caret
#' @export
#' @return list
RFE_FeatureSelection <- function(train_data, test_data, trg, sd=2023) {
  
  set.seed(sd)  
  
  # tryCatch({
  # Check if the target column exists in the dataset
  #   if (!(trg %in% colnames(train_data)) ||!(trg %in% colnames(test_data))) {
  #    stop("Target column not found in dataset")
  #   }
  
  # Features
  
  n <- ncol(train_data)
  
  
  
  x_train <- train_data[,-n]
  y_train <- train_data[,n]
  
  
  
  control <- rfeControl(functions = rfFuncs, 
                        method = "repeatedcv", 
                        repeats = 5,
                        number = 10)
  
  
  
  
  result_rfe <- rfe(x = x_train, 
                    y = as.factor(unlist(y_train)), 
                    # sizes = c(1:ncol(train_data)-1),
                    sizes = c(1:ncol(x_train)),
                    rfeControl = control)
  
  
  # Get the selected features
  # selected_features <- colnames(train_data[, !colnames(train_data) %in% 
  #                                            target_col])[result_rfe$optVariables]
  # 
  
  # selected_features <- predictors(result_rfe)
  # Return the dataset with the selected features
  
  return(result_rfe)
  
  
  # }, error = function(e) {
  # Log the error
  #   cat("Error:", e$message, "\n")
  #   return(NULL)
  #  })
}

