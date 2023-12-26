
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




#' @title FS_Recursive_Elimination 
#' @description Feature Selection - Recursive Feature Elimination 
#' @param train_data is train set
#' @param test_data is test set
#' @param target_col is target (i.e., outcome)
#' @import caret
#' @export
#' @return list
FS_Recursive_Elimination <- function(train_data, test_data, target_col) {
  # """
  # This function performs feature selection on a given dataset using the caret package.
  # 
  # Parameters:
  # data (data.frame): The dataset to perform feature selection on
  # target_col (character): The name of the target column in the dataset
  # 
  # Returns:
  # data.frame: The dataset with the selected features
  # """
  # 
  
  
  tryCatch({
    # Check if the target column exists in the dataset
    if (!(target_col %in% colnames(train_data)) ||!(target_col %in% colnames(test_data))) {
      stop("Target column not found in dataset")
    }
    
    # Features
    
    n <- ncol(train_data)
    
    #y_train <- train_data$Target
    #y_train <- as.data.frame(train_data[,n])
    #x_train <- as.matrix(train_data[,1:n-1])
    
    y_train <- train_data[,n]
    x_train <- train_data[,-n]
    
    
    #y_test <- test_data$Target
    #y_test <- test_data[,n]
    #x_test <- as.matrix(test_data[,1:n-1])
    
    y_test <- test_data[,n]
    x_test <- test_data[,-n]
    
    # Perform feature selection using the caret package
    control <- rfeControl(functions = rfFuncs, 
                          method = "cv", 
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
    
    
  }, error = function(e) {
    # Log the error
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}



#' @title FS_Wilcox
#' @description wilcox test
#' @param tar.bact.target is the full dataset (target+bacteria)
#' @param trg is the outcome
#' @import stats
#' @export
#' @return vector
FS_Wilcox <- function(tar.bact.target, trg) {  
  #https://topepo.github.io/caret/feature-selection-using-univariate-filters.html
  # for classification models, that predictors 
  # can be filtered by conducting some sort of k-sample test 
  # (where k is the number of classes) to see if the mean of the predictor is different between the classes. Wilcoxon tests, t-tests and ANOVA models are sometimes used. Predictors that have statistically significant differences between 
  # the classes are then used for modeling.
  
  myList<-list()
  
  
  for(i in 2:ncol(tar.bact.target)) {  #get col names of index i colnames(df)[1]
    
    print(paste("i=", i))
    print(paste("colname=", colnames(tar.bact.target)[i]))
    bact <- tar.bact.target[,i]
    test <- wilcox.test(bact ~ tar.bact.target[,1])
    
    
    if(test$p.value<=0.05){ #if the p-value is <=0.05 at 
      #the 5% significance level, 
      #we reject the null hypothesis and we conclude that 
      #bacteriaXX is significantly different between the groups (e.g.; sex) (1/0).
      myList <- c(myList, colnames(tar.bact.target)[i])
      
    }   
    
  }
  
  if(!trg %in% myList){
    myList <- append(myList,trg)
    
  }
  
  wilcox.list <-  unlist(myList)
  
  # return(df)
  return(wilcox.list)
}
