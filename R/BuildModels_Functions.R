# --------------------------------------
# Random Forest
# --------------------------------------


#' @title random_forest_model
#' @description Create a caret model
#' @param train_data is train set
#' @param sd is the seed
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @import caret
#' @export
#' @return train object

random_forest_model<- function(train_data, ctrl, sd=2023){
  set.seed(sd)
  
  tuneGrid <- expand.grid(.mtry=c(1:ncol(train_data)-1),
                          .min.node.size=c(10, 20),
                          .splitrule = "gini")



  
  ranger_fit <- caret::train(Target ~ ., 
                                  data=train_data, 
                                  method="ranger", # Random Forest
                                  metric="ROC", 
                                  importance = "impurity",
                                  tuneGrid=tuneGrid, 
                                  trControl=ctrl)

return(ranger_fit)  
  
}




# --------------------------------------
# XGBoost
# --------------------------------------


#' @title xgtree_model
#' @description Create  targets dataframe
#' @param train_data is train set
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @param sd is the seed
#' @import caret
#' @export
#' @return train object

xgtree_model<- function(train_data, ctrl, sd=2023){
set.seed(sd)  
  
  

  tunegrid.xgtree=expand.grid(nrounds=1000,
                              eta=c(0.5, 0.2,  0.1, 0.07, 0.02),
                              max_depth=c(2, 3, 4, 5, 6, 7, 8, 9, 10),
                              gamma = 0.01,
                              subsample = 0.5,
                              min_child_weight = c(1,2, 3, 4, 5),
                              colsample_bytree = 1)

  
  

  
  xgtree_model <-   caret::train(Target ~ ., 
                                 data = train_data, 
                                 metric = "ROC",
                                 trControl=ctrl,
                                 tuneGrid = tunegrid.xgtree,
                                 method = "xgbTree")
  
  return(xgtree_model)  
  
}





# --------------------------------------
# k-Nearest Neighbors
# --------------------------------------


#' @title knn_model
#' @description Create  targets dataframe
#' @param train_data is train set
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @param sd is the seed
#' @import caret
#' @export
#' @return train object

knn_model<- function(train_data, ctrl, sd=2023){
set.seed(sd)

knnGrid <-  expand.grid(k = c(1:100))

  
  
knn_model <-  caret::train(Target ~ ., 
                           data = train_data, 
                           method = "knn",
                           trControl = ctrl,
                           tuneGrid = knnGrid,
                           metric = "ROC",
                           tuneLength = 7)
  
  return(knn_model)  
  
}




# --------------------------------------
# svmRadial
# --------------------------------------

#' @title svmRadial_model
#' @description Create  targets dataframe
#' @param train_data is train set
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @param sd is the seed
#' @import caret
#' @export
#' @return train object

svmRadial_model<- function(train_data, ctrl, sd=2023){
set.seed(sd) 
  
tuneGr = expand.grid(
    C = c(1e-1, 1e0, 1e1, 1e2, 1e3),
    sigma = c(.01, .1, 1.0)
  )




svmRadialFit <- caret::train(Target ~ ., 
                             data = train_data, 
                             method="svmRadial", 
                             metric="ROC", 
                             tuneGrid=tuneGr, 
                             trControl=ctrl)
  
  
  return(svmRadialFit)  
  
}


# --------------------------------------
# Naive Bayes
# --------------------------------------

#' @title naive_bayes_model
#' @description Create  targets dataframe
#' @param train_data is train set
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @param sd is the seed
#' @import caret
#' @export
#' @return train object

naive_bayes_model<- function(train_data, ctrl, sd=2023){
set.seed(sd)  
  
  Grid = data.frame(usekernel = TRUE,
                    laplace = 0,
                    adjust = 1)


 
NaiveBayesFit <-  caret::train(Target ~ ., 
                               data = train_data,
                               method = 'naive_bayes',
                               trControl = ctrl,
                               metric = "ROC",
                               tuneGrid = Grid)
  
  
  return(NaiveBayesFit)  
  
}


#' @title glmnet_model
#' @description Create  targets dataframe
#' @param train_data is train set
#' @param ctrl is la list (i.e. returned by TrainControl function)
#' @param sd is the seed
#' @import caret
#' @export
#' @return train object

glmnet_model<- function(train_data, ctrl, sd=2023){
  set.seed(sd)  
  
 myGrid <- expand.grid(alpha = 0:1,lambda = seq(0.0001, 1, length = 20))
  
  
  model_glmnet <- caret::train(Target ~ ., 
                               data = train_data, 
                               method = "glmnet",
                               metric = "ROC",
                               tuneGrid = myGrid, 
                               trControl = ctrl)
  
  
  return(model_glmnet)  
  
}


