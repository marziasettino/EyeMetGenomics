
#' #' @title Preprocessing_recipes
#' #' @description Pre-Processing the data with{recipes}
#' #' @param tar.bact.trg is the sub dataset filtered by trg
#' #' @param q is the formula (e.g.; sex ~.)
#' #' @param sd is the seed
#' #' @export
#' #' @return dataframe
#' preprocessing_recipes <- function(tar.bact.trg, q, sd=2020) {
#' 
#' set.seed(sd)
#' rec <- recipe(q, 
#'               data = tar.bact.trg) %>%   # Formula
#'   step_dummy(all_nominal(), -trg) %>%          # convert nominal data into one or more numeric.
#'   step_corr(all_predictors()) %>%                 # remove variables that have large absolute 
#'   # correlations with other variables.
#'   step_center(all_numeric(), -all_outcomes())%>%  # normalize numeric data to have a mean of zero.
#'   step_scale(all_numeric(), -all_outcomes()) %>%        # normalize numeric data to have a standard deviation of one.
#'   step_naomit(all_predictors()) 
#'   # %>%step_downsample(trg)                    # all classes should have the same frequency as the minority 
#' # class(not needed in our case)
#' 
#' return(rec)
#' }







#' @title performance
#' @description Performance and statistics
#' @param model_fit_default is the rand_forest function output
#' @param test_data is the test dataset
#' @param trg is the target (e.g.; sex)
#' @param sd is the seed
#' @export
#' @return dataframe

performance <- function(model_fit_default, test_data, trg, sd=2020) {

set.seed(sd)
test_results_default <- 
  test_data %>%
  select(trg) %>%
  as_tibble() %>%
  mutate(
    model_class_default = predict(model_fit_default, new_data = test_data) %>% 
      pull(.pred_class),
    model_prob_default  = predict(model_fit_default, new_data = test_data, type = "prob") %>% 
      pull(.pred_0)) #  pull(.pred_Yes))

return(test_results_default)
}







#' @title my_finalize_func
#' @description Function to finalize the recip and the model and returne the AUC value and the ROC curve of the tuned model.  
#' @param result_tuning is the tune_grid function output
#' @param my_recipe is the preprocessing function output
#' @param my_model is the finalize_model function output
#' @export
#' @return dataframe
# 
my_finalize_func <- function(result_tuning, my_recipe, my_model,q, data_in_scope_test, rec, trg) {
  
  # Accessing the tuning results
  bestParameters <- select_best(result_tuning, metric = "roc_auc", maximize = TRUE)
  
  # Finalize recipe
  final_rec <- 
    rec %>%
    finalize_recipe(bestParameters) %>%
    prep()
  
  # Attach the best HP combination to the model and fit the model to the complete training data(data_in_scope_train) 
  final_model <-
    my_model %>%
    finalize_model(bestParameters) %>%
    fit(q, data = juice(final_rec))
  
  # Prepare the finale trained data to use for performing model validation. 
  df_train_after_tuning <- as.data.frame(juice(final_rec)) 
  df_test_after_tuning <- as.data.frame(bake(final_rec, new_data = data_in_scope_test))
  
  # Predict on the testing data 
  set.seed(2020)
  results_ <- 
    df_test_after_tuning%>%
    select(trg) %>%
    as_tibble()%>%
    mutate(
      model_class = predict(final_model, new_data = df_test_after_tuning) %>% 
        pull(.pred_class),
      model_prob  = predict(final_model, new_data = df_test_after_tuning, type = "prob") %>% 
        pull(.pred_0)) #pull(.pred_Yes))
  # Compute the AUC  
  auc <-  results_%>% roc_auc(truth = trg, model_prob)
  # Compute the confusion matrix
  confusion_matrix <- conf_mat(results_, truth= trg, model_class)
  # Plot the ROC curve
  rocCurve <- roc_curve(results_, truth = trg, model_prob)%>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    # geom_path(colour = "darkgreen", size = 1.5) +
    geom_path(colour = "darkgreen", linewidth = 1.5) +
    geom_abline(lty = 3, size= 1, colour = "darkred") +
    coord_equal()+
    theme_light()
  
  new_list <- list(auc, confusion_matrix, rocCurve,final_model)  
  return(new_list)
}




#' @title nonzeroCoef
#' @description get the non zero coefficients
#' @param beta is matrix of coefficients
#' @param bystep  FALSE means which variables were ever nonzero/bystep = TRUE means which variables are nonzero for each step
#' @export
#' @return integer
#
nonzeroCoef = function (beta, bystep = FALSE) 
{
  
  nr=nrow(beta)
  if (nr == 1) {#degenerate case
    if (bystep) 
      apply(beta, 2, function(x) if (abs(x) > 0) 
        1
        else NULL)
    else {
      if (any(abs(beta) > 0)) 
        1
      else NULL
    }
  }
  else {
    beta=abs(beta)>0 # this is sparse
    which=seq(nr)
    ones=rep(1,ncol(beta))
    nz=as.vector((beta%*%ones)>0)
    which=which[nz]
    if (bystep) {
      if(length(which)>0){
        beta=as.matrix(beta[which,,drop=FALSE])
        nzel = function(x, which) if (any(x)) 
          which[x]
        else NULL
        which=apply(beta, 2, nzel, which)
        if(!is.list(which))which=data.frame(which)# apply can return a matrix!!
        which
      }
      else{
        dn=dimnames(beta)[[2]]
        which=vector("list",length(dn))
        names(which)=dn
        which
      }
      
    }
    else which
  }
}


#' @title extractGlmnetInfo
#' @description extract Glmnet Info
#' @param object is a List or Vector
#' @export
#' @return integer
extractGlmnetInfo <- function(object) {
  
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  data.frame(lambda.min=lambdaMin, error.min=object$cvm[whichMin],
             lambda.1se=lambda1se, error.1se=object$cvm[which1se])
}



