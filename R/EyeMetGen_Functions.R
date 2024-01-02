





#' @title get_target_df
#' @description get a dataset subset including only a single target
#' @param trg is the targets 
#' @export
#' @return dataframe

get_target_df<- function(df_tar.bact,trg){
  
  k <- which(colnames(tar.bact) == trg)
  df_trg <- as.data.frame(tar.bact[,k]) #exctract trg column
  names(df_trg)[1] =trg
  bacts <- tar.bact[,grep("^bacteria", colnames(tar.bact))] #select all bacteria columns
  df <- cbind(df_trg,bacts)
  
  df <- na.omit(df)
  
 
 
  return(df)
  
}




#' @title Create_targets
#' @description Create  targets dataframe
#' @param path is the path of the targets dataset
#' @import openxlsx
#' @export
#' @return dataframe

Create_targets<- function(path){
 # targets<-read.xlsx("/Users/marzia/MyDir/GenomicsData/data_raw/database_sani_complete2.xlsx")
  targets.xls<-read.xlsx(path)
  
  targets.xls<-targets.xls[,c("Codice.episodio.lab","Hight","Weight","Smoking", 
                              "Sport","Ipertensione","Diabete","Sex",
                              "Computer","Glasses","Age","NAZIONE",
                              "reflusso.Gastro.Esofageo","Etnia","Luogo.nascita.Emisfero",
                              "Continente","Allergia.si")]

  
  targets.names<-c("id","height","weight","smoking",
                   "sport","ipertension","diabete","sex","computer", 
                   "glasses","age","nationality","reflux","etnicity",
                   "hemisphere","continent","allergy")
  
  colnames(targets.xls)<- targets.names
  targets <- targets.xls
  targets <- reformat_smoking(targets)
  targets <- reformat_computer(targets)
  targets <- reformat_sport(targets)
  targets <- reformat_etnicity(targets)
  targets <- reformat_sex(targets)
  targets <- reformat_hemisphere(targets)
  targets <- reformat_continent(targets)
  targets <- reformat_allergy(targets)
  targets <- reformat_diabete(targets)
  targets <- reformat_nationality(targets)
  targets <- reformat_reflux(targets)
  targets <- reformat_glasses(targets)
  targets <- reformat_ipertension(targets)
  targets <- add_BMI(targets)
 # targets <- targets_to_factor(targets)
  
  return(targets)
}

add_BMI<- function(targets){
  targets$height <-  as.numeric(targets$height)
  targets$weight <-  as.numeric(targets$weight)
  
  df <- data.frame("bmi")
  names(df) <- c("bmi")
  
  for(i in 1:nrow(targets)) {  
    
    if(is.na(targets[i,2]) | is.na(targets[i,3])) {
      df[i,] <- NA
    }else  {
      df[i,] <- targets[i,3] / (targets[i,2]/100)^2
    }
  }
  df$bmi <- as.numeric(df$bmi)
  targets <- cbind(targets,df)   
  
  
  return(targets)
}






reformat_computer<- function(targets){
  targets$computer[targets$computer == "Y24-"]<-1 
  targets$computer[targets$computer == "Y24+"] <- 1
  targets$computer[targets$computer == "Y"] <- 1
  targets$computer[targets$computer == "24H-"] <- 1
  targets$computer[targets$computer == "N"] <- 0
  
  return(targets)
}


reformat_sport<- function(targets){
  targets$sport[targets$sport == "Y"]<-1 
  targets$sport[targets$sport == "S"]<-1 
  targets$sport[targets$sport == "N"]<-0 

  return(targets)
}


reformat_sex<- function(targets){
  targets$sex[targets$sex == "F"]<-1 
  targets$sex[targets$sex == "M"] <- 0
  return(targets)
}

reformat_diabete<- function(targets){
  targets$diabete[targets$diabete == "N"]<-0 
  targets$diabete[targets$diabete == "Y"] <- 1
  return(targets)
}


reformat_ipertension<- function(targets){
  targets$ipertension[targets$ipertension == "N"]<-0 
  targets$ipertension[targets$ipertension == "Y"] <- 1
  return(targets)
}
reformat_glasses<- function(targets){
  targets$glasses[targets$glasses == "N"]<-0 
  targets$glasses[targets$glasses == "Y"] <- 1
  return(targets)
}
reformat_nationality<- function(targets){
  targets$nationality[targets$nationality == "ES"]<-0 
  targets$nationality[targets$nationality == "ITA"] <- 1
  return(targets)
}

reformat_reflux<- function(targets){
  targets$reflux[targets$reflux == "N"]<-0 
  targets$reflux[targets$reflux == "Y"] <- 1
  return(targets)
}

reformat_smoking<- function(targets){
  targets$smoking[targets$smoking == "N"]<-0 
  targets$smoking[targets$smoking == "Y"] <- 1
  return(targets)
}



reformat_etnicity<- function(targets){
  targets$etnicity[targets$etnicity == "A"]<-1 
  targets$etnicity[targets$etnicity == "L"] <- 1
  targets$etnicity[targets$etnicity == "LAT"] <- 1
  targets$etnicity[targets$etnicity == "C"] <- 0
  
  return(targets)
}




reformat_hemisphere<- function(targets){
  targets$hemisphere[targets$hemisphere == "Australe"]<-1 
  targets$hemisphere[targets$hemisphere == "Boreale"] <- 0
  return(targets)
}

reformat_continent<- function(targets){
  targets$continent[targets$continent == "America centrale"]<-1 
  targets$continent[targets$continent == "America del nord"] <- 1
  targets$continent[targets$continent == "America del sud"] <- 1
  targets$continent[targets$continent == "Asia"] <- 1
  targets$continent[targets$continent == "Europa"] <- 0
  
  return(targets)
}







reformat_allergy<- function(targets){
  targets$allergy[targets$allergy == "Y"]<-1 
  targets$allergy[targets$allergy == "N"] <- 0

  
  return(targets)
}

targets_to_numeric<- function(targets){
  #targets$id<-as.integer(targets$id)
  targets$height<-as.integer(targets$height)
  targets$weight<-as.integer(targets$weight)
  targets$sex<-as.integer(targets$sex)
  targets$smoking<-as.integer(targets$smoking)
  targets$glasses<-as.integer(targets$glasses)
  targets$sport<-as.integer(targets$sport)
  targets$computer<-as.integer(targets$computer)
  targets$nationality<-as.integer(targets$nationality)
  targets$ipertension<-as.integer(targets$ipertension)
  targets$diabete<-as.integer(targets$diabete)
  targets$continent<-as.integer(targets$continent)
  targets$allergy<-as.integer(targets$allergy)
  targets$hemisphere<-as.integer(targets$hemisphere)
  targets$reflux<-as.integer(targets$reflux)
  targets$etnicity<-as.integer(targets$etnicity)
  return(targets)
}



#' @title Create_bacteria
#' @description Create bacteria dataframe
#' @param path is the path of the bacteria dataset
#' @import openxlsx
#' @import dplyr
#' @export
#' @return dataframe

Create_bacteria<- function(path){

  genus<-read.csv(path)
  
  #Create dataframe bacteria
  Taxonomy <- data.frame(colnames(genus))
  
  #Taxonomy <- Taxonomy %>%
  #  mutate(id =paste("bacteria", rownames(Taxonomy), sep = ""))
  
  Taxonomy <- mutate(Taxonomy,id =paste("bacteria", 
                                        rownames(Taxonomy), sep = ""))
  
  colnames(Taxonomy)<- c("bacteria","id")
  
  bacteria<-genus
  colnames(bacteria)<-Taxonomy$id
  
  
  
  #Get number of column of "pz"
  genus.nc<-which(colnames(genus)=="pz")
  
  #rename last column "id"
  colnames(bacteria)[genus.nc] = "id"
  
  
  #move position of id column to the first column
  #bacteria<-bacteria %>% relocate(id)
  bacteria<-relocate(bacteria,id)
  
  
  #Get number of column of "id" to text
 # bacteria.nc<-which(colnames(bacteria)=="id")
  
  return(bacteria)
}




bacteria_normal <- function(bacteria) {  
  #Test if some bacteria is ormally distribuited
  #perform shapiro-wilk test
  myList <- list()  
  for(i in 2:ncol(bacteria)) {
    bact<-as.numeric(unlist(bacteria[i]))
    sh<-shapiro.test(bact)
    if(sh$p.value>0.05){
      print(colnames(bacteria[i]))
      print(paste("BACTERIA", i, "is normally distribuited"))
      #bacteria.index[i]<-i
      myList <- c(myList, colnames(bacteria)[i])
    }
    else  print("No")
  }
  
}





#-----------------


#' @title save_plt
#' @description save roc curve
#' @param model_fitename name of model_fite 
#' @param dpi of the plot
#' @param plt ggplot output
#' @param height height of ggplot output
#' @param width width of ggplot output
#' @import ragg
#' @export


save_plt <- function(plt,path,dpi=300,height=45,width=45) {  
  if(is.null(path)){
    
    path<-file.path(getwd())
    filenm <- "ResultsPlt.png"
    path<-paste0(path,"/","ResultsPlot","/",filenm)
  }
  #else{path<-pt} 

 
  
plt <- plt +
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = 15),
    legend.text = ggplot2::element_text(size = 15),
    plot.title = ggplot2::element_text(size=15, face="bold"),
    axis.title=ggplot2::element_text(size=8,face="bold"),
    axis.text.x = ggplot2::element_text(hjust = 1,face="bold",size = 8),
   axis.text.y = ggplot2::element_text(hjust = 1,face="bold",size = 8))

ggplot2::ggsave(plt, 
       filename=path,
       device = ragg::agg_png, 
       res = 300,
       width = 4, 
       height = 3, 
       units = "in")



message(paste("Plot saved in: ", path))
}


#' @title nonzeroCoef
#' @description save roc curve
#' @param bystep bystep = FALSE means which variables were ever nonzero /bystep = TRUE means which variables are nonzero for each step
#' @param model_fit is a glmnet model (ridge,lasso,elastic model)
#' @export
#' @return A numeric vector with the Extracted Non-Zero Model Coefficients
nonzeroCoef = function (model_fit, bystep = FALSE) 
{
  ### bystep = FALSE means which variables were ever nonzero
  ### bystep = TRUE means which variables are nonzero for each step
  nr=nrow(model_fit)
  if (nr == 1) {#degenerate case
    if (bystep) 
      apply(model_fit, 2, function(x) if (abs(x) > 0) 
        1
        else NULL)
    else {
      if (any(abs(model_fit) > 0)) 
        1
      else NULL
    }
  }
  else {
    model_fit=abs(model_fit)>0 # this is sparse
    which=seq(nr)
    ones=rep(1,ncol(model_fit))
    nz=as.vector((model_fit%*%ones)>0)
    which=which[nz]
    if (bystep) {
      if(length(which)>0){
        model_fit=as.matrix(model_fit[which,,drop=FALSE])
        nzel = function(x, which) if (any(x)) 
          which[x]
        else NULL
        which=apply(model_fit, 2, nzel, which)
        if(!is.list(which))which=data.frame(which)# apply can return a matrix!!
        which
      }
      else{
        dn=dimnames(model_fit)[[2]]
        which=vector("list",length(dn))
        names(which)=dn
        which
      }
      
    }
    else which
  }
}




#' @title draw_confusion_matrix_old
#' @description draw confusion matrix plot
#' @param cm is a confusionMatrix object
#' @param  filenm is the name of file
#' @export

draw_confusion_matrix_old <- function(cm, pt=NULL) {
  
#  table <- data.frame(confusionMatrix(prediction, test_data$Target)$table)
  table <- data.frame(cm$table)  
  plotTable <- table %>%
    mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
    group_by(Reference) %>%
    mutate(prop = Freq/sum(Freq))
  
  # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
  plt <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, 
                                                fill = goodbad, alpha = prop)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(good = "green", bad = "red")) +
    #theme_bw() +
    theme(text = element_text(size = 20)) +  
    xlim(rev(levels(table$Reference)))+
    labs(title = "Confusion Matrix",
         subtitle = paste0("Accuracy=",round(cm$overall['Accuracy'],3)))
  
if(is.null(pt)){
  
  path<-file.path(getwd())
  filenm <- "ConfusionMatrix.jpg"
  path<-paste0(path,"/","ResultsPlot","/",filenm)
}
else{path<-pt} 
  
  
  ggsave( 
    plt,
    filename = path,
    #width = width,
    #height = height,
    width = 20,
    height = 10,
    #units = "in",
    dpi=300
  )
  
  message(paste("Plot saved in: ", path)) 
  
  
  
  return(plt)
  
  
}  



#' @title draw_confusion_matrix
#' @description draw confusion matrix plot
#' @param cm is a confusionMatrix object
#' @param trg is the target
#' @export
draw_confusion_matrix <- function(cm, trg) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(paste0("Confusion Matrix ",trg), cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
  

} 








#' @title preprocess_split_dataset
#' @description generate train and test for the target subset
#' @param data_in_scope is the subset with respect the target
#' @import rsample
#' @import recipes
#' @import dplyr
#' @export
#' @return list

preprocess_split_dataset <- function(data_in_scope, sd=2023) {
  trg <- colnames(data_in_scope)[1]
  colnames(data_in_scope)[1] = "Target"
  
  if(is.integer(data_in_scope$Target)){
    data_in_scope$Target <- as.factor(data_in_scope$Target)
    levels(data_in_scope$Target) <- c("yes", "no")
   
    }

  
  
  # Split data into train and test data 
  set.seed(sd)
  

  
  trainIndex <- createDataPartition(data_in_scope$Target, 
                                    p = .7, 
                                    list = FALSE, 
                                    times = 3)
  
  data_in_scope_train <- data_in_scope[ trainIndex,]
  data_in_scope_test  <- data_in_scope[-trainIndex,]
  
 
  print(paste0("Splitting ",trg,"..."))
 
  
  
  preProc <- preProcess(data_in_scope_train, 
                               method = c("center","scale","spatialSign",
                                          "nzv", "center", "scale", "knnImpute"))
  
  data_in_scope_train_transf <- predict(preProc, data_in_scope_train)
  data_in_scope_test_transf <- predict(preProc,data_in_scope_test)  
  
  
  myList <- list(data_in_scope_train_transf,data_in_scope_test_transf)
  names(myList) <-c("train_data","test_data")
 
    
  return(myList)
  
}  









#' @title test_roc
#' @description extract AUC from the caret model object
#' @param model is the train model
#' @param test_data are the test data dataframe
#' @export


test_roc <- function(model, data) {
  roc(data$Target,
      predict(model, data, type = "prob")[,0])
}  



#' @title doplots
#' @description draw ROC plot with Confidence Interval
#' @param rocobj is a roc object
#' @param calls is a list of plot
#' @export

doplots <- function(rocobj, calls) {
  for (call in calls) {
    eval(call)
  }
  invisible(rocobj)
 
}







#' @title preprocessing_recipes
#' @description Pre-Processing the data with{recipes}
#' @param data_in_scope_train is the train dataset
#' @param sd is the seed
#' @import themis
#' @export
#' @return dataframe
preprocessing_recipes <- function(data_in_scope_train, sd=2020) {
 # q <- Target~.
  
  set.seed(sd)
  rec <- recipe(Target~., 
                data = data_in_scope_train) %>%   # Formula
    step_dummy(all_nominal(), -Target) %>%          # convert nominal data into one or more numeric.
    # correlations with other variables.
    step_center(all_numeric(), -all_outcomes())%>%  # normalize numeric data to have a mean of zero.
    step_scale(all_numeric(), -all_outcomes()) %>%        # normalize numeric data to have a standard deviation of one.
    step_naomit(all_predictors()) %>%
    step_zv(all_predictors()) %>%                     #remove 0-variance predictors
    step_normalize(all_numeric_predictors()) %>%
    step_corr(all_numeric_predictors(),threshold = 0.85)                  # remove variables that have large absolute 
  # %>%step_downsample(Target)                    # all classes should have the same frequency as the minority 
  
  
  #IMBALANCE:you can use the function step_downsample() or step_upsample() 
  #to reduce the imbalance between majority and minority class.
  
  
  return(rec)
}




#' @title get_prediction
#' @description get Performance and statistics
#' @param model_fit is the model to test
#' @param test_data is the test dataset
#' @export
#' @return dataframe

get_prediction <- function(model_fit, test_data) {
 
  
  
  test_data_with_pred <- test_data%>%
    select(Target)%>%as_tibble()%>%
    mutate(model_class_ca = predict(model_fit, newdata = test_data),
           model_prob_ca = predict(model_fit, newdata = test_data, 
                                   type= "prob")$yes)
  
  
  return(test_data_with_pred)
}



#' @title my_finalize_func
#' @description Function to finalize the recipe and the model and return the AUC value and the ROC curve of the tuned model.  
#' @param result_tuning is the result of the tuning process (i.e.; tune_grid function output)
#' @param my_recipe is the preprocessing function output (i.e.;  recipe object)
#' @param my_model is the model to tune
#' @export
#' @return list

my_finalize_func <- function(result_tuning, my_recipe, my_model) {
  
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
    fit(Target ~ ., data = juice(final_rec))
  
  # Prepare the final trained data to use for performing model validation. 
  df_train_after_tuning <- as.data.frame(juice(final_rec)) 
  df_test_after_tuning <- as.data.frame(bake(final_rec, new_data = test_data))
  
  # Predict on the testing data 
  set.seed(2020)
  results_ <- 
    df_test_after_tuning%>%
    select(Target) %>%
    as_tibble()%>%
    mutate(
      model_class = predict(final_model, new_data = df_test_after_tuning) %>% 
        pull(.pred_class),
      model_prob  = predict(final_model, new_data = df_test_after_tuning, type = "prob") %>% 
        pull(.pred_yes))
  # Compute the AUC  
  auc <-  results_%>% roc_auc(truth = Target, model_prob)
  # Compute the confusion matrix
  confusion_matrix <- conf_mat(results_, truth= Target, model_class)
  # Plot the ROC curve
  rocCurve <- roc_curve(results_, truth = Target, model_prob)%>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(colour = "darkgreen", size = 1.5) +
    geom_abline(lty = 3, size= 1, colour = "darkred") +
    coord_equal()+
    theme_light()
  
  #new_list <- list(auc, confusion_matrix, rocCurve)  
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






#' @title selectBestModelForTarget
#' @description get the non zero coefficients
#' @param beta is matrix of coefficients
#' @param bystep  FALSE means which variables were ever nonzero/bystep = TRUE means which variables are nonzero for each step
#' @export
#' @return integer
#
selectBestModelForTarget = function (TargetmodelsToCompare,TargetmodelsAUCToCompare) {

  bestModelsAUCList <- vector(mode = "list", length = length(TargetmodelsToCompare))
  names(bestModelsAUCList) <- names(TargetmodelsToCompare)
  
  bestModelsList <- vector(mode = "list", length = length(TargetmodelsToCompare))
  names(bestModelsList) <- names(TargetmodelsToCompare)
  
    
    for(i in 1:length(TargetmodelsAUCToCompare)) {
      targetAUCModels <- TargetmodelsAUCToCompare[[i]]
      bestModelsAUCList[[i]] <- targetAUCModels[1,c(1,2)]
     # print(names(bestModelsAUCList)[i])
      
    }

}

#' @title get_bestModel
#' @description get the best model for a target froam a list of models
#' @param Listmodels is the list of models for the target
#' @param bestModelAUC is the list AUC for all the models 
#' @param trg is the target
#' @export
#' @return train object

get_bestModel<- function(TargetmodelsAUCToCompare, TargetmodelsToCompare, trg){
  bestModel <- NULL
  
  bestAUC.target <- TargetmodelsAUCToCompare[[trg]][1,c(1,2)]
  Listmodels.target <- TargetmodelsToCompare[[trg]]   
  
  
  bestAuc <- changenameBestAUC(bestAUC.target)
  for(i in 1: length(Listmodels.target)) {  
    if(Listmodels.target[[i]][["method"]]==bestAuc){
      bestModel <-  Listmodels.target[[i]]
      break
    }
  }
  return(bestModel)
}

 



#' @title changenameBestAUC
#' @description get the best model for a target froam a list of models
#' @param bestAUC.target is the name of the model to change
#' @export
#' @return  string
changenameBestAUC<- function(bestAUC.target){
  best <- bestAUC.target["Method"] 
  
  # result = switch(   
  #   best$Method,   
  #   "SVM Radial"= "svmRadial",   
  #   "XGBoost"= "xgbTree",   
  #   "Knn"= "knn",   
  #   "Random Forest"= "ranger", 
  #   "Naive Bayes"= "naive_bayes" 
  # 
  # ) 
  
 
  if(best == "SVM Radial"){
    best <- "svmRadial"
  }else if(best == "XGBoost"){
    best <-"xgbTree"
  }else if(best == "Knn"){
    best <-"knn"  
  }else if(best == "Random Forest"){
    best <-"ranger"  
  }else if(best == "Naive Bayes"){
    best <-"naive_bayes"    
    
  }#if
    
 
return(best)
 }





#' @title Get_taxonomy
#' @description Create bacteria dataframe
#' @param path is the path of the bacteria csv file
#' @import openxlsx
#' @import dplyr
#' @export
#' @return dataframe

Get_taxonomy<- function(path){
  genus<-read.csv(path)
  #Create dataframe bacteria
  Taxonomy <- data.frame(colnames(genus))
  
  #Taxonomy <- Taxonomy %>%
  #  mutate(id =paste("bacteria", rownames(Taxonomy), sep = ""))
  
  Taxonomy <- mutate(Taxonomy,id =paste("bacteria", 
                                        rownames(Taxonomy), sep = ""))
  
  colnames(Taxonomy)<- c("bacteria","id")
  
  return(Taxonomy)
  
}




#' @title get_split_size_tabs
#' @description get the split size list
#' @param train_data is the train data dataframe
#' @param test_data are the test data dataframe
#' @param trg is a single target
#' @export

get_split_size_tabs <- function(train_data,test_data,trg) {
  
  tb_train <- table(train_data$Target)
  tb_test <- table(test_data$Target)
  tb_train_prop <- round(100*prop.table(table(train_data$Target)), 0)
  IMR <- getIMR(train_data)
  list_size <- list(tb_train,tb_test,tb_train_prop,IMR)
  names(list_size) <- c("train_size","test_size","train_prop","IMR")

return(list_size)  
} 


#' @title get_split_size_tabs_All
#' @description get the split size list of all targets
#' @param pathToLoad path
#' @export

get_split_size_tabs_All <- function(pathToLoad="~/MyDir/EyeMetGenomics/data/Split_FS_Bal/RC_dataset.rda",
                                    targets) {
  split_size_list <- vector(mode = "list", length = length(targets))
  names(split_size_list) <-targets
  
  
  
    
  load(pathToLoad)
  message("Dataset loaded from",  pathToLoad)
  obj <- paste0("RC_DatasetsList")
  DF <- get(obj)  
  
  for(i in 1:length(DF)) {
    trg <- names(DF)[i]  
    train_data <- DF[[i]][[1]] 
    test_data <- DF[[i]][[2]] 
    
    split_size_list[[trg]] <- get_split_size_tabs(train_data,test_data,trg) 
  
  }   
  
  
  return(split_size_list)  
} 










#' @title get_IMB
#' @description get the split size list
#' @param df dataframe
#' @return integer
#' @export

getIMR <- function(df){ #N_majority/N_minority
  minCl <- names(which.min(table(df$Target)))
  IR <- sum(df$Target!=minCl)/sum(df$Target==minCl)
  #prop.table(table(train_data$Target)) 
  return(IR)
}




#' @title balancing
#' @description it makes balancing for a target
#' @param train_data is the train set
#' @param test_data is the test set
#' @param kappa is the k parameter of smote function
#' @param dupSZ is the dup_size parameter of smote function
#' @import smotefamily
#' @export
#' @return list

#balancing<- function(DF,targets,kappa=1,dupSZ=3){
balancing<- function(train_data,test_data,kappa=1,dupSZ=20, sd=2023){
  
  balanced_train <- vector(mode = "list", length = 3)
  names(balanced_train) <-c("train_data","test_data","IMR")
  set.seed(sd)
# print(paste0("kappa=",kappa," dupSZ=",dupSZ))
  
  # is_unbalanced_target <- vector(mode = "list", length = 5)
  # names(is_unbalanced_target) <-c("train_data","test_data","train_prop","test_prop","IMR")
  # 
 
    # train_data <- DF[[i]][[1]] 
    # test_data <- DF[[i]][[2]] 
    
imr <- getIMR(train_data)  
  
    if(imr>2){
      #unbalanced <- TRUE
    #  train_data <- ovun.sample(Target~., data = train_data, 
    #                            method = "over", seed=1)$data
      
      n <- length(train_data)
      smote_result = smotefamily::SMOTE(train_data[,-n],
                                        target = train_data[,n], 
                                        K = kappa, 
                                        dup_size = dupSZ)
      
    train_data_smote <- smote_result$data
    
    names(train_data_smote)[names(train_data_smote) == "class"] <- "Target"
    imr <- getIMR(train_data_smote)
    if(imr<=2){
      train_data <-train_data_smote
    }else{
      print(paste0("IMR=",imr," (kappa=",kappa," dupSZ=",dupSZ,").  Let's change Kappa and dupSZ!"))
    }  
 }      
      balanced_train[["train_data"]] <-train_data
      balanced_train[["test_data"]] <-test_data
      balanced_train[["IMR"]] <-imr
  
    

    
    # is_unbalanced_target[["train_data"]] <-train_data
    # is_unbalanced_target[["test_data"]] <-test_data
    # is_unbalanced_target[["train_prop"]] <-prop.table(table(train_data$Target)) 
    # is_unbalanced_target[["test_prop"]] <-prop.table(table(test_data$Target)) 
    # is_unbalanced_target[["IMR"]] <-getIMR(train_data)
    # 
    
   # is_unbalanced_list[[trg]] <- is_unbalanced_target    
    
    
 
  
  return(balanced_train)
  
}






#' @title Create_sizeTab
#' @description create overview of the size table for a specific target
#' @param split_size_list is the list of the size (unbalanced/balanced)
#' @param trg is the target
#' @export
#' @return list 

Create_sizeTab<- function(split_size_list,trg){
  
  train_prop <- as.data.frame(split_size_list[[trg]][["train_prop"]])
  colnames(train_prop) <- c("Class","%Prop")
  IMR <- split_size_list[[trg]][["IMR"]]
  
  train_prop_IMR <- cbind(train_prop,IMR)
  
  return(train_prop_IMR)
  
}













#' @title plot_bestModelROC
#' @description get the ROC curve 
#' @param trg is thetarget
#' @param bestModelsList is the list of the best model for each target 
#' @param balancedList is the list of train set (balanced) and test set for the target  
#' @export
#' @return roc object

#plot_bestModelROC<- function(trg,bestModelsList,RC_DatasetsList){
plot_bestModelROC<- function(trg,bestModelsList,balancedList){  
  
 
  
  best_model_final <- bestModelsList[[trg]]
  
  # train_data <- RC_DatasetsList[[trg]][[1]]
  # test_data <- RC_DatasetsList[[trg]][[2]]  
  
  train_data <- balancedList[[trg]][["train_data"]]
  test_data <- balancedList[[trg]][["test_data"]]  
  
  test_pred <- predict(best_model_final, 
                       test_data, type = "prob")[,1]#prob of yes
  
  
  
  
  
  
  # # 
  plot1 <- quote(plot(rocobj, 
                      main=paste0("AUC-ROC (",trg,")"), 
                      print.auc=TRUE))
  
  plot2 <- quote(plot(ci.se(rocobj, 
                            specificities=seq(0, 100, 5)), 
                      type="shape", col="#1c61b6AA"))
  plot3 <- quote(plot(ci(rocobj, of="thresholds", 
                         thresholds="best")))
  
  
  roc1 <- roc(test_data$Target, test_pred, percent = TRUE,ci=TRUE)

  
  dev.new(width=80, height=70, unit="in")
  
  plt <- EyeMetGenomics::doplots(roc1, list(plot1, plot2, plot3))
  
  
  return(plt)
  
}



#' @title plot_bestModelConfusionMatrix
#' @description plot the ConfusionMatrix
#' @param trg is thetarget
#' @param bestModelsList is the list of the best model for each target 
#' @param balancedList is the list of train (balanced) and test for target  
#' @export
#' @return confusionMatrix object

plot_bestModelConfusionMatrix<- function(trg,bestModelsList,balancedList){
  
  
  
  
  best_model_final <- bestModelsList[[trg]]
  
  train_data <- balancedList[[trg]][["train_data"]]
  test_data <- balancedList[[trg]][["test_data"]]  
  
  prediction <-predict(best_model_final, test_data)
  
  
  
  
  
  #dev.new(width=80, height=70, unit="in")
  
  cm_final <- caret::confusionMatrix(prediction, 
                                     test_data$Target, 
                                     positive = "yes")
  
  
  return(cm_final)
  
}



