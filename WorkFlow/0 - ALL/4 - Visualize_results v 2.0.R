library(pROC)
library("dplyr")

# select the best model for the chosen target among those in the "TargetModelList" dataframe
#!Change with the best model you have found 
###################################################################

# set beforehand parameters

###################################################################

#------------------- paths ------------------------------------
path<-file.path(getwd())
pathToLoadBalanced <- paste0(path,"/data/Split_FS_Bal/RFE_balancedList.rda")  # balanced dataset
#pathToLoadBalanced <- paste0(path,"/data/Split_FS_Bal/RC_balancedList.rda") 

filenmModels <- "RFE_TargetmodelsToCompare_02_Gen_2024.rda"
pathToLoadBalancedModels<-paste0(path,"/data/CompareModels/",filenmModels)  
  
filenmAUC <- "RFE_TargetmodelsAUCToCompare_02_Gen_2024.rda"
pathToLoadBalancedAUC<-paste0(path,"/data/CompareModels/",filenmAUC)  
  
#------------------- variable and lists ------------------------------------


targets <- c("smoking","computer","glasses","sex",
             "nationality","hemisphere", "ipertension",
             "reflux", "sport","age","bmi")

targets <- c( "ipertension","reflux", "sport","age","bmi")



bestModelsList <- vector(mode = "list", length = length(targets))
names(bestModelsList) <-targets

#pathToLoadBalanced <- paste0(pathToLoadBalanced,trg,".rda")
load(pathToLoadBalanced)
message(paste("balancedList.rda loaded from: ", pathToLoadBalanced)) 


RocPlotList <- vector(mode = "list", length = length(targets))
names(RocPlotList) <-targets

CMPlotList <- vector(mode = "list", length = length(targets))
names(CMPlotList) <-targets


VipsList <- vector(mode = "list", length = length(targets))
names(VipsList) <-targets


################################

# Load Datasets with Models and AUC

################################

load(pathToLoadBalancedModels)
message(paste("Loaded TargetmodelsToCompare from: ", pathToLoadBalancedModels)) 

load(pathToLoadBalancedAUC)
message(paste("Loaded TargetmodelsAUCToCompare from: ", pathToLoadBalancedAUC))   

######################################################################## 

# Get The best Model for each target

#########################################################################




for(i in 1:length(bestModelsList)) { 
  trg <- names(bestModelsList)[i]
    print(paste0("Target=",trg))
    bestModelsList[[i]] <- EyeMetGenomics::get_bestModel(TargetmodelsAUCToCompare,
                                                         TargetmodelsToCompare, 
                                                         trg)
  
  
}  
  

####################################################################
# Draw ROC curves for best models (select trg)
####################################################################



# trg <- "smoking"
# trg <- "computer"
# trg <- "glasses"
# trg <- "sex"
# trg <- "allergy"
# trg <- "nationality"
# trg <- "hemisphere"
# trg <- "continent"






for(i in 1:length(RocPlotList)) { 
  trg <- names(RocPlotList)[i]
  print(paste0("Target=",trg))
 # RocPlotList[[i]] <- plot_bestModelROC(trg,bestModelsList, RC_DatasetsList)
  RocPlotList[[i]] <- plot_bestModelROC(trg,bestModelsList, balancedList_RFE)
}  

trg <- "age"
RocPlotList[[trg]] <- plot_bestModelROC(trg,bestModelsList, balancedList)
#----------------------Confusion Matrix --------


for(i in 1:length(CMPlotList)) { 
  trg <- names(CMPlotList)[i]
  CMPlotList[[i]] <- plot_bestModelConfusionMatrix(trg,bestModelsList, 
                                                   balancedList)
  
}  

plt <- plot_bestModelConfusionMatrix(trg,bestModelsList, balancedList)
dev.new(width=80, height=70, unit="in")
draw_confusion_matrix(plt,trg)

 dev.off()  
 graphics.off()

 #----------------------Draw Confusion Matrix --------
 
for(i in 1:length(CMPlotList)) { 
  trg <- names(CMPlotList)[i]
  dev.new(width=80, height=70, unit="in")
  draw_confusion_matrix(CMPlotList[[i]],trg)
  
}  



#------------ vip ------------------
 #computer svmRadial 
 #sex nayve bayes
 #continent Naive Bayes

 for (i in 1:length(VipsList)) {
   trg <- names(VipsList)[i]
 
   
   skip_to_next <- FALSE
   
   tryCatch(VipsList[[i]] <- varImp(bestModelsList[[i]]), 
            error = function(e) 
            { skip_to_next <- TRUE
              print(paste0("Error for target=",trg))
            
              }
            
            ) 
   
   if(skip_to_next) { next }     
 }
 
 
 
 #------------ Plot vip ------------------
 dev.off()  
 graphics.off()
 
 
 

dev.new(width=80, height=70, unit="in")#OK!
plot(VipsList[["smoking"]], main=paste0("VIP ", "smoking"), top=10)
   #plot(VipsList[["computer"]],main=paste0("VIP ", "computer"), top=10)

dev.new(width=80, height=70, unit="in")#OK!
plot(VipsList[["glasses"]], main=paste0("VIP ", "glasses"), top=5)

   #plot(VipsList[["sex"]],main=paste0("VIP ", "sex"), top=10)

dev.new(width=80, height=70, unit="in")#OK!
plot(VipsList[["allergy"]], main=paste0("VIP ", "allergy"), top=10)

dev.new(width=80, height=70, unit="in")#OK!
plot(VipsList[["nationality"]], main=paste0("VIP ", "nationality"), top=10)

dev.new(width=80, height=70, unit="in")#OK!
plot(VipsList[["hemisphere"]], main=paste0("VIP ", "hemisphere"), top=10)

#dev.new(width=80, height=70, unit="in")#OK!
#plot(VipsList[["continent"]], main=paste0("VIP ", "continent"), top=10)







############################## FINE ##################














#------------ Multi ROC BMI and SEX ------------------


roc <- multiclass.roc(test_data$Target, test_pred,
                      plot=TRUE)

roc$auc
#BMI=>Multi-class area under the curve: 0.7803
#AGE=>Multi-class area under the curve: 0.6232
#------------ vip ------------------


vip <- varImp(best_model_final)


plt_vip <- plot(vip, top=10)

dev.new(width=80, height=70, unit="in")#OK!
plt_vip

dev.copy(jpeg,filename=path)
#save_plt(plt_vip,path,height=45,width=45)






