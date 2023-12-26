library(pROC)
library("dplyr")

# select the best model for the chosen target among those in the "TargetModelList" dataframe
#!Change with the best model you have found 
###################################################################

# set beforehand parameters

###################################################################

#------------------- paths ------------------------------------
path<-file.path(getwd())
pathToLoad <- paste0(path,"/data/Split_RC/balancedList.rda")  # balanced dataset
  
filenmModels <- "TargetmodelsToCompare_23_dic_2023.rda"
pathToLoadModels<-paste0(path,"/data/CompareModels/",filenmModels)  
  
filenmAUC <- "TargetmodelsAUCToCompare_23_dic_2023.rda"
pathToLoadAUC<-paste0(path,"/data/CompareModels/",filenmAUC)  
  
#------------------- variable and lists ------------------------------------
#REMOVED: "ipertension","reflux","sport", "etnicity"

targets <- c("smoking","computer","glasses","sex","nationality",
              "hemisphere","continent","reflux","sport","age")



#targets <- c("smoking","computer","glasses","sex","allergy",
#             "nationality","hemisphere","continent",
 #            "reflux","sport","etnicity","age","bmi")

#AGGIUNGERE AGE E BMI
#targets <- c("smoking","computer","glasses","sex","allergy",
#             "nationality","hemisphere","continent","age","bmi")




bestModelsList <- vector(mode = "list", length = length(targets))
names(bestModelsList) <-targets

#pathToLoad <- paste0(pathToLoad,trg,".rda")
load(pathToLoad)
message(paste("RC_dataset loaded from: ", pathToLoad)) 


RocPlotList <- vector(mode = "list", length = length(targets))
names(RocPlotList) <-targets

CMPlotList <- vector(mode = "list", length = length(targets))
names(CMPlotList) <-targets


VipsList <- vector(mode = "list", length = length(targets))
names(VipsList) <-targets


################################

# Load Datasets with Models and AUC

################################

load(pathToLoadModels)
message(paste("Loaded TargetmodelsToCompare from: ", pathToLoadModels)) 

load(pathToLoadAUC)
message(paste("Loaded TargetmodelsAUCToCompare from: ", pathToLoadAUC))   

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
 # RocPlotList[[i]] <- plot_bestModelROC(trg,bestModelsList, RC_DatasetsList)
  RocPlotList[[i]] <- plot_bestModelROC(trg,bestModelsList, balancedList)
}  

trg <- "sex"
RocPlotList[[trg]] <- plot_bestModelROC(trg,bestModelsList, balancedList)
#----------------------Confusion Matrix --------


for(i in 1:length(CMPlotList)) { 
  trg <- names(CMPlotList)[i]
  CMPlotList[[i]] <- plot_bestModelConfusionMatrix(trg,bestModelsList, balancedList)
  
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






