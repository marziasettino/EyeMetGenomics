library(EyeMetGenomics)




path<-file.path(getwd())
pathToLoad_UnbalancedData <- paste0(path,"/data/Split_RC/RC_datasetList.rda")  #not balanced dataset

pathToLoad_BalancedData <- paste0(path,"/data/Split_RC/BalancedList.rda")

load(pathToLoad_UnbalancedData)
message("Dataset loaded from",  pathToLoad_UnbalancedData)

load(pathToLoad_BalancedData)
message("Dataset loaded from",  pathToLoad_BalancedData)


obj1 <- paste0("RC_DatasetsList")
DF_unbalanced <- get(obj1)

obj2 <- paste0("balancedList")
DF_balanced <- get(obj2)
#######################################################################################
targets <- c("smoking","computer","glasses","sex","allergy",
             "nationality","hemisphere","continent",
             "bmi","ipertension","reflux","sport","etnicity","age")


split_size_list_unbalanced <- vector(mode = "list", length = length(targets))
names(split_size_list_unbalanced) <-targets


split_size_list_balanced <- vector(mode = "list", length = length(targets))
names(split_size_list_balanced) <-targets



#if getIMR(train_data)>=2 ==>Imbalanced

#######################################

# get Size Unbalanced Dataset

#######################################
for(i in 1:length(DF_unbalanced)) {
#unbalanced <- FALSE  
  
  
  trg <- names(DF_unbalanced)[i]  
  train_data <- DF_unbalanced[[i]][[1]] 
  test_data <- DF_unbalanced[[i]][[2]] 
  
  split_size_list_unbalanced[[trg]] <- get_split_size_tabs(train_data,test_data,trg) 
  
  
}  
  
#######################################

# get Size Balanced Dataset

#######################################

for(i in 1:length(DF_balanced)) {
  #unbalanced <- FALSE  
  
  
  trg <- names(DF_balanced)[i]  
  train_data <- DF_balanced[[i]][[1]] 
  test_data <- DF_balanced[[i]][[2]] 
  
  split_size_list_balanced[[trg]] <- get_split_size_tabs(train_data,test_data,trg) 
  
  
}  





path1 <- paste0("~/MyDir/EyeMetGenomics/data/Split_RC/split_size_list_unbalanced.rda")
path2 <- paste0("~/MyDir/EyeMetGenomics/data/Split_RC/split_size_list_balanced.rda")

save(split_size_list_unbalanced, file = path1)
message(paste("split_size_list_unbalanced saved in: ", path1)) 


save(split_size_list_balanced, file = path2)
message(paste("split_size_list_unbalanced saved in: ", path2)) 



###########################

# Visualize Tabs of size for each target (UNBALANCED SIZE)

###########################
trg <- "smoking"

Create_sizeTab(split_size_list_unbalanced,trg)%>%knitr::kable(
  caption = paste0("Table of Size - ",trg))




###########################

# Visualize Tabs of size for each target (BALANCED SIZE)

###########################

trg <- "smoking"

Create_sizeTab(split_size_list_balanced,trg)%>%knitr::kable(
  caption = paste0("Table of Size - ",trg))


############ RESULTS #######
#CONTINET ancora sbilanciati
#hemisphere ancora sbilanciati









