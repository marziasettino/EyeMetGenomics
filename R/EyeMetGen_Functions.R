remove_zerocolumns <- function(tar.bact) {    
  myList<-list()
  for(i in 13:ncol(tar.bact)) {
    
    if(sum(tar.bact[,i])==0){ 
      # print(paste("#Column=", i)) #17,18,74
      # print(paste("#Column Name=", names(tar.bact)[i])) #bacteria6
      myList <- c(myList, i)
      
    }
  } 
  
  toremove <- unlist(myList)
  tar.bact <- tar.bact[-toremove]  
  return(tar.bact)
}





#' @title merge_preprocessing
#' @description Merge bacteria and targets dataframes
#' @param targets is the targets dataset
#' @param bacteria is the bacteria dataset
#' @import openxlsx
#' @export
#' @return dataframe

merge_preprocessing<- function(targets,bacteria){
  
  targets <- reformat_computer(targets)
  targets <- reformat_sport(targets)
  targets <- reformat_etnicity(targets)
  # targets <- targets_to_factor(targets)
  
  
  tar.bact<-merge(targets,bacteria, by = "id")
  
  #select only columns without NA in targets
  #colSums(is.na(targets)) # Count NA
  
  tar.bact<-tar.bact[,-2:-3] #remove hight, weight because they have NA
  
  
  
  #remove columns with all 0 (!run after once NA are removed)
  # tar.bact <- remove_zerocolumns(tar.bact)
  #remove columns with near zero variance
  #f a variable has very little change or variation, 
  #it's like a constant and not useful for prediction. 
  remove.index <- nearZeroVar(tar.bact)
  tar.bact <- tar.bact[,-remove.index]
  #remove incomplete cases
  tar.bact <- na.omit(tar.bact)
  
  return(tar.bact)
  
}


#' @title Create_targets
#' @description Create  targets dataframe
#' @param path is the path of the targets dataset
#' @import openxlsx
#' @export
#' @return dataframe

Create_targets<- function(path){
 # targets<-read.xlsx("/Users/marzia/MyDir/GenomicsData/data_raw/database_sani_ok2.xlsx")
  targets<-read.xlsx(path)
  
  targets<-targets[,c("Codice.episodio.lab","Hight","Weight","Smoking", "Sport",
                      "Ipertensione","Diabete","Sex","Computer","Glasses","Age",
                      "NAZIONE","reflusso.Gastro.Esofageo","Etnia")]
  # !Attenzione:mancano altre colonne
  
  targets.names<-c("id","hight","weight","smoking",
                   "sport","ipertension","diabete","sex","computer", 
                   "glasses","age","nationality","reflux","etnicity")
  
  colnames(targets)<- targets.names
  
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
  targets$sport[targets$sport == "S"]<-1 
  return(targets)
}



reformat_etnicity<- function(targets){
  targets$etnicity[targets$etnicity == "A"]<-1 
  targets$etnicity[targets$etnicity == "L"] <- 1
  targets$etnicity[targets$etnicity == "LAT"] <- 1
  targets$etnicity[targets$etnicity == "C"] <- 0
  
  return(targets)
}


targets_to_factor<- function(targets){
  targets$sex<-factor(targets$sex)
  targets$smoking<-factor(targets$smoking)
  targets$glasses<-factor(targets$glasses)
  targets$sport<-factor(targets$sport)
  #to_correlate$Age<-factor(to_correlate$Age)
  targets$computer<-factor(targets$computer)
  targets$nationality<-factor(targets$nationality)
  targets$ipertension<-factor(targets$ipertension)
  targets$diabete<-factor(targets$diabete)
  
  return(targets)
}



#' @title Create_bacteria
#' @description Create bacteria dataframe
#' @param path is the path of the bacteria dataset
#' @import openxlsx
#' @export
#' @return dataframe

Create_bacteria<- function(path){
 # genus<-read.csv("/Users/marzia/MyDir/GenomicsData/genus.csv")
  genus<-read.csv(path)
  
  #Create dataframe bacteria
  Taxonomy <- data.frame(colnames(genus))
  
  Taxonomy <- Taxonomy %>%
    mutate(id =paste("bacteria", rownames(Taxonomy), sep = ""))
  
  colnames(Taxonomy)<- c("bacteria","id")
  
  bacteria<-genus
  colnames(bacteria)<-Taxonomy$id
  
  
  
  #Get number of column of "pz"
  genus.nc<-which(colnames(genus)=="pz")
  
  #rename last column "id"
  colnames(bacteria)[genus.nc] = "id"
  
  
  #move position o column id to firts column
  bacteria<-bacteria %>% relocate(id)
  
  #Get number of column of "id" to text
  bacteria.nc<-which(colnames(bacteria)=="id")
  
  return(bacteria)
}


wilcox.test.bacteria <- function(tar.bact, target) {  
  k <- which(colnames(tar.bact) == target)
  nl <- ncol(tar.bact)
  
  tar.bact3 <- as.data.frame(tar.bact[,c(k,13:nl)])
  
  myList<-list()
  
  for(i in 9:ncol(tar.bact3)) {  #bacteria
    bacteria <- tar.bact3[,i]
    target <- tar.bact3[,1]
    test <- wilcox.test(bacteria ~ target)
    
    if(test$p.value<=0.05){ #if the p-value is <=0.05 at 
      #the 5% significance level, 
      #we reject the null hypothesis and we conclude that 
      #bacteria are significantly different between sex (1/0).
      print(paste("i=", i))
      print(paste("#Bacteria name=", colnames(tar.bact3)[i])) 
      
      myList <- c(myList, colnames(tar.bact3)[i])
    }   
    
  }
  
  return(myList)
}

bacteria_normal <- function(bacteria) {  
  #Vrifica se qualche bacteria è normally distribuited
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
