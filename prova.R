library(EyeMetGenomics)
library(dplyr)
library(caret)

#----------------Preprocessing--------------

targets<-Create_targets("/Users/marzia/MyDir/GenomicsData/data_raw/database_sani_ok2.xlsx")
#targets <- targets_to_factor(targets)

bacteria<-Create_bacteria("/Users/marzia/MyDir/GenomicsData/genus.csv")
tar.bact <- merge_preprocessing(targets,bacteria)