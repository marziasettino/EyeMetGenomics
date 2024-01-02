library(EyeMetGenomics)
library(dplyr)
library(caret)
library(openxlsx)
library(rsample)
library(recipes)

#----------------Preprocessing--------------
path<-file.path(getwd())


filenmToLoadTargets <- "database_sani_complete2.xlsx"
pathToLoadTargets<-paste0(path,"/data/data_raw/",filenmToLoadTargets)  
pathToLoadTargets

filenmToLoadBacteria <- "genus.csv"
pathToLoadBacteria<-paste0(path,"/data/data_raw/",filenmToLoadBacteria)  


filenmToSavePreprocessedSplit <- "PreprocessedSplit_02_Gen_23.RData"
pathToSavePreprocessedSplit<-paste0(path,"/data/Preprocessed/",filenmToSavePreprocessedSplit)  




targets<-Create_targets(pathToLoadTargets)
targets <- targets_to_numeric(targets)



bacteria<-Create_bacteria(pathToLoadBacteria)

#save(targets,bacteria, file = "data/originalDataset.rda")



########## MERGE #######################
tar.bact<-merge(targets,bacteria, by = "id")


#-----------------------STEP 1 - subsetting Target feautures---------

tar.bact.smoking <- get_target_df(tar.bact,"smoking")
tar.bact.sex <- get_target_df(tar.bact,"sex")
tar.bact.sport <- get_target_df(tar.bact,"sport")
tar.bact.ipertension <- get_target_df(tar.bact,"ipertension")
tar.bact.diabete <- get_target_df(tar.bact,"diabete")
tar.bact.computer <- get_target_df(tar.bact,"computer")
tar.bact.glasses <- get_target_df(tar.bact,"glasses")
tar.bact.nationality <- get_target_df(tar.bact,"nationality")
tar.bact.allergy <- get_target_df(tar.bact,"allergy")
tar.bact.continent <- get_target_df(tar.bact,"continent")
tar.bact.hemisphere <- get_target_df(tar.bact,"hemisphere")
tar.bact.reflux <- get_target_df(tar.bact,"reflux")
tar.bact.etnicity <- get_target_df(tar.bact,"etnicity")
#----- Not categorical variables as outcome=>categorized------
tar.bact.age <- get_target_df(tar.bact,"age")
tar.bact.bmi <- get_target_df(tar.bact,"bmi")

#--------------------



tar.bact.smoking <- preprocess_split_dataset(tar.bact.smoking)
tar.bact.sex <- preprocess_split_dataset(tar.bact.sex)
tar.bact.sport <- preprocess_split_dataset(tar.bact.sport)
tar.bact.ipertension <- preprocess_split_dataset(tar.bact.ipertension)
#tar.bact.diabete <- preprocess_split_dataset(tar.bact.diabete)
tar.bact.computer <- preprocess_split_dataset(tar.bact.computer)
tar.bact.glasses <- preprocess_split_dataset(tar.bact.glasses)
tar.bact.nationality <- preprocess_split_dataset(tar.bact.nationality)
tar.bact.allergy <- preprocess_split_dataset(tar.bact.allergy)
tar.bact.continent <- preprocess_split_dataset(tar.bact.continent)
tar.bact.hemisphere <- preprocess_split_dataset(tar.bact.hemisphere)
tar.bact.reflux <- preprocess_split_dataset(tar.bact.reflux)
tar.bact.etnicity <- preprocess_split_dataset(tar.bact.etnicity)
#----- Not categorical variables as outcome=>categorized------
tar.bact.age <- preprocess_split_dataset(tar.bact.age)
tar.bact.bmi <- preprocess_split_dataset(tar.bact.bmi)



save.image(pathToSavePrepocessedSplit)


# save(tar.bact.smoking,
#      tar.bact.sport,
#      tar.bact.ipertension,
#      tar.bact.sex,
#      tar.bact.computer,
#      tar.bact.glasses,
#      tar.bact.nationality,
#      tar.bact.allergy,
#      tar.bact.hemisphere,
#      tar.bact.continent,
#      tar.bact.diabete,
#      tar.bact.reflux,
#      tar.bact.etnicity,
#      tar.bact.age,
#      tar.bact.bmi,
#      file = "data/prepocessedDataset_single.rda")


save(tar.bact.smoking, file = "data/tar.bact.smoking.rda")
save(tar.bact.sport, file = "data/tar.bact.sport.rda")
save(tar.bact.ipertension, file = "data/tar.bact.ipertension.rda")
save(tar.bact.sex, file = "data/tar.bact.sex.rda")
save(tar.bact.glasses, file = "data/tar.bact.glasses.rda")
save(tar.bact.nationality, file = "data/tar.bact.nationality.rda")
save(tar.bact.allergy, file = "data/tar.bact.allergy.rda")
save(tar.bact.hemisphere, file = "data/tar.bact.hemisphere.rda")
save(tar.bact.continent, file = "data/tar.bact.continent.rda")
save(tar.bact.diabete, file = "data/tar.bact.diabete.rda")
save(tar.bact.reflux, file = "data/tar.bact.reflux.rda")
save(tar.bact.etnicity, file = "data/tar.bact.etnicity.rda")
save(tar.bact.age, file = "data/tar.bact.age.rda")
save(tar.bact.bmi, file = "data/tar.bact.bmi.rda")



