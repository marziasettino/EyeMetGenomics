#https://www.datacamp.com/tutorial/pca-analysis-r
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# Note that, the PCA method is particularly useful when the variables 
# within the data set are highly correlated. Correlation indicates that 
# there is redundancy in the data. Due to this redundancy, PCA can be
# used to reduce the original variables into a smaller number of new variables
# ( = principal components) explaining most of the variance in the 
# original variables.

library("corrr")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")

library("tidyverse")
library("DataExplorer")

data_to_load_path <- paste0("~/MyDir/EyeMetGenomics/data/originalDataset.rda")
load(data_to_load_path)

#overview of the data
str(bacteria)
str(targets)

#Check for null values 
colSums(is.na(bacteria))
colSums(is.na(targets))

summary(bacteria[,1:50])
summary(targets)

targets %>% glimpse()
bacteria %>% glimpse()

bacteria %>% plot_intro()
targets %>% plot_intro()

############################ BoxPlot of age by sex ##################
targets$sex <- as.factor(targets$sex)

plt <- ggplot(targets, aes(x=sex, y=age, fill=sex)) + geom_boxplot()

plt2 <- plt + scale_fill_discrete(name="Sex",
                          breaks=c("0","1"),
                          labels=c("male", "female"))



plt3 <-plt2 + ggplot2::theme(
    legend.title = ggplot2::element_text(size = 15),
    legend.text = ggplot2::element_text(size = 15),
    plot.title = ggplot2::element_text(size=15, face="bold"),
    axis.title=ggplot2::element_text(size=12,face="bold"),
    axis.text.x = ggplot2::element_text(hjust = 1,face="bold",size = 12),
    axis.text.y = ggplot2::element_text(hjust = 1,face="bold",size = 12))


plt3

############################ BoxPlot of bmi by sex ##################
targets2 <-  df <- na.omit(targets)
targets2$sex <- targets2$sex





plt <- ggplot(targets2, aes(x=sex, y=bmi, fill=sex)) + geom_boxplot()

plt2 <- plt + scale_fill_discrete(name="BMI",
                                  breaks=c("0","1"),
                                  labels=c("male", "female"))



plt3 <-plt2 + ggplot2::theme(
  legend.title = ggplot2::element_text(size = 15),
  legend.text = ggplot2::element_text(size = 15),
  plot.title = ggplot2::element_text(size=15, face="bold"),
  axis.title=ggplot2::element_text(size=12,face="bold"),
  axis.text.x = ggplot2::element_text(hjust = 1,face="bold",size = 12),
  axis.text.y = ggplot2::element_text(hjust = 1,face="bold",size = 12))


plt3


########################################
targets$sex<- factor(targets$sex, levels = c(0,1), 
                          labels = c("male", "female"))

targets$computer<- factor(targets$computer, levels = c(0,1), 
                     labels = c("yes", "no"))

sex_plot <-ggplot(targets, aes(x="", y=sex, fill=sex)) + 
              geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)

sex_plot

computer_plot <-ggplot(targets, aes(x="", y=computer, fill=computer)) + 
  geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)
computer_plot

#Normalizing the data
bacteria_normalized <- scale(bacteria[,-1])
head(bacteria_normalized)

#Correlation matrix
corr_matrix <- cor(bacteria_normalized)
ggcorrplot(corr_matrix)

#PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)

#load each principal component2. 
data.pca$loadings[, 1:4]


fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

#Contribution of each variable 
fviz_cos2(data.pca, choice = "var", axes = 1:2)


fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

######################################################
library("FactoMineR")
library("factoextra")
library("corrplot")

res.pca <- PCA(bacteria[,-1], graph = FALSE)
print(res.pca)

#------Visualization and Interpretation
#eigenvalues measure the amount of variation retained by each principal component.
eig.val <- get_eigenvalue(res.pca)
eig.val

#Scree Plot, which is the plot of eigenvalues ordered from largest to the smallest. 

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)


# Coordinates of variables
head(var$coord, 4)

fviz_pca_var(res.pca, col.var = "black")

head(var$cos2, 4)


corrplot(var$cos2, is.corr=FALSE)




#--------
#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
library("psych")
sex <- targets[,c(1,8)]
tar.bact.sex<-merge(sex,bacteria, by = "id")
tar.bact.sex_normalized <- scale(bacteria[,-1])

pairs.panels(bacteria_normalized,
             gap = 0,
             bg = c("red", "yellow", "blue")[bacteria_normalized$Species],
             pch=21)



