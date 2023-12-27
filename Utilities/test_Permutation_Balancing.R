library(gtools) 

# generating combinations of the 
# alphabets taking 2 at a time 
print ("Combination of five objects taken two at a time") 
#comb <- combinations(5, 2) 




vec<- seq(1, 10, by=1)
res1 <- gtools::permutations(n=4,r=2,v=vec,repeats.allowed=T) 



# train_data <- DF[["bmi"]][[1]] 
# test_data <- DF[["bmi"]][[2]] 

# balancedList[["bmi"]]<- balancing(train_data,test_data,2,80) 
# balancedList[["bmi"]][["IMR"]]



train_data <- DF[["age"]][[1]] 
test_data <- DF[["age"]][[2]] 

for(i in 1:length(res1)) { 
  perm <- res1[i,]
  kappa <- perm[1]
  dupSZ <- perm[2]
balancedList[["age"]]<- balancing(train_data,test_data,kappa,dupSZ) 
balancedList[["age"]][["IMR"]]
}


########## BMI #################


vec<- seq(1, 20, by=1)
res1 <- gtools::permutations(n=4,r=2,v=vec,repeats.allowed=T) 


train_data <- DF[["bmi"]][[1]] 
prop.table(table(train_data$Target)) 

test_data <- DF[["bmi"]][[2]] 
kappa <- 1
dupSZ <- 5
for(i in 1:length(res1)) { 
  perm <- res1[i,]
  #kappa <- perm[1]
  dupSZ <- perm[2]
  balancedList[["bmi"]]<- balancing(train_data,test_data,kappa,dupSZ) 
  balancedList[["bmi"]][["IMR"]]
}


 