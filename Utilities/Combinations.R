library(gtools) 

# generating combinations of the 
# alphabets taking 2 at a time 
print ("Combination of five objects taken two at a time") 
#comb <- combinations(5, 2) 

vec<- seq(1, 10, by=1)

res1 <- permutations(n=4,r=2,v=vec,repeats.allowed=T) 