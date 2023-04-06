# Below, you find code to generate all possibilities 
#  when setting a parameter to 0.

# Code to obtain all possible equality hypotheses:

# Make all combinations of possible =hypotheses
nr_groups <- 3
names_group <- paste0("est", 1:nr_groups) 
#or if you have a fit object: names_group <- names(coef(<insert fit object>)) 
h <- paste(names_group, '== 0')
#h
#make columns per number of variables in hypothesis
sizeCol <- matrix(0, nrow = nr_groups, ncol = 1)
hypothesis_save <- matrix(NA, nrow = 1, ncol = 1)
teller <- 0
for(i in 1:nr_groups){
  col <- matrix(combn(h, i, 
                      FUN = function(x){
                        return(paste(x, collapse = '; '))
                        }
                      ), ncol = 1)
  #assign(paste0("col", i), col)
  sizeCol[i] <- length(col)
  hypothesis <- matrix(NA, nrow = (1+sum(sizeCol)), ncol = 1)
  hypothesis[1:(1+teller), 1] <- hypothesis_save
  hypothesis[(1+teller+1):(1+teller+sizeCol[i]), 1] <- col
  hypothesis_save <- hypothesis
  teller <- teller + sizeCol[i]
}
# Delete first row, because it says NA there now
hypothesis <- matrix(hypothesis[-1,], ncol = 1) 
# Reverse ordering, such that it goes from 
#  very restricted to less restricted hypotheses
hypothesis <- apply(hypothesis,2,rev)
#hypothesis
#
#create all hypotheses H01 to H0nrhypos
for(i in 1:nrhypos){
  assign(paste("H0", i, sep = ""), hypothesis[i])    
}
#H01
#H02

# Print names with commas such that you can use that text in the goric function:
noquote(paste0("H0", 1:nrhypos, ","))
