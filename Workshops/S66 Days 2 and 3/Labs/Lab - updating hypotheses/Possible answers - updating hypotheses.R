if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy

if (!require("bain")) install.packages("bain") # install this package first (once)
library(bain) # for bain function

if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

###################################################################################


JU <- read.table("JU.txt", header=TRUE)

# Make the variable group a factor #
#Since we loaded a txt file, R does not know the measurement levels of the variables 
#                           and assumes all to be continuous (so, interval or ratio). 
#Hence, we need to tell R that the variable `group` is a factor 
#           (i.e., a grouping / categorical / nominal variable): 
JU$g <- factor(JU$g)  # this command tells R that g is a factor and not a continuous variable 

# Inspect the data #
head(JU) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(JU$z, JU$g, mat = TRUE)
descrstat



## Compute the GORIC ##
#In this example, we will use the JU data set to render theory-based hypotheses 
#for the replication data set of C.

## JU data

# First, we need the R object with unconstrained estimates
lm_fit_JU <-  lm(z ~ g-1, data = JU)
# Note that:
# 1. `y ~ group - 1` instructs the function `lm` (linear model) to regress y on group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means in each group, resulting here in five group means. 
#    On the other hand,  `y ~ group' would estimate an intercept, 
#    representing the mean of the reference group, and 
#    the mean differences between the other (here, four) groups and the reference group.
# 3. The results are collected in, what is called, an R-object, named `lm_fit_...`.

# Check the names used in model
names(coef(lm_fit_JU))
# Specify restrictions using those names

# Hypotheses Set

#On the JU data set, we could do an exploratory analysis, 
# which means that we are going to use all combinations with equalities (and no restrictions).
#With 4 means there are a lot of combinations, so you may want to reduce the number of hypotheses; 
# especially when you have some expectations already.
#In case there are already some theories in the literature, then use those.
#I just assume, the following two exist in the literature 
# (you can of course specify different ones or even only one):
H1_JU <- 'grh > gph > grl > gpl'   # hypothesis could also be specified as pairwise restrictions
H2_JU <- 'grh > gph == grl > gpl'  
# Note, these are used as competing hypotheses. 
#
# It is possible to have two theories which are not competing.
# Say, one addressing comparison between men and women and another one addressing different education levels.
# Then, do not evaluate them together, but each separately against their own complement.



# Calculate GORIC values and weights
#In the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. 
#Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value.
#   If it is sensitive, then increase number of iterations used in calculation of the penalty.
set.seed(123) # Set seed value
goric_JU <- goric(lm_fit_JU, hypotheses = list(H1_JU, H2_JU))
goric_JU
summary(goric_JU)
# Both H1 and H2 are not weak hypotheses, since their support is stronger than for the unconstrained.
# Since at least one of the competing hypotheses is not weak, one can compare their support.
#
# It can be seen that H1_JU receives the most support. 
# But H2 does obtain some support as well.
# Now, it is up to you as a researcher to decide whether you will only evaluate H1_JU or both H1_JU and H2_JU in the replication study.



### Replication of the JU study: C

C <- read.table("C.txt", header = TRUE)
C$g <- factor(C$g) # this command tells R that gr is a factor and not a continuous variable like at

# If one wish to inspect the data: 
## Inspect data
#head(C)

## Compute descriptives for each group
#descrip <- describeBy(C$z,C$g,mat=TRUE)
#print(descrip)

# lm object (of ANOVA model)
lm_fit_C <-  lm(z ~ g-1, data = C)

# Check names used in model
names(coef(lm_fit_C))
# Specify restrictions using those names

# Set hypotheses
# Based on the results of JU, we perhaps only want to inspect H1_JU.
# Note that if you evaluated only equalities in the first set,
# you can use the sample means to come to order-restricted hypotheses.
# Then, use those here (so, then you update your hypotheses).
H1_C <- H1_JU

# Calculate GORIC values and weights
set.seed(123) # Set seed value
goric_c <- goric(lm_fit_C, hypotheses = list(H1_C), comparison = 'complement')
goric_c
summary(goric_c)
# The order-restricted hypothesis ‘H1_C’ has 25.826 times more support than its complement.

###################################################################################
