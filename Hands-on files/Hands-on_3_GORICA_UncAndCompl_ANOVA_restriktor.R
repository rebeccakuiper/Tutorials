
#### Evaluate a set of hypotheses vs Hunc and H1 vs its complement with GORICA: ANOVA Example ####

# Load libraries. 
# These contain functions such as 'goric' that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.

## First, install the packages, if you have not done this already:
if (!require("psych")) install.packages("psych")
if (!require("restriktor")) install.packages("restriktor")

## Then, load the packages:
library(psych) # for the function describeBy
library(restriktor) # for the goric function

# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function

################################################################################


# Notably, it is only possible to load the data if you are using the correct 
# working directory (with both your R script and data file). 
# The command `getwd()` shows you your current working directory.
#
# You can change the working directory to the one you prefer using the function 
# `setwd()` by specifying the correct location between parentheses. 
# Alternatively, in Rstudio, you can use the "Session" tab (on top) or you can 
# use the "Files"-pane (on top of probably the right lower box of Rstudio, this 
# pane is located next to the panes for "Plots", "Packages", "Help", and "Viewer").


### Example Lucas ### 

# Read/Load the Data # 
Lucas <- read.table("Data_Lucas.txt", header=TRUE)

# Make the variable 'group' a factor #
Lucas$group <- factor(Lucas$group)  

# Inspect the data #
head(Lucas) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(Lucas$Influence, Lucas$group, mat = TRUE)
descrstat



## Compute GORICA ##

# One needs an R object with unconstrained estimates 
# (here, five group means and one residual variance)
lm_fit_Lucas <-  lm(Influence ~ group-1, data = Lucas)


# Hypotheses set
# names(coef(lm_fit_Lucas))
#H0 <- 'group1 = group2 = group3 = group4 = group5' # only when of interest!
H1 <- 'group5 = group3 > (group1, group4) > group2'
# which is the same as:
#H1 <- 'group5 = group3 > group1; group3 > group4 > group2; group1 > group2' 
# Note: H1 is not full row-rank, 
#       see below and the goric tutorial for more details.
H2 <- 'group3 > group1; group1 > group4; group4 = group5; group5 > group2'
# and the unconstrained as failsafe (default, in case of multiple hypotheses)


# Calculate GORICA values and weights
#
# To apply the GORICA (to an lm object) instead of the GORIC (default), 
# one should use 'type = "gorica"'.
#
#Like in the GORIC, in the calculation of the GORIC, an iterative process is 
# needed to calculate the penalty / complexity part. 
# Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty 
#   value.
#   If it is sensitive, then increase number of iterations used in calculation 
#   of the penalty.


# A set of hypotheses vs Hunc
#
set.seed(123) # Set seed value
output_gorica <- goric(lm_fit_Lucas, 
                       hypotheses = list(H1 = H1, H2 = H2), 
                       type = "gorica")
output_gorica
#summary(output_gorica)
output_gorica$ratio.gw
#
# If you did the same analysis with the GORIC, you will see that the (relative) 
# weights are (about) the same for the GORIC and GORICA.
# Note: the ratio of weights may differ somewhat.
#
# From this output, it can be seen that the order-restricted hypothesis $H_1$ 
# has (>1 times) more support than $H_u$ (the unconstrained hypothesis). 
# Hence, $H_1$ is not a weak hypotheses and can be compared to the other (weak 
# and non-weak) competing hypotheses: 
# $H_1$ is (>1 times) more likely than $H_2$.


## In case of one informative hypothesis (H1) ##
#
# H1 vs its complement (default, in case of 1 hypothesis)
#If you have only one informative hypothesis ($H_1$), then evaluate this against 
# its complement (i.e., all other theories, thus excluding the one of interest).
H1 <- 'group5 = group3 > (group1, group4) > group2'
# which is the same as:
#H1 <- 'group5 = group3 > group1 > group2; group3 > group4 > group2'
# Note: H1 is not full row-rank, see the goric tutorial for more details.
#
# GORICA
set.seed(123) # Set seed value
output_gorica_c <- goric(lm_fit_Lucas, 
                         hypotheses = list(H1), 
                         type = "gorica")
output_gorica_c
#summary(output_gorica_c)
# The order-restricted hypothesis $H_1$ has (>1 times) more support than its 
# complement (and the weights are once again the same as compared to the GORIC).
#
#Note: The log-likelihood (loglik) weights seem to be quite close. 
#This could then indicate that one or more of the inequality constraints 
#can be replaced by (about-)equality constraints. 
#Notably, one could investigate with the benchmarks function (discussed later), 
#using 'output_type = "rlw"', whether the loglik weights indeed are close.  
#For more information, see the guidelines ('Guidelines_output_GORIC.html') 
# and/or the benchmark tutorial on https://github.com/rebeccakuiper/Tutorials. 


###


## In case of estimates and their covariance matrix ##
#
# Now, 'type = "gorica"' is default.
#
# H1 vs its complement  (default, in case of 1 hypothesis)
#If you have only one informative hypothesis ($H_1$), then evaluate this against 
# its complement (i.e., all other theories, thus excluding the one of interest).
H1 <- 'group5 = group3 > (group1, group4) > group2'
# which is the same as:
#H1 <- 'group5 = group3 > group1; group3 > group4 > group2; group1 > group2' 
# Note: H1 is not full row-rank, see the goric tutorial for more details.
#
# GORICA
set.seed(123) # Set seed value
est <- coef(lm_fit_Lucas)
VCOV <- vcov(lm_fit_Lucas)
output_gorica_c_est <- goric(est, VCOV = VCOV, 
                             hypotheses = list(H1))
output_gorica_c_est
#summary(output_gorica_c_est)
#
# The order-restricted hypothesis $H_1$ has (>1 times) more support than its 
# complement (and the weights are once again the same as compared to the GORIC).
#
#Note: The log-likelihood (loglik) weights seem to be quite close. 
#This could then indicate that one or more of the inequality constraints 
#can be replaced by (about-)equality constraints. 
#Notably, one could investigate with the benchmarks function (discussed later), 
#using 'output_type = "rlw"', whether the loglik weights indeed are close.  
#For more information, see the guidelines ('Guidelines_output_GORIC.html') 
# and/or the benchmark tutorial on https://github.com/rebeccakuiper/Tutorials. 
