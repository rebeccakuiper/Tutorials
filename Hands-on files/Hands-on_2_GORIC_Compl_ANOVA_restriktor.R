
#### Evaluate H1 vs its complement with GORIC: ANOVA Example ####

# Load libraries. 
# These contain functions such as 'goric' that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.
#
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


###  Example Palmer & Gough ### 

# Data
PandG_data <- read.table("Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 

# Fit object
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)

# Informative hypothesis
# This is known before inspecting the lm results.
# Check the names used in model
names(coef(fit.PandG))
# Specify restrictions using those names
H1 <- 'group1 > group2 > group3' 
# vs its complement (default, in case of 1 hypothesis).

# GORIC
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
goric.PandG_c <- goric(fit.PandG, 
                       hypotheses = list(H1 = H1))
goric.PandG_c
#summary(goric.PandG_c)


################################################################################


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



## Compute the GORIC ##

# One needs an R object with unconstrained estimates 
# (here, five group means and one residual variance)
lm_fit_Lucas <-  lm(Influence ~ group-1, data = Lucas)


# Informative hypothesis
#
# This is known before inspecting the lm results.
#
# Check the names used in model
names(coef(lm_fit_Lucas))
# Specify restrictions using those names
#
H1 <- 'group5 = group3 > (group1, group4) > group2'
# This is evaluated against its complement (default in case of 1 hypothesis).

# Calculate GORIC values and weights
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
output_c <- goric(lm_fit_Lucas, 
                  hypotheses = list(H_theory1 = H1))
output_c
#summary(output_c)
#
# The order-restricted hypothesis ‘H_theory1’ has  (>1 times) more support 
#                                                           than its complement.             
#
#
#Note: The log-likelihood (loglik) weights / fit values seem to be quite close.
# In this example, it is probably due to the equality restriction: 
# An equality can never be exactly true in the sample.
# In this sample it is close, that is, the two sample means are almost equal. 
# Hence, the complement has the maximum fit (not H_theory1) and almost equal fit.
# Assuming that the equality is of real interest, 
# one could think about specifying an about-equality.
# If the about-equality is true in the sample, then H_theory1 will have maximum fit.
#
# In general:
#Equal or nearly equal fit values indicate that one or more of the 
#inequality constraints can be replaced by (about-)equality constraints. 
#Notably, one could investigate with the benchmarks function (discussed later), 
#using 'output_type = "rlw"', whether the loglik weights indeed are close.
#
#For more information, see the guidelines ('Guidelines_output_GORIC.html') 
# and/or the benchmark tutorial on https://github.com/rebeccakuiper/Tutorials. 


########################



