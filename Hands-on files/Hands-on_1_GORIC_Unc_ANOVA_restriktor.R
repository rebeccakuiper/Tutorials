
#### Evaluate a set of hypotheses vs Hunc with GORIC: ANOVA Example ####

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


########################


# Example Palmer & Gough

# Data
PandG_data <- read.table("Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 

## NHST: pairwise testing
#pairwise.t.test(PandG_data$Importance, PandG_data$group, p.adj = 'bonferroni')

# Fit object, also NHST
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)
# - 1 says to leave out the intercept; rendering group means (not group mean differences)
#summary(fit.PandG) # NHST


# (Informative) hypotheses
names(coef(fit.PandG)) # To check labels to be used in hypotheses specification
H0 <- 'group1 = group2 = group3' # Only if of interest!
H1 <- 'group1 > group2 > group3' 
# Note: if no comparison with, say, group3, then leave that out: 'group1 > group2' (not 'group1 > group2, group3' or so)
#
# Note: If equalities are of interest, I would inspect about-equalities.
# For more details, see the tutorial(s) on my github page.


# GORIC
set.seed(123) # Set seed value
goric.PandG <- goric(fit.PandG, hypotheses = list(Hnull = H0, H1 = H1))
#goric.PandG$result[,1] <- c("H0","H1","Hu")
goric.PandG
#goric.PandG$result
#summary(goric.PandG)
goric.PandG$ratio.gw

###

# Most probably, H0 is not of interest, but perhaps there is another theory:

# Informative hypotheses
H1 <- 'group1 > group2 > group3' 
H2 <- 'group1 > group2 < group3'

# GORIC
set.seed(123) # Set seed value
goric.PandG_H2 <- goric(fit.PandG, hypotheses = list(H1 = H1, H2 = H2))
goric.PandG_H2
#goric.PandG_H2$result
#summary(goric.PandG_H2)
goric.PandG_H2$ratio.gw


#####################


### Example Lucas ### 

# Read/Load the Data # 
# If you open the data file `Data_Lucas.txt` in a text editor, 
# you can see that the variable labels have been inserted (using quotes; i.e., 
# "...") in the first line of the file, which is called a header. 
# Therefore, you have to specify 'header = TRUE' when loading the data:
Lucas <- read.table("Data_Lucas.txt", header=TRUE)

# Make the variable 'group' a factor #
#
# Since a .txt file was loaded, R does not know the measurement levels of the 
# variables and assumes all of them to be continuous, meaning that there
# measurement level is interval or ratio. 
# Hence, especially when there are more than two groups, 
# one has to tell R that the variable `group` is a factor by using the 
# `factor()` function on the `group` variable (i.e., a grouping / categorical / 
# nominal variable):
#
Lucas$group <- factor(Lucas$group)  
# this command tells R that group is a factor 
# and not a continuous variable like Influence

# Inspect the data #
head(Lucas) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(Lucas$Influence, Lucas$group, mat = TRUE)
descrstat



## Compute the GORIC ##

# One needs an R object with unconstrained estimates 
# (here, five group means and one residual variance)
lm_fit_Lucas <-  lm(Influence ~ group-1, data = Lucas)
#
#Note that:
# 1. `y ~ group - 1` instructs the function `lm` (linear model) to regress the 
#    variable y on the variable group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means of each group, resulting, here, in five group means. 
#    On the other hand, if the intercept is not dropped, `y ~ group' would 
#    estimate an intercept, representing the mean of the reference group, and 
#    the mean differences between the other (here, four) groups and the 
#    reference group.
# 3. The results are collected in, what is called, an R-object, named 
#    `lm_fit_Lucas`.
  

# Check the names used in model
names(coef(lm_fit_Lucas))
# Specify restrictions using those names

# Hypotheses Set
#
# These are known before inspecting the lm results.
#
# To evaluate the hypotheses of interest, it is necessary to specify the 
# restrictions in these hypotheses correctly:
#  * Within the `restriktor()` and `goric()` functions, it is possible to use 
#    the following operators: `>`, `<`, `=`, `<=`, `>=`, `==` 
#    (where the last three denote the same constraint as the first three). 
# * The `goric()` and the `restriktor()` functions can deal with:
#   + pairwise restrictions separated by a semicolon `;` 
#     (e.g., *"beta1 > beta2; beta2 = beta3"*).
#   + combined restrictions consisting of more than one operator 
#     (e.g., *"beta1 > beta2 = beta3"*).
#   Note that one should use the labels of the parameter estimates 
#   (in the example above: group1-group5).
# * One can also define hypothesis in terms of linear functions of parameters. 
#   (For more details, see 'Extra possibility specification hypotheses' near the 
#    end of the goric() tutorial called 'Tutorial_GORIC_restriktor_General').
#
# Summary: 
# - goric uses "=" or "==" to denote an equality restriction
# - goric uses ";" to separate the restrictions within one hypothesis
#
#H0 <- 'group1 = group2 = group3 = group4 = group5' # only when of interest!
H1 <- 'group5 = group3 > (group1, group4) > group2'
# which is the same as:
#H1 <- 'group5 = group3 > group1 > group2; group3 > group4 > group2' 
# Note: H1 is not full row-rank, 
#       see below and the goric tutorial for more details.
H2 <- 'group3 > group1; group1 > group4; group4 = group5; group5 > group2'


# Calculate GORIC values and weights
#
#In the calculation of the GORIC, an iterative process is needed to calculate 
#the penalty / complexity part. Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty 
#   value.
#   If it is sensitive, then increase number of iterations used in calculation 
#   of the penalty (see below).
#
set.seed(123) # Set seed value
output <- goric(lm_fit_Lucas, hypotheses = list(H1 = H1, H2 = H2))
output
#summary(output)
output$ratio.gw
#
# It can be seen that the order-restricted hypothesis $H_1$ has (>1 
# times) more support than $H_u$ (the unconstrained hypothesis). 
# Hence, $H_1$ is not a weak hypotheses and can be compared to the other (weak 
# and non-weak) competing hypotheses: 
# $H_1$ is (>1 times) more likely than $H_2$.



## Two methods to calculate the penalty ##

# There are two methods that can be used in calculating the penalty. 
# The default method is often much faster and needs less input specification. 
# It can, however, not deal with hypotheses that are not of full row-rank 
# (like $H_1$ above). 
# In that case, `restriktor` uses automatically the other ('boot'/bootstrap) method.
#
#For additional details, see '?goric' or the Vignette.


################################################################################
