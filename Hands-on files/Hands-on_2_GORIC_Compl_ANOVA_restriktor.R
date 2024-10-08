
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


# Informative hypothesis
#
# This is known before inspecting the lm results.
#
# To evaluate the hypotheses of interest, it is necessary to specify the 
# restrictions in these hypotheses correctly:
#  * Within the `restriktor()` and `goric()` functions, it is possible to use 
#    the following operators: `>`, `<`, `=`, `<=`, `>=`, `==` 
#    (where the last three denote the same constraint as the first three). 
# * The `goric()` and the `restriktor()` functions can deal with:
#   + pairwise restrictions separated by a semicolon `;` or '&' 
#     (e.g., *"beta1 > beta2; beta2 > beta3"*).
#   + combined restrictions consisting of more than one operator 
#     (e.g., *"beta1 > beta2 > beta3"*).
#   Note that one should use the labels of the parameter estimates 
#   (in the example above: group1-group5).
# * One can also define hypothesis in terms of linear functions of parameters. 
#   (For more details, see 'Extra possibility specification hypotheses' near the 
#    end of the goric() tutorial called 'Tutorial_GORIC_restriktor_General').
# * One can use 'abs()' in case one wants to specify an absolute strength; 
#   e.g., abs(beta1) > abs(beta2); abs(beta2) > abs(beta3).
#
# Check the names used in model
names(coef(lm_fit_Lucas))
# Specify restrictions using those names
#
H1 <- 'group5 = group3 > (group1, group4) > group2'
# which is the same as:
#H1 <- 'group5 = group3 > group1 > group2; group3 > group4 > group2' 
# Note: H1 is not full row-rank, 
#       see below and the goric tutorial for more details.
# This is evaluated against its complement (default in case of 1 hypothesis).

# Calculate GORIC values and weights
#
#In the calculation of the GORIC, an iterative process is needed to calculate 
#the penalty / complexity part. Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty 
#   value.
#   If it is sensitive, then increase number of iterations used in calculation 
#   of the penalty.
#
set.seed(123) # Set seed value
output_c <- goric(lm_fit_Lucas, 
                  hypotheses = list(H_theory1 = H1))
output_c
#summary(output_c)
#
# The order-restricted hypothesis ‘H_theory1’ has  (>1 times) more support 
#                                                           than its complement.             


########################


# Example Palmer & Gough

# Data
PandG_data <- read.table("Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 

# Fit object
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)

# Informative hypothesis
H1 <- 'group1 > group2 > group3' 
# vs its complement (default, in case of 1 hypothesis).

# GORIC
set.seed(123) # Set seed value
goric.PandG_c <- goric(fit.PandG, 
                     hypotheses = list(H1 = H1))
goric.PandG_c
#summary(goric.PandG_c)


################################################################################

