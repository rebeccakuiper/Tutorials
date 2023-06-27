
#### Evaluate H1 vs its complement with GORIC: ANOVA Example ####

# Load libraries. 
# These contain functions such as 'goric' that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.
#
# Install the packages (once) 
if (!require("psych")) install.packages("psych") 
# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function
# If from CRAN:
if (!require("restriktor")) install.packages("restriktor")
#
# Load 
library(psych) # for the function describeBy
library(restriktor) # for goric function

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
H1 <- 'group5 = group3 > group1; group3 > group4 > group2; group1 > group2' 
# Note: H1 is not full row-rank, 
#       see below and the goric tutorial for more details.

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
output_c <- goric(lm_fit_Lucas, hypotheses = list(H1), comparison = "complement")
summary(output_c)
#
# The order-restricted hypothesis H1 has  13.4 times more support 
#                                                           than its complement.             


################################################################################


# restriktor (0.4-60): generalized order-restricted information criterion: 
#   
# Level probabilities:
#   Number of requested bootstrap draws 99999 
#   Number of successful bootstrap draws for H1: 99999
# 
# Results:
#         model    loglik  penalty    goric  loglik.weights  penalty.weights  goric.weights
# 1          H1  -278.051    3.195  562.493           0.499            0.931          0.931
# 2  complement  -278.048    5.795  567.688           0.501            0.069          0.069
# --- 
# The order-restricted hypothesis ‘H1’ has 13.431 times more support than its complement.
# 
# Restriktor message: Since the constraint matrix is not full row-rank, the level probabilities 
# are calculated using mix.weights = "boot" (the default is mix.weights = "pmvnorm").
# For more information see ?restriktor.