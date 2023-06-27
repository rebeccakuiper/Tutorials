
#### Updating hypotheses with GORIC and GORICA: ANOVA Example

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

###################################################################################


# Notably, it is only possible to load the data if you are using the correct 
# working directory (with both your R script and data file). 
# The command `getwd()` shows you your current working directory.
#
# You can change the working directory to the one you prefer using the function 
# `setwd()` by specifying the correct location between parentheses. 
# Alternatively, in Rstudio, you can use the "Session" tab (on top) or you can 
# use the "Files"-pane (on top of probably the right lower box of Rstudio, this 
# pane is located next to the panes for "Plots", "Packages", "Help", and "Viewer").


### Example Monin ### 

# Read/Load the Data # 
# If you open the data file `Data_Monin.txt` in a text editor, 
# you can see that the variable labels have been inserted (using quotes; i.e., 
# "...") in the first line of the file, which is called a header. 
# Therefore, you have to specify 'header = TRUE' when loading the data:
Monin <- read.table("Data_Monin.txt", header=TRUE)

# Make the variable group a factor #
#
# Since a .txt file was loaded, R does not know the measurement levels of the 
# variables and assumes all of them to be continuous, meaning that there
# measurement level is interval or ratio. 
# Hence, especially when there are more than two groups, 
# one has to tell R that the variable `group` is a factor by using the 
# `factor()` function on the `group` variable (i.e., a grouping / categorical / 
# nominal variable):
#
Monin$group <- factor(Monin$group)  
# this command tells R that group is a factor 
# and not a continuous variable like Influence

# Inspect the data #
head(Monin) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(Monin$attract, Monin$group, mat = TRUE)
descrstat



## Compute GORIC values and weights ##
#In this example, we will use the Monin data set to render theory-based 
#hypotheses for the replication data set of Holubar.

## Monin data

# One needs an R object with unconstrained estimates 
# (here, three group means and one residual variance)
lm_fit_Monin <-  lm(attract ~ group-1, data = Monin)
#
#Note that:
# 1. `y ~ group - 1` instructs the function `lm` (linear model) to regress the 
#    variable y on the variable group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means of each group, resulting, here, in three group means. 
#    On the other hand, if the intercept is not dropped, `y ~ group' would 
#    estimate an intercept, representing the mean of the reference group, and 
#    the mean differences between the other (here, two) groups and the 
#    reference group.
# 3. The results are collected in, what is called, an R-object, named 
#    `lm_fit_Monin`.

# Check the names used in model
names(coef(lm_fit_Monin))
# Specify restrictions using those names

# Hypotheses Set
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
# On the Monin data set, we are going to do an exploratory analysis, 
# which means that we are going to use all combinations with equalities 
# (and no restrictions):
H00 <- 'group1 = group2 = group3'
H01 <- 'group1 = group2'  
H02 <- 'group1 = group3'   
H03 <- 'group2 = group3'


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
goric(lm_fit_Monin, hypotheses = list(H00, H01, H02, H03))
#
# It can be seen that $H_01$ ($\mu_1 = \mu_2, \mu_3$) receives the most support. 
# Based on the means (see `descrstat`), 
# where we see that $\mu_1$ and $\mu_2$ are both larger than $\mu_3$, 
# we will evaluate the following hypothesis in the Holubar data: 
# $H_1: \mu_1 = \mu_2 > \mu_3$.
# Since $H_u$ obtained some support as well, 
# we could specify (using the sample means of the Monin data) 
# the following competing hypothesis: $H_2: \mu_2 > \mu_1 > \mu_3$.


# In case you want to use the GORICA values and weights instead, use:
#
#set.seed(123) # Set seed value
#goric(fit.lm_Monin, H00, H01, H02, H03, type = "gorica")




### Replication of the Monin study: Holubar

Holubar <- read.table("Data_Holubar.txt", header = TRUE)
Holubar$gr <- factor(Holubar$gr) 
# this command tells R that gr is a factor and not a continuous variable like at

## Inspect data
#head(Holubar)

## Compute descriptives for each group
#descrip <- describeBy(Holubar$at,Holubar$gr,mat=TRUE)
#print(descrip)

# lm object (of ANOVA model)
lm_fit_Holubar <-  lm(at ~ gr-1, data = Holubar)

# Check names used in model
names(coef(lm_fit_Holubar))
# Specify restrictions using those names

# Set hypotheses
#
# Based on the results and sample means of Monin, 
# we created two competing hypotheses: 
H1 <- 'gr1 = gr2 > gr3' 
H2 <- 'gr2 > gr1 > gr3'  

# Calculate GORIC values and weights
#
set.seed(123) # Set seed value
output_repl <- goric(lm_fit_Holubar, hypotheses = list(H1, H2))
summary(output_repl)
#
# Since the support for $H_1$ and $H_2$ is lower than for $H_u$, 
# both are weak hypotheses. 
# Hence, the study of Holubar did not replicate the findings of Monin.

# In case you are only interested in the 'main hypothesis' $H_1$ found in Monin, 
# you could also evaluate this against its complement:
set.seed(123) # Set seed value
goric(lm_fit_Holubar, hypotheses = list(H1), comparison = "complement")
# Since $H_1$ has only 0.39 (lower than 1) times more support than its 
# complement, it is a weak hypothesis. 
# Hence, the study of Holubar did not replicate the findings of Monin.


## Notably, in case you want to use the GORICA, use the following commands:
#
#set.seed(123) # Set seed value
#goric(lm_fit_Holubar, hypotheses = list(H0, H1, H2), type = "gorica")
##
## When you want to calculate the GORICA for $H_1$ and its complement, use:
#set.seed(123) # Set seed value
#goric(lm_fit_Holubar, hypotheses = list(H1), type = "gorica", comparison = "complement")


###################################################################################


# restriktor (0.4-60): generalized order-restricted information criterion: 
#   
# Results:
#         model    loglik  penalty    goric  loglik.weights  penalty.weights  goric.weights
# 1          H1  -144.981    2.500  294.962           0.125            0.731          0.280
# 2  complement  -143.038    3.500  293.076           0.875            0.269          0.720
# --- 
# The order-restricted hypothesis ‘H1’ has 0.390 times more support than its complement.
