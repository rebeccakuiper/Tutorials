
#### Evaluate a set of hypotheses vs Hunc with GORIC: ANOVA Example ####

# Load libraries. These libraries contain functions that will be used in this R code. 
# Each time you reopen this R file you have to execute this step.
#
if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy
#
if (!require("bain")) install.packages("bain") # install this package first (once)
library(bain)

###################################################################################


# Notably, it is only possible to load the data if you are using the correct working directory (with both your R script and data file). 
# The command `getwd()` shows you your current working directory. 
# You can change the working directory to the one you prefer using the function `setwd()` by specifying the correct location between parentheses. 
# Alternatively, in Rstudio, you can use the "Session" tab (on top) or 
# you can use the "Files"-pane (on top of probably the right lower box of your Rstudio-screen, this pane is located next to the panes for "Plots", "Packages", "Help" and "Viewer").


### Example Lucas ### 

# Read/Load the Data # 
# If you open the data file `Data_Lucas.txt` in a text editor, 
# you can see that the variable labels have been inserted (using quotes; i.e., "...") in the first line of the file, which is called a header. 
# Therefore, you have to specify 'header = TRUE' when loading the data:
Lucas <- read.table("Data_Lucas.txt", header=TRUE)

# Make the variable group a factor #
#Since we loaded a txt file, R does not know the measurment levels of the variables and assumes all to be continuous (so, interval or ratio). 
#Hence, we need to tell R that the variable `group` is a factor (i.e., a grouping / categorical / nominal variable): 
Lucas$group <- factor(Lucas$group)  # this command tells R that group is a factor and not a continuous variable like Influence

# Inspect the data #
head(Lucas) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(Lucas$Influence, Lucas$group, mat = TRUE)
descrstat



## Compute BF and PMS ##

# first need R object with unconstrained estimates (here, five group means and one residual variance)
lm_fit_Lucas <-  lm(Influence ~ group-1, data = Lucas)
# Note that:
# 1. `y ~ group - 1` instructs the function `lm` (linear model) to regress y on group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means in each group, resulting here in five group means. 
#    On the other hand,  `y ~ group' would estimate an intercept, 
#    representing the mean of the reference group, and 
#    the mean differences between the other (here, four) groups and the reference group.
# 3. The results are collected in, what is called, an R-object, named `lm_fit_lucas`.

# Check the names used in model
names(coef(lm_fit_Lucas))
# Specify restrictions using those names

# Hypotheses Set
#
# NOTES: 
# - bain uses "=" to denote an equality restriction
# - bain uses "," to separate the restrictions within one hypothesis
# - bain uses ";" to separate the hypotheses
#
H0 <- 'group1 = group2 = group3 = group4 = group5' 
H1 <- 'group5 = (group3, group4) > group1 > group2' # Note: H1 is not full row-rank, see below and the goric tutorial for more details.
H2 <- 'group3 > group1 > group4 = group5 > group2'
# Combine them to character (notably, only H0 and H2 used, not H1):
Hypotheses <- 'group1 = group2 = group3 = group4 = group5;
group3 > group1 > group4 = group5 > group2'


# Calculate GORIC values and weights
#In the calculation of the BF / PMPs, an iterative process is needed to calculate the penalty / complexity part. Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value 
#   If it is sensitive, then increase number of iterations used in calculation of the penalty (see below).
set.seed(123) # Set seed value
output <- bain(lm_fit_Lucas, Hypotheses)
#output <- bain(lm_fit_Lucas, H1) # does not work
output
#summary(output)

###################################################################################

Bayesian informative hypothesis testing for an object of class lm (ANOVA):
  
  Fit   Com   BF.u  BF.c  PMPa  PMPb 
H1 0.000 0.001 0.000 0.000 0.001 0.000
H2 0.006 0.008 0.709 0.709 0.999 0.415
Hu                               0.585

Hypotheses:
  H1: group1=group2=group3=group4=group5
H2: group3>group1>group4=group5>group2

Note: BF.u denotes the Bayes factor of the hypothesis at hand versus the unconstrained hypothesis Hu. BF.c denotes the Bayes factor of the hypothesis at hand versus its complement.