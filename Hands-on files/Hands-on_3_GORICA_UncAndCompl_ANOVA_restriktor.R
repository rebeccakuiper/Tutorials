
#### Evaluate a set of hypotheses vs Hunc and H1 vs its complement with GORICA: ANOVA Example ####

# Load libraries. These libraries contain functions such as 'goric' that will be used in this R code. 
# Each time you reopen this R file you have to execute this step.
#
if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy
#
# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function
# If from CRAN:
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

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



## Compute the GORIC ##

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
# It is possible to use the following operators: `>`, `<`, `=`, `== `,`<=`or`>=` within the `restriktor()` and `goric()` functions. 
#   *  `==` operator is interpreted in the same fashion as the `=`, meaning an equality  
#   *  `<=` and `>=` operators are interpreted as respectively: `<` and `>` by the code
#-----------------------------------------------------------------------------------------------------------
# The `goric()` and the `restriktor()` functions can deal with:
#   *   pairwise restrictions (e.g. "x1>x2;x2==x3" also equivalent to *"x1>x2;x2=x3"*)
#   *   combined with more than one operators restrictions(e.g. *"x1>x2==x3"* also equivalent to *"x1>x2=x3"*)
# It is important to remember that all restrictions within one hypothesis has to be separated with a semicolon `;`.
#
H0 <- 'group1 == group2 == group3 == group4 == group5' 
H1 <- 'group5 == group3 > group1 > group2; group3 > group4 > group2'
H2 <- 'group3 > group1 > group4 == group5 > group2'

# Calculate GORICA values and weights
# To apply the GORICA instead of the GORIC (default), one should use 'type = "gorica"'.
#
#Like in the GORIC, in the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value 
#   If it is sensitive, then increase number of iterations used in calculation of the penalty (see below).


# A set of hypotheses vs Hunc
#
set.seed(123) # Set seed value
output_gorica <- goric(lm_fit_Lucas, H0, H1, H2, type = "gorica")
summary(output_gorica)
# If you did the same analysis with the GORIC, you will see that the (relative) weights are (about) the same for the GORIC and GORICA.
#
# From this output, it can be seen that the order-restricted hypothesis $H_1$ has 16.5 times more support than $H_u$ (the unconstrained hypothesis). 
# Hence, $H_1$ is not a weak hypotheses and can be compared to the other (weak and non-weak) competing hypotheses: 
# $H_1$ is much more (1.13e+06 times more) supported than $H_0$ and 37.2 times more likely than $H_2$.


## In case of one informative hypothesis (H1) ##
# H1 vs Hunc 
#
#If you have only one informative hypothesis ($H_1$), then evaluate this against its complement (i.e., all other theories, thus excusing the one of interest).
H1 <- 'group5 == group3 > group1 > group2; group3 > group4 > group2'
set.seed(123) # Set seed value
output_gorica_c <- goric(lm_fit_Lucas, H1, comparison = "complement", type = "gorica")
summary(output_gorica_c)
# The order-restricted hypothesis $H_1$ has 13.4 times more support than its complement (and the weights are once again the same as compared to the GORIC).


###################################################################################


restriktor: generalized order-restriced information criterion approximation:
  
  Results:
  model  loglik  penalty  gorica  gorica.weights
1          H1   1.647    2.195   1.096           0.931
2  complement   1.650    4.795   6.291           0.069
---
  
  Relative GORICA-weights:
  vs. H1  vs. complement
H1          1.000   13.432        
complement  0.074   1.000         
---
  The order-restricted hypothesis ‘H1’ has 13.432 times more support than its complement.

Restriktor message: Since the constraint matrix is not full row-rank, the level probabilities 
are calculated using mix.weights = "boot" (the default is mix.weights = "pmvnorm").
For more information see ?restriktor.
