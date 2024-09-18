
#### Evaluate a set of hypotheses (with GORIC(A)) using benchmarks: ANOVA Example ####

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

## Benchmarks ##
#
#I am not in favor of cut-off points, 
#but when you feel you need them to label the height of the weights and/or their ratios, 
#you can use benchmarks as proposed and discussed in 
#'Guidelines_GORIC-benchmarks' on https://github.com/rebeccakuiper/Tutorials.
#
# Below, you can find some examples.

################################################################################

# Example Palmer & Gough

# Data
PandG_data <- read.table("Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 

# Fit object
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)
#summary(fit.PandG) # NHST

# (Informative) hypothesis
H1 <- 'group1 > group2 > group3' 
# vs complement (default)

# GORIC
set.seed(123) # Set seed value
goric.PandG <- goric(fit.PandG, 
                     hypotheses = list(H1))
goric.PandG
#summary(goric.PandG)

# Benchmarks #
benchmarks.PandG <- benchmark(goric.PandG, model_type = "means", ncpus = 8)
benchmarks.PandG
plot(benchmarks.PandG)


###

# When using the estimates and their covariance matrix and thus gorica:
# use default model_type 
# and possible specify (null) population parameters (instead of effect sizes).
set.seed(123) # Set seed value
est <- coef(fit.PandG)
VCOV <- vcov(fit.PandG)
# GORICA (default)
gorica.PandG <- goric(est, VCOV = VCOV, 
                       hypotheses = list(H1))
# Benchmarks #
benchmarks.PandG_gorica <- benchmark(gorica.PandG, ncpus = 8)
benchmarks.PandG_gorica
plot(benchmarks.PandG_gorica)


#####################


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

# Fit model
lm_fit_Lucas <-  lm(Influence ~ group-1, data = Lucas)

# Check the names used in model
names(coef(lm_fit_Lucas))
# Specify restrictions using those names

# Hypotheses Set
H1 <- 'group5 = group3 > (group1, group4) > group2'
H2 <- 'group3 > group1; group1 > group4; group4 = group5; group5 > group2'

# Calculate GORICA values and weights
set.seed(123) # Set seed value
goric.Lucas <- goric(lm_fit_Lucas, 
                     hypotheses = list(H1 = H1, H2 = H2), 
                     type = "gorica")
goric.Lucas
#summary(goric.Lucas)
goric.Lucas$ratio.gw

# Benchmarks #
benchmarks.Lucas <- benchmark(goric.Lucas, model_type = "means", ncpus = 8)
benchmarks.Lucas
plot(benchmarks.Lucas)

################################################################################

