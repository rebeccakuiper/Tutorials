
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


# Example Palmer & Gough

# Data
PandG_data <- read.table("Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 

# Fit object
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)
#summary(fit.PandG) # NHST

# (Informative) hypothesis
H1 <- 'group1 > group2 > group3' 

# GORIC
set.seed(123) # Set seed value
goric.PandG <- goric(fit.PandG, 
                     hypotheses = list(H1), comparison = "complement")
goric.PandG
#summary(goric.PandG)


## Benchmarks ##
#
#I am not in favor of cut-off points, 
#but when you feel you need them to label the height of the weights and/or their ratios, 
#you can use benchmarks as proposed and discussed in 
#'Guidelines_GORIC-benchmarks' on https://github.com/rebeccakuiper/Tutorials.
#
#
#library(devtools)
#install_github("rebeccakuiper/benchmarks")
library(benchmarks)
#?benchmarks
#
pop.es <- c(0) # c(0, .2, .5)
benchmarks_1c <- benchmarks_ANOVA(goric.PandG, pop.es) # This takes some time
benchmarks_1c$error.prob.pref.hypo
benchmarks_1c$benchmarks.weight
benchmarks_1c$benchmarks.ratios

# Alternatively, one can use the benchmarks() function.
# In that case, one needs to specify the population estimates 
# (instead of population effect size).



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
output_gorica <- goric(lm_fit_Lucas, 
                       hypotheses = list(H1 = H1, H2 = H2), 
                       type = "gorica")
output_gorica
#summary(output_gorica)
output_gorica$ratio.gw

## Benchmarks ##
#
#I am not in favor of cut-off points, 
#but when you feel you need them to label the height of the weights and/or their ratios, 
#you can use benchmarks as proposed and discussed in 
#'Guidelines_GORIC-benchmarks' on https://github.com/rebeccakuiper/Tutorials.
#
#library(devtools)
#install_github("rebeccakuiper/benchmarks")
library(benchmarks)
#?benchmarks
#
pop.es <- c(0) # c(0, .2, .5)
benchmarks_12u <- benchmarks_ANOVA(output_gorica, pop.es, iter = 10)
# Note: I only used 10 iterations here, since the calculation takes quite long 
# because of the not full row-rank hypothesis.
benchmarks_12u$error.prob.pref.hypo
benchmarks_12u$benchmarks.weight
benchmarks_12u$benchmarks.ratios


# Note: When using the estimates and their covariance matrix
# in which case you also need to state the group sample size (N).
set.seed(123) # Set seed value
est <- coef(lm_fit_Lucas)
VCOV <- vcov(lm_fit_Lucas)
output_gorica <- goric(est, VCOV = VCOV, 
                         hypotheses = list(H1, H2), 
                         type = "gorica")
pop.es <- c(0) # c(0, .2, .5)
benchmarks_12u <- benchmarks_ANOVA(output_gorica, N = descrstat$n, pop.es, iter = 10)
# Could also use N = 30 or N = rep(30,5)
# Note: I only used 10 iterations here, since the calculation takes quite long 
# because of the not full row-rank hypothesis.
benchmarks_12u$error.prob.pref.hypo
benchmarks_12u$benchmarks.weight
benchmarks_12u$benchmarks.ratios

# Alternatively, one can use the benchmarks() function.
# In that case, one needs to specify the population estimates 
# (instead of population effect size).


################################################################################

