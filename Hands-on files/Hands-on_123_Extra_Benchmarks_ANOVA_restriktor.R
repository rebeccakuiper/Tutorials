
#### Evaluate a set of hypotheses (with GORIC(A)) using benchmarks: ANOVA Example ####

# Load libraries. 
# These contain functions such as 'goric' that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.

## First, install the packages, if you have not done this already:
if (!require("psych")) install.packages("psych")
#if (!require("restriktor")) install.packages("restriktor")

## Then, load the packages:
library(psych) # for the function describeBy
#library(restriktor) # for the goric function

# If you want to use restriktor from github:
if (!require("devtools")) install.packages("devtools")
library(devtools) 
install_github("LeonardV/restriktor")
library(restriktor) # for goric function


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
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
goric.PandG <- goric(fit.PandG, 
                     hypotheses = list(H1))
goric.PandG
#summary(goric.PandG)

# Benchmarks #
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
iter <- 500
benchmarks.PandG <- benchmark(goric.PandG, model_type = "means", 
                              iter = iter, ncpus = 8)
benchmarks.PandG
plot(benchmarks.PandG)
plot(benchmarks.PandG, log_scale = T)


###

# When using the estimates and their covariance matrix and thus gorica:
# use default model_type 
# and possible specify (null) population parameters (instead of effect sizes).
est <- coef(fit.PandG)
VCOV <- vcov(fit.PandG)
# GORICA (default)
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
gorica.PandG <- goric(est, VCOV = VCOV, 
                       hypotheses = list(H1))
# Benchmarks #
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
iter <- 500
benchmarks.PandG_gorica <- benchmark(gorica.PandG, 
                                     iter = iter, ncpus = 8)
benchmarks.PandG_gorica
plot(benchmarks.PandG_gorica)
plot(benchmarks.PandG_gorica, log_scale = T)


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
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
goric.Lucas <- goric(lm_fit_Lucas, 
                     hypotheses = list(H1 = H1, H2 = H2), 
                     type = "gorica")
goric.Lucas
#summary(goric.Lucas)
goric.Lucas$ratio.gw

# Benchmarks #
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
iter <- 500
benchmarks.Lucas <- benchmark(goric.Lucas, model_type = "means", 
                              iter = iter, ncpus = 8)
benchmarks.Lucas
plot(benchmarks.Lucas)
plot(benchmarks.Lucas, log_scale = T)


####

# Let us know look at the following hypothesis:
H1 <- 'group5 > group3 > (group1, group4) > group2'
# vs its complement 

set.seed(123) # Set seed value
goric.Lucas_c <- goric(lm_fit_Lucas, 
                  hypotheses = list(H_theory1 = H1), 
                  mix_weights = "boot")
goric.Lucas_c
#
#Note: The log-likelihood (loglik) weights / fit values seem to be quite close. 
#This could then indicate that one or more of the inequality constraints 
#can be replaced by (about-)equality constraints. 
#Notably, one could investigate with the benchmarks function, 
#using 'output_type = "rlw"', whether the loglik weights indeed are close.  
#For more information, see the guidelines ('Guidelines_output_GORIC.html') 
# and/or the benchmark tutorial on https://github.com/rebeccakuiper/Tutorials. 

# Benchmarks #
#
# Note that running this takes quite some time 
# (even longer when you use the default number of iterations).
#
#
# Using default setting (i.e., using default null population):
# This only inspects the situation where all means are equal.
#
#set.seed(123) # Set seed value, for reproducibility and option for sensitive check
#iter <- 500
#benchmarks.Lucas <- benchmark(goric.Lucas_c, 
#                              model_type = "means", 
#                              iter = iter, ncpus = 8)
#print(benchmarks.Lucas, output_type = "rlw")
#plot(benchmarks.Lucas, output_type = "rlw", log_scale = T)
#
# 'Observed' seems to describe best, the null does not seem to be likely.
#
# Notably, only the situation where all means are equal is inspected.
# In case of equal fit values, it makes more sense to look at several possible
# boundary hypotheses, that is, several possible null populations.
#
#
# Using alternative setting: Specifying several null populations yourself:
#
# Specify several null populations
est <- coef(lm_fit_Lucas)
pop_est <- matrix(c(
  # all means equal
  rep(mean(est), 5), 
  # means 1,3,4,5 equal
  est[1], est[2], mean(est[c(1,3,4,5)]), mean(est[c(1,3,4,5)]), mean(est[c(1,3,4,5)]),
  # means 3,5 equal
  est[1], est[2], mean(est[c(3,5)]), est[4], mean(est[c(3,5)]),
  # all means equal to sample means, so Observed
  est[1], est[2], est[3], est[4], est[5]
),
byrow = TRUE, ncol = length(est))
rownames(pop_est) <- c("PE_12345eq", "PE_1345eq", "PE_35eq", "Observed")
#
# Run benchmark function with the specified null populations:
set.seed(123) # Set seed value, for reproducibility and option for sensitive check
iter <- 500
# As a reminder: running the next code will take some time.
benchmarks.Lucas <- benchmark(goric.Lucas_c, pop_est = pop_est, 
                              iter = iter, ncpus = 8)
print(benchmarks.Lucas, output_type = "rlw") # see below
plot(benchmarks.Lucas, output_type = "rlw", log_scale = T)
#
# 'PE_35eq' and 'Observed' seems to describe/fit our finding the best.
# The distributions for these two populations overlap; which makes sense
# since the mean values in both resemble a lot.
# I would conclude that the means of group 5 and 3 are equal, that is,
# I would advise to evaluate the following hypothesis in a future study:
# H1 <- 'group5 = group3 > (group1, group4) > group2'.
# One could also argue that we have a too low sample size to find a difference.
#
# In case you can specify a range for when the means are said to be the same, 
# then use that in your (future) hypothesis; e.g.:
# H1 <- '0.05 < (group5 - group3) < 0.05, group3 > (group1, group4) > group2',
# where '0.05 < (group5 - group3) < 0.05' is an about-equality restriction.
#
#
# Results of benchmarks.Lucas based on the default number of iterations:
# print(benchmarks.Lucas, output_type = "rlw")
Benchmark Results 
---------------------------------------------------------------------- 
  Preferred Hypothesis: H_theory1
Error probability Preferred Hypothesis vs. Complement: 0.097
Sample Size: 150
Number of Parameters: 5
Population Estimates (PE):
  group1  group2  group3  group4  group5
PE_12345eq   2.463   2.463   2.463   2.463   2.463
PE_1345eq    2.329   1.328   2.747   2.747   2.747
PE_35eq      2.329   1.328   3.215   2.231   3.215
Observed     2.329   1.328   3.200   2.231   3.229


================================================================================================================== 
  Benchmark: Percentiles of Ratio-of-log-likelihood-weights for the Preferred Hypothesis 'H_theory1' 
------------------------------------------------------------------------------------------------------------------ 
  Population estimates = PE_12345eq
Sample 5%    35%   50%   65%   95%  
H_theory1 vs. complement 1.003  0.023 0.251 0.395 0.568 0.967

Population estimates = PE_1345eq
Sample 5%    35%   50%   65%   95%  
H_theory1 vs. complement 1.003  0.098 0.579 0.751 0.905 1.058

Population estimates = PE_35eq
Sample 5%    35%   50%   65%   95%  
H_theory1 vs. complement 1.003  0.264 0.919 1.000 1.043 1.942

Population estimates = Observed
Sample 5%    35%   50%   65%   95%  
H_theory1 vs. complement 1.003  0.281 0.938 1.000 1.057 1.958


################################################################################

