# Install and load packages

if (!require("bain")) install.packages("bain") # install this package first (once)
library(bain) # for the data 'sesamesim' and for the function bain

if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy

###################################################################################


# Exercise 1a&1b

# Read/Load the Data # 
# It comes from the bain package and is called sesamesim
# I re-name it to data:
data <- sesamesim

# Make the grouping variable site a factor #
data$site <- factor(data$site)  # this command tells R that site is a factor and not a continuous variable

# Inspect the data #
head(data) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy((data$postnumb-data$prenumb), data$site, mat = TRUE)
descrstat



## Compute the GORIC ##

# First, we need the R object with unconstrained estimates 
lm_fit_sesam <-  lm((postnumb-prenumb) ~ site-1, data = data)

# Check the names used in model:
names(coef(lm_fit_sesam))
# Specify restrictions using those names.
#Note: it often comes down to the name and levels of the factor(s).

# Hypothesis/-es
#
# As a reminder, the 5 groups are:
#1.	 disadvantaged inner city,
#2.	 advantaged suburban,
#3.	 advantaged rural,
#4.	 disadvantaged rural,
#5.	 disadvantage Spanish speaking.
#
# Theory-based hypothesis
H1_sesam <- 'site2 > site3 > site4; 
             site2 > (site1, site5)'
# vs its complement (default in case of one hypothesis)


# p-value null hypothesis test(s)
summary(lm_fit_sesam)
# Each mean is significantly different from 0.
# And hypothesis that all means are zero is rejected as well 
# (F-statistic = 46.08 with 5 and 235 df's;  p-value = 2.2e-16 < .001).
# But: We do not know anything yet about the orderings expected beforehand 
#                      as specified in our theories/expectations/hypotheses.


# Calculate GORIC values and weights
#In the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. 
# Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value.
#   If it is sensitive, then increase number of iterations used in calculation of the penalty.
set.seed(123) # Set seed value
goric_sesam <- goric(lm_fit_sesam, 
                hypotheses = list(H1_sesam = H1_sesam))
goric_sesam
#summary(goric_sesam)
#
#The order-restricted hypothesis ‘H1_sesam’ has  more support than its complement.
#
# So, on average, the data thus supports our hypothesis that 
# children from a suburban advantaged background improve more than children from 
# a rural one and that children from an advantaged background improve more than 
# children from a disadvantaged background (for both rural and suburban).

#If needed, you can use `Guidelines_output_GORIC.html’ 
#from https://github.com/rebeccakuiper/Tutorials 
#to interpret the GORIC output.


#In case you want to inspect the order-restricted maximum likelihood estimates (or-mle), inspect:
goric_sesam$ormle

# Note
goric_sesam$ormle
goric_sesam$b.unrestr 
# Our hypothesis is thus in agreement with data;
# as we could also see from the maximum log likelihood values.


#####

# Exercise 2

# Benchmarks GORIC
#
#More information can be found on the github repository 
#https://github.com/rebeccakuiper/Tutorials:  
#- 'Guidelines_GORIC-benchmarks' (html or pdf) and  
#- 'Hands-on_123_Extra_Benchmarks_ANOVA_restriktor.R' (in the Hands-on files folder). 
#
#In the benchmark code below, 
#the null population for which ES = 0, that is, 
#all means are equal is used. 
# Use 'pop_es' to specify own null population(s).	
#  
# Benchmarks (based on GORICA):
benchmarks_sesam <- benchmark(goric_sesam, 
                              ncpus = 8, iter = 2000)
benchmarks_sesam 
plot(benchmarks_sesam, x_lim = c(0, 15))
plot(benchmarks_sesam, log_scale = T)
# Our finding is very extreme under the null:
# Namely, our sample value is higher than the 95th percentile under the null. 
# Hence, there is tremendous support for 'H1_sesam' (cf. 'Guidelines_GORIC-benchmarks').
#
# Inspection of the weights of the preferred hypothesis:
benchmarks_sesam_means$benchmarks_goric_weights
plot(benchmarks_sesam, output_type = "gw")
plot(benchmarks_sesam, output_type = "gw", log_scale = T)


# Extra for those who noticed and want to know :-) #
# Note that the difference in sample value is due to using est&VCOV (gorica) instead of lm object (goric or gorica):
# Technical info:
# This has to do with using a different scaling of the covariance matrix:
# namely, dividing by N (lm object) vs N-k (using vcov() function).
#
set.seed(123) # Set seed value
gorica_sesam <- goric(lm_fit_sesam, 
                      hypotheses = list(H1_sesam = H1_sesam),
                      type = "gorica")
#
set.seed(123) # Set seed value
est <- coef(lm_fit_sesam)
VCOV <- vcov(lm_fit_sesam)
gorica_sesam_est <- goric(est, VCOV = VCOV,
                          hypotheses = list(H1_sesam = H1_sesam),
                          type = "gorica")
gorica_sesam_est
#
# This way it does resemble the output when using the lm object:
set.seed(123) # Set seed value
est <- coef(lm_fit_sesam)
N_min_k <- lm_fit_sesam$df.residual
N <- N_min_k + lm_fit_sesam$rank
VCOV <- vcov(lm_fit_sesam)*N_min_k/N
gorica_sesam_est_Nmink <- goric(est, VCOV = VCOV,
                                hypotheses = list(H1_sesam = H1_sesam),
                                type = "gorica")
#
goric_sesam
gorica_sesam
gorica_sesam_est
gorica_sesam_est_Nmink


###################################################################################

