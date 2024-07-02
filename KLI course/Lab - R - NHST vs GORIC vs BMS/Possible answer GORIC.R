if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy

if (!require("bain")) install.packages("bain") # install this package first (once)
library(bain) # for bain function

if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

###################################################################################


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

# Hypotheses Set
#
# As a reminder, the 5 groups are:
#1.	 disadvantaged inner city,
#2.	 advantaged suburban,
#3.	 advantaged rural,
#4.	 disadvantaged rural,
#5.	 disadvantage Spanish speaking.

H12_sesam <- "site2>site3>site1>site4, site5>site4" # specify the hypothesis of interest
# or: H12_sesam <- "site2>site3>site1>site4 & site5>site4"


# p-value null hypothesis test(s)
summary(lm_fit_sesam)
# Each mean is significantly different from 0.
# And hypothesis that all means are zero is rejected as well 
# (F-statistic: 46.08 on 5 and 235 DF,  p-value: < 2.2e-16).
# But: We do not know anything yet about the orderings expected on forehand 
#                      as specified in our theories/expectations/hypotheses.


# Calculate GORIC values and weights
#In the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. 
# Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value.
#   If it is sensitive, then increase number of iterations used in calculation of the penalty.
set.seed(123) # Set seed value
output <- goric(lm_fit_sesam, 
                hypotheses = list(H12_sesam = H12_sesam), 
                comparison = 'complement')
output
#summary(output)
#
#The order-restricted hypothesis ‘H12_sesam’ has  6.5 times more support than its complement.

#In case you want to inspect the order-restricted maximum likelihood estimates (or-mle), inspect:
output$ormle

# Note
output$ormle
output$b.unrestr 
# complement is in agreement with data;
# as we could also see from the maximum log likelihood values.



#####

# Benchmarks GORIC

# Install and load benchmark function
if (!require("devtools")) install.packages("devtools")
library(devtools)
install_github("rebeccakuiper/benchmarks")
library(benchmarks)
# For more information regarding the included functions:
#?benchmarks
#?benchmarks_ANOVA

#In the benchmark code below, 
#the null population for which all means are equal is used. 
#If you rather check for another null using say a small effect size, you can do that. 
#You can either use a specific ANOVA effect size (pop.es) in the benchmarks_ANOVA function 
#or specify a set of population estimations (pop.est) in the benchmarks function. 
#When using the null where all means are equal:
  
# Example using the benchmarks_ANOVA function:
pop.es <- c(0)
benchmarks_1c <- benchmarks_ANOVA(output, pop.es)
benchmarks_1c$error.prob.pref.hypo
# There is a 13.4% change that H12_sesam is not the correct hypothesis.
benchmarks_1c$benchmarks.weight
benchmarks_1c$benchmarks.ratios
# Our finding is quite extreme under the null (in between 65 and 95%).
# Hence, there is high (compelling) support.


###################################################################################

