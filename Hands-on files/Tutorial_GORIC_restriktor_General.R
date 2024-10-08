
#### Tutorial for goric(): 
# How to evaluate theory-based hypotheses using the GORIC and GORICA


# Below you find examples for the goric function in the restriktor package.


# Two types of analyses can be run: 
# 1. type = "goric" (the default for lm objects), 
#                     which can be applied to multivariate normal linear models; 
# 2. type = "gorica" (the default for all other input options), 
#the approximation of the GORIC which can be applied to a broad range of models.
#
# Both balance fit (log likelihood) and complexity (penalty, that is, PT).
#
# The PT is calculated using sampling and thus one need to set a seed value:
# 1) Every time same PT value; 
# 2) Change it to check sensitivity of PT value 
#  (if sensitive, then increase number of iterations used in calculation of PT).


# There is also an option 'comparison'.
# There are three types of comparisons: 
# a. none, 
# b. unconstrained (default in case of multiple hypotheses), and 
# c. complement  (default in case of one hypothesis).
#
# If "none", then only hypotheses of interest are inspected. 
# This can lead to choosing the best out of a set of weak hypotheses,
# that is, hypotheses not supported by the data.
# Therefore, this is only recommended when the hypotheses of interest cover 
# the full parameter space / cover the whole set of possible theories.
#
# If "unconstrained", then the unconstrained / unrestricted / classical 
# alternative hypothesis is included in the set. 
# This safeguards from choosing a weak hypothesis as the best one.
# If at least one of the theory-based / informative hypotheses is not weak,
# one can compare that/those to all other hypotheses in the set.
#
# The "complement" of the hypothesis of interest acts like a competing hypothesis. 
# Since there is per definition no overlap between the hypothesis and its 
# complement, this is more powerful then including the unconstrained hypothesis.
# 
# Note that, currently, "complement" only works for one hypothesis and not a whole set. 
# However, the use of the complement of the set would not change the relative 
# support between two competing theories. 
# Hence, one can just use the unconstrained (as a failsafe then).
#
# At the end, there are three examples for the three types of comparisons.


#The `goric` function of `restriktor` takes different forms of input.  
#For model parameter estimates:  
#* enter fitted unconstrained object (lm, glm, glmer, nlmer, lmer, or lavaan object);  
#* enter the (structural) parameter estimates and their covariance matrix yourself.  
#For constraints:  
#* enter character constraints;  
#* enter list with constraints matrix.
#
#Please note that:  
#* The GORIC can only be calculated for lm objects.  
#* The GORIC (type = 'goric') is the default for lm objects, while GORICA (type = 'gorica') is for the other input options.  
#* In the GORICA, the estimates are assumed to be normally distributed. So, for some models when sample size is low, this assumption may not hold. In that case, it is often not clear how well the GORICA performs. See Altinisik et al 2021, for some simulations regarding logistic regression and SEM models (for which the GORICA performs well).
#
# Below you find examples


# Hypothesis specification 
#
# When 'character constraints':
# - One can use '>', '<' and '=', but also '>=', '<=', and '=='.
# - One can specify the restrictions 
#  * all at once (e.g., beta1 > beta2 < beta3) or 
#  * in a pairwise manner separated by ';' or '&' 
#    (e.g., beta1 > beta2; beta2 < beta3).
# - Note that one should use the labels of the parameter estimates.
# - One can also define terms that are a linear function of parameters and 
# specify hypotheses on those (see 'Extra possibility specification hypotheses' 
# near the end of this document).
# - One can use 'abs()' in case one wants to specify an absolute strength; 
#   e.g., abs(beta1) > abs(beta2); abs(beta2) > abs(beta3).
#
# When 'list with constraints matrix'
# One should now specify "list(constraints = xxx, rhs = xxx, neq = x)", with 
# 'constraints' a matrix, 
# 'rhs' a vector (when not zero), and 
# 'neq' a number (when not zero), 
# for which then holds: constraints * parameters >= rhs, 
# where the first neq restrictions are equalities.
#
# See also the example below.


# In the examples below, 'full output' will be shown.
# At the end, in the last example for the three types of comparisons, 
# it will be shown how one can ask for specific output parts.


################################################################################

## First, install the package, if you have not done this already:
if (!require("restriktor")) install.packages("restriktor")

## Then, load the package:
library(restriktor) # for goric function

# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function

#Note that one needs to install the package only once, 
#but has to load the package each time the R script is run.

##

# Generate example data
#
set.seed(123) # Set seed value to obtain the same data every time.
#
n <- 10
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 1 + x1 + x2 + x3 + rnorm(n)
#
# Example for reading in data (with header) from the text file 'Data_Lucas.txt'. 
# y <- read.table("Data_Lucas.txt", header=TRUE)

##

# Fit regression model
fit.lm <- lm(y ~ x1 + x2 + x3)


######


# Specify hypotheses and apply GORIC(A)


# A. lm object + use type = "goric" or "gorica":


#1.	Fitted unconstrained lm object + character constraints 
H1 <- "x1 > 0"
H2 <- "x1 > 0; x2 > 0" 
# Note: If estimates of continuous predictor variables are compared 
# (e.g., x1 < x2), then one should standardize the data; see below.
#
# GORIC (default)
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2)) 
# or: out <- goric(fit.lm, H1, H2, type = "goric")
out
#summary(out) 
#
# GORICA
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2), 
             type = "gorica")
out
#summary(out)


#2a.	Fitted unconstrained lm object + list with constraints matrix
# Notes: 
# - When there is an intercept, this is part of constraint matrix as well.
# - equality restrictions should be stated first.
H1 <- list(constraints = c(0,1,0,0)) # "x1 > 0"
H2 <- list(constraints = rbind(c(0,1,0,0), c(0,0,1,0))) #"x1 > 0; x2 > 0"
# Btw list(constraints = c(0,-1,1,0)) reflects x1 < x2
# Note: now, rhs and neq (number of equalities) are assumed to be 0.
#
# GORIC (default)
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2))
out
#summary(out)
#
# GORICA
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2), 
             type = "gorica")
out
#summary(out)


#2b.	Fitted unconstrained lm object + list with constraints matrix
# Notes: 
# - When there is an intercept, this is part of constraint matrix as well.
# - equality restrictions should be stated first.
H1 <- list(constraints = c(0,1,0,0), rhs = 2) # "x1 > 2"
H2 <- list(constraints = rbind(c(0,1,0,0), c(0,0,1,0)), rhs = c(1, 0), neq = 1)
# "x1 = 1; x2 > 0"
#
# GORIC (default)
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2))
out
#summary(out)
#
# GORICA
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2), 
             type = "gorica")
out
#summary(out)



# B. parameter estimates and their covariance matrix (then, type = "gorica"):
# Note: This can be especially helpful for not-lm objects.


#3a.	Numeric vector + character constraints
est <- coef(fit.lm) 
VCOV <- vcov(fit.lm)
H1 <- "x1 > 0"
H2 <- "x1 > 0; x2 > 0"
# GORICA (default)
set.seed(123) # Set seed value
out <- goric(est, VCOV = VCOV, 
             hypotheses = list(H1, H2))
out
#summary(out)
#
# one could also use only the structural parameters, that is, 
# the ones used in all the hypotheses in the set:
H1 <- "x1 > 0"
H2 <- "x1 > 0; x2 > 0"
est <- coef(fit.lm)[2:3] # these are the structural parameters, 
#                          since only 'x1' and 'x2' are part of the hypotheses.
VCOV <- vcov(fit.lm)[2:3,2:3] # now, you also need this submatrix to obtain the 
#                               corresponding covariance matrix
# GORICA (default)
set.seed(123) # Set seed value
out <- goric(est, VCOV = VCOV, 
             hypotheses = list(H1, H2))
out
#summary(out)
# Now, the exact same weights are obtained
# The penalties are now 2 points lower 
#                       (because 2 unconstrained/free parameters are left out).
# The log likelihoods also change. 
# But the differences in those between the hypotheses remain the same.


#3b.	Relabeled numeric vector + character constraints
est <- coef(fit.lm)
names(est) <- c("beta1", "beta2", "beta3", "beta4") # You can rename parameters
VCOV <- vcov(fit.lm)
H1 <- "beta1 > 0" # use same names/labels as the ones of the estimates
H2 <- "beta1 > 0; beta2 > 0"
# GORICA (default)
set.seed(123) # Set seed value
out <- goric(est, VCOV = VCOV, 
             hypotheses = list(H1, H2))
out
#summary(out)


#3c.	'Manual' numeric vector + character constraints
# If you obtain the estimates via another program (or from an article or so):
est <- as.vector(c(0.9867181,   1.1321274,   0.7792158,   0.8933787)) 
names(est) <- c("beta1", "beta2", "beta3", "beta4") # Label estimates
VCOV <- matrix(c(0.143354252,  0.031582705,  0.049881853,  0.006769526,
                 0.031582705,  0.10707347,  -0.034664495, -0.033094646,
                 0.049881853, -0.034664495,  0.146597544,  0.008185565,
                 0.006769526, -0.033094646,  0.008185565,  0.116578062), 
               byrow = T, ncol = 4)
H1 <- "beta1 > 0" # use same names/labels as the ones of the estimates
H2 <- "beta1 > 0; beta2 > 0"
# GORICA (default)
set.seed(123) # Set seed value
out <- goric(est, VCOV = VCOV, 
             hypotheses = list(H1, H2))
out
#summary(out)


#4.	Numeric vector + list with constraints matrix
est <- coef(fit.lm)
VCOV <- vcov(fit.lm)
H1 <- list(constraints = c(0,1,0,0))
H2 <- list(constraints = rbind(c(0,1,0,0), c(0,0,1,0)))
# GORICA (default)
set.seed(123) # Set seed value
out <- goric(est, VCOV = VCOV, 
             hypotheses = list(H1, H2))
out
#summary(out)


# Standardized data
#
## Note: If estimates of continuous predictor variables are compared 
# (e.g., x1 < x2), then one should standardize the data.
#
# Generate data
# To demonstrate this, new data will be generated, where the variables in the 
# data are going to differ in their contribution to the dependent variable. 
# Below, the vector of ratios is to c(1,1.5,2), which then reflects the 
# population regression coefficient values. 

# To demonstrate this, new data will be generated. In the data, the contribution 
# of the variables to the dependent variable will differ. Below, the vector of 
# ratios of these contributions is set to c(1,1.5,2). This, then reflects 
# (the ratios of) the population regression coefficient values. It means that 
# the contribution to the outcome variable y, increases from x1 to x3, where 
# that of x2 is 1.5 and that of x3 is 2 times as large as that of x1.
ratio <- c(1, 1.5, 2)
n <- 30
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
data <- cbind(x1, x2, x3)
data_s <- as.data.frame(scale(data))
y <- ratio[1]*data_s$x1 + ratio[2]*data_s$x2 + ratio[3]*data_s$x3 + rnorm(n)
# Note: since there is one outcome, the outcome does not need to be standardized
#
# Fit regression model
fit.lm_s <- lm(y ~ -1 + x1 + x2 + x3, data = data_s)
#
#1.	Fitted unconstrained lm object + character constraints 
H1 <- "x1 < x2" 
# vs its complement (default in case of one hypothesis)
#
# # GORIC (default)
set.seed(123) # Set seed value
out_s <- goric(fit.lm_s, hypotheses = list(H1))
out_s
#summary(out_s)


######

# Extra possibility specification hypotheses

#In restriktor, it also possible to specify hypotheses in terms of 
# linear functions of the parameters.
#For instance, in case of adjusted means, you can do that in two ways:
#1) You can write them out yourself 
#   (using the parameter/variable names from an R object, like lm()).
#2) You can first specify the adjusted means (below called m1 to m4) and specify 
#   the hypotheses in terms of those:
#H1 <- '
#m1 := .Intercept.
#m2 := .Intercept. + facialBurns
#m3 := .Intercept. + gender
#m4 := .Intercept. + facialBurns + gender + gender.facialBurns
#m1 < m4
#m2 < m4
#m3 < m4 '
## For more detail, see Vanbrabant, Van Loey, and Kuiper (2020).
# Note: This code cannot be run now, since the variables and, more importantly, 
# the parameters are labeled differently. One can run:
H1 <- "
m1 := .Intercept.
m2 := .Intercept. + x1
m3 := .Intercept. + x2
m4 := .Intercept. + x3
m1 < m4
m2 < m4
m3 < m4
"
#
# # GORIC (default)
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1)) 
out
#summary(out)


######


# There are three types of comparisons: none, unconstrained, and complement.
# Subsequently, it is shown, for the last type, what output can be asked for, 
# which of course holds for all examples.

# 1) If "none", then only hypotheses of interest are inspected. 
#    This can lead to choosing the best out of a set of weak hypotheses.
#    Hence, one should only use this if the hypotheses cover the whole parameter
#    space, that is, cover all possible theories/hypotheses.
H1 <- "x1 > 0"
H2 <- "x1 > 0; x2 > 0"
# GORIC
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2), comparison = "none")
out
#summary(out)

# 2) If "unconstrained", then the unconstrained / unrestricted / classical 
#    alternative hypothesis is included in the set. 
#    This safeguards from choosing a weak hypothesis as the best one.
H1 <- "x1 > 0"
H2 <- "x1 > 0; x2 > 0"
# Default in case of multiple hypotheses: comparison = "unconstrained"
# GORIC
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1, H2)) 
out
#summary(out)
out$ratio.gw
# Since at least one of, even both, H1 and H2 are not weak, one can compare
# the support for H1 versus H2 (where H2 is 1.9 times more supported).

# 3) The "complement" of the hypothesis of interest acts like a competing hypothesis. 
#    Since there is per definition no overlap between the hypothesis and its 
#    complement, this is more powerful then including the unconstrained.
H1 <- "x1 > 0"
# Default in case of one hypothesis: comparison = "complement"
# GORIC
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H1))
out
summary(out)
summary(out, brief = FALSE)
#
H2 <- "x1 > 0; x2 > 0"
# Default in case of one hypothesis: comparison = "complement"
# GORIC
set.seed(123) # Set seed value
out <- goric(fit.lm, hypotheses = list(H2))
out
#summary(out)
# Note that the obtained weights for the two hypotheses cannot be compared. 
# Then, one should evaluate them simultaneously in one set, as done above.


# Output
#
out                         
# gives Results (i.e., per model/hypothesis the log likelihood, 
# penalty, goric(a), and goric(a) weights).
summary(out)                
# gives besides Results also the Relative GORIC(A)-weights matrix.
summary(out, brief = FALSE) 
# gives also the order-/inequality-restricted coefficients 
# (order-restricted mle's = ormle's) and constraint matrices.
#
# Ask for different elements of the output by:
out$result      # gives Results 
                # (i.e., per model/hypothesis the log likelihood, penalty, 
                # goric(a), and goric(a) weights).
out$ratio.gw    # gives the relative GORIC(A)-weights matrix, which gives 
                # the relative support of a hypothesis versus another.
out$ratio.lw    # gives the relative log-likelihood weights matrix.
out$ratio.gw    # gives the relative penalty weights matrix.
out$ormle       # gives the order-/inequality-restricted coefficients (ormle's).
coef(out)       # also gives the order-/inequality-restricted coefficients.
out$b.unrestr   # gives the unrestricted/unconstrained mle's.
out$type        # states what type of analysis is used (goric or gorica).
out$comparison  # states what type of comparison is used 
                # (none, unconstrained, complement).
out$constraints # gives the constraint matrices.
out$rhs         # gives the right hand side (rhs) of the constraints.
out$neq         # gives the number of equalities (neq) in the constraints, 
                # where the first neq are then equalities.
out$hypotheses_usr # gives the hypotheses that are inserted by the user as input.


################################################################################


## Notes ## 


# Note 1 (general for goric and gorica, when using the restriktor package) #

# If the restriction matrix is not of full row-rank, 
# this means one of the following:
# a) There is at least one redundant restriction.
#    Then, either 
#    a.1) leave the redundant one out or 
#    a.2) use another (more time-consuming) way of obtaining the 
#         level probabilities for the penalty term 
#         (goric function does this by default) - see below.
# b) There is at least one range restriction (e.g., -2 < group1 < 2). 
#    Such a restriction can be evaluated but there is a sensitivity 
#    (of a scaling factor in the covariance matrix, like with a prior in a Bayes 
#    factor) which currently cannot be checked for.
# c) There is at least one conflicting restriction (e.g., 2 < group1 < -2). 
#    Such a restriction can evidently never hold and is thus impossible to 
#    evaluate (delete the one that is incorrect and apply the goric() again).

# Ad a.2) and b)
# There are two methods that can be used in calculating the penalty, or better, 
# in obtaining the level probabilities (LPs) which are used in the calculation 
# of the penalty term:
# 1. default method using the multivariate normal distribution.
# 2. bootstrap method.
#
# Ad 1. The default method uses the multivariate normal distribution to obtain 
# the LPs and is often much faster (if number of parameters is smaller than 10) 
# and needs less input specification. 
# It can, however, not deal with hypotheses that are not of full row-rank 
# (like $H_1$ above). 
# In that case, `restriktor` uses automatically the other (bootstrap) method. 
# Ad 2. The bootstrap method uses bootstrapping to calculate the PT.
# For additional details, see '?goric' or the Vignette.


# Note 2 (w.r.t. evaluation using the complement) #

# Using the complement does not per se render a higher goric weight for Hm than 
# when Hu was used. When Hm is not completely in agreement with the data, using 
# Hu may overstate the support for H1. So, whether H1 is correct or not, 
# evaluating it against its complement is better than against Hu.


# Note 3 (w.r.t. complement in case of two hypotheses of interest) #

# One cannot compare the support of hypotheses versus their complements,
# one should evaluate them simultaneously in one set (like done above):
#
# Calculate goric for H1 and its complement
#set.seed(123) # Set seed value
#output_c_H1 <- goric(lm_fit_Lucas, hypotheses = list(H1)) # default: complement
#output_c_H1
##summary(output_c_H1)
#
# Calculate goric for H2 and its complement
#set.seed(123) # Set seed value
#output_c_H2 <- goric(lm_fit_Lucas, hypotheses = list(H2)) # default: complement
#output_c_H2
##summary(output_c_H2)
#
# Calculate goric for H1 and H2 (and Hu):
#set.seed(123) # Set seed value
#output_H1H2 <- goric(lm_fit_Lucas, hypotheses = list(H1, H2)) # default: unconstrained
#output_H1H2
##summary(output_H1H2)
#output_H1H2$ratio.gw
#
# H1 has 38.5 times more support than H2.
# This is not equal to:
#output_c_H1$relative.gw[1,2] / output_c_H2$relative.gw[1,2] 
# 36
#
#
# Notably, you could derive the support from H1 vs H2 from 
# their support versus that of Hu:
#set.seed(123) # Set seed value
#output_u_H1 <- goric(lm_fit_Lucas, hypotheses = list(H1), comparison = "unconstrained")
#output_u_H1
##summary(output_u_H1)
#
#set.seed(123) # Set seed value
#output_u_H2 <- goric(lm_fit_Lucas, hypotheses = list(H2), comparison = "unconstrained")
#output_u_H2
##summary(output_u_H2)
#
#output_u_H1$relative.gw[1,2] / output_u_H2$relative.gw[1,2] 
# approx (0.943/0.057) / (0.3/0.7) = 16.481 / 0.428
# 38.49661
# which (approximately) equals:
#output_H1H2$relative.gw[1,2]
# 38.49665


# Note 4 (w.r.t. GORIC weights versus GORICA weights) #

# The GORICA weights (asymptotically) equal the GORIC weights. 
# The differences are minor and often not notable with 2 decimals.
# Because of these minor differences, the relative weights 
# (i.e., ratio of weights) can differ.
# These differences in relative weights can even be large, when dividing a very 
# large number by a very small number with minor differences in these values.


################################################################################
