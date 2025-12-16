#Causal dominance hypothesis evaluation
#When the interest lies in evaluating a theory-based, informative hypothesis, 
#one can use the AIC-type criterion called GORICA (Altinisik et al., 2021). 
#One can then evaluate one or more hypotheses reflecting a priori theories, 
#which then often contain orderings of (standardized) parameters. 
#For example, one may hypothesize that a cross-lagged relationship is 
#higher than another one; often referred to as ‘causal dominance’. 
#Such a causal dominance hypothesis can be evaluated with the GORICA (Sukpan and Kuiper, 2024).
#
#Below you find some code to evaluate a causal dominance hypothesis using the GORICA. 
#
#Additionally, html tutorials and R scripts for evaluating informative hypotheses 
#using the GORIC(A) in R can be found on https://github.com/rebeccakuiper/Tutorials.
#
#
#
# references
#
#Altinisik, Y., van Lissa, C., Hoijtink, H., Oldehinkel, A. J., & Kuiper, R. M. (2021). 
#Evaluation of inequality constrained hypotheses using a generalization of the AIC. 
#Psychological Methods, 26(5), 599-621. 
#https://doi.org/10.1037/met0000406
#
#Sukpan, C., & Kuiper, R. M. (2024). 
#How to Evaluate Causal Dominance Hypotheses in Lagged Effects Models. 
#Structural Equation Modeling, 31(3), 404-419. 
#https://doi.org/10.1080/10705511.2023.2265065


# How to evaluate causal dominance in lagged effects models
# R code to run a `wave-specific' parameters model
# A bivariate RI-CLPM with 2 variables and 5 time points

# Using extracted standardized estimates and their covariance matrix as input

library(lavaan)
library(restriktor)

# Load the data set into R
dat <- read.table("RICLPM.dat", 
                  col.names = c(
                    "x1", "x2", "x3", "x4", "x5", 
                    "y1", "y2", "y3", "y4", "y5")
)

# Hypothesis w.r.t. random intercept variances
H_RIvar <- "varRIx > 0 & varRIy > 0" 
# versus it complement, that is, versus all other possibilities
# default in case of one hypothesis
#
# Hypothesis w.r.t. wave-specific cross-lagged effects
H_dominance.ws <- "abs(beta2) < abs(gamma2); abs(beta3) < abs(gamma3); 
         abs(beta4) < abs(gamma4); abs(beta5) < abs(gamma5)" 
# versus it complement, that is, versus all other possibilities 
# default in case of one hypothesis

# Fitting a RI-CLPM; here, a bivariate RI-CLPM with wave-specific parameters:
RICLPM_ws <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  #
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  #
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5

  # Estimate lagged effects between within-person centered variables
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3
  wx5 + wy5 ~ wx4 + wy4
  

  # Estimate covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  
  # Estimate variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
  wx4 ~~ wx4 
  wy4 ~~ wy4 
  wx5 ~~ wx5
  wy5 ~~ wy5
'
RICLPM_ws.fit <- lavaan(RICLPM_ws,
                     data = dat, 
                     missing = "ML", 
                     meanstructure = T, 
                     int.ov.free = T
)
summary(RICLPM_ws.fit, standardized = T)




# One could label the parameters, similarly to example with constrained parameters, 
# but then using unique names.
# Alternatively, one can extract the parameters of interest:
#
#
# Extract the (unstandardized) RI variance estimates 
# and their covariance matrix:
est_RIvar <- coef(RICLPM_ws.fit)[c(22,23)]
names(est_RIvar) <- c("varRIx", "varRIy")
vcov_RIvar <- vcov(RICLPM_ws.fit)[c(22,23), c(22,23)]
#
#
# Extract the standardized cross-lagged estimates 
# and their covariance matrix:
#
# Standardized parameter estimates and there covariance matrix
StdEst <- standardizedsolution(RICLPM_ws.fit, type = "std.nox")
vcov_StdEst <- lavInspect(RICLPM_ws.fit, "vcov.std.nox")
#
# Check which are the indices for the parameters of interest:
StdEst
index_StdEst <- c(22,23, 26,27, 30,31, 34,35)
# CHECK: StdEst[index_StdEst, ] 
# and what the indices for the corresponding covariance matrix:
vcov_StdEst
index_vcov <- c(2,3, 6,7, 10,11, 14,15)
# CHECK: vcov_StdEst[index_vcov, index_vcov]
#
est  <- StdEst[index_StdEst, 4] # Standardize parameter estimates
# label estimates, and these labels should be used in the hypothesis/-es:
names(est) <- c("beta2", "gamma2", "beta3", "gamma3", "beta4", "gamma4", "beta5", "gamma5")
vcov <- vcov_StdEst[index_vcov, index_vcov] # Covariance matrix of standardize parameter estimates
#
# Note: make sure to change the numbers 
#       such that they correspond to the correct estimates.


# Compute GORICA values and weights
#
# Hypothesis w.r.t. random intercept variances
# H_RIvar <- "varRIx > 0 & varRIy > 0" 
# versus it complement, that is, versus all other possibilities
set.seed(123)
GORICA.Result.ws_est_RIvar <- goric(est_RIvar, VCOV = vcov_RIvar, 
                             hypotheses = list(H_RIvar = H_RIvar)) 
# Defaults: comparison = "complement" 
#           type = "gorica"
#
GORICA.Result.ws_est_RIvar
#summary(GORICA.Result.ws_est_RIvar)
#
# Hypothesis w.r.t. wave-specific cross-lagged effects (as specified in the model)
#H_dominance.ws <- "abs(beta2) < abs(gamma2); abs(beta3) < abs(gamma3); 
#         abs(beta4) < abs(gamma4); abs(beta5) < abs(gamma5)"
# versus it complement, that is, versus all other possibilities
set.seed(123)
GORICA.Result.ws_est <- goric(est, VCOV = vcov, 
                          hypotheses = list(H_dominance.ws = H_dominance.ws)) 
# Defaults: comparison = "complement" 
#           type = "gorica"
#
GORICA.Result.ws_est
#summary(GORICA.Result.ws_est)

