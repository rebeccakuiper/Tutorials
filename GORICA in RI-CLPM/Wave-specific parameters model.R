#Causal dominance hypothesis evaluation
#When the interest lies in evaluating a theory-based, informative hypothesis, 
#one can use the AIC-type criterion called GORICA (Altinisik et al., 2021). 
#One can then evaluate one or more hypotheses reflecting a priori theories, 
#which then often contain orderings of (standardized) parameters. 
#For example, one may hypothesize that a cross-lagged relationship is 
#higher than another one; often referred to as ‘causal dominance’. 
#Such a causal dominance hypothesis can be evaluated with the GORICA (Sukpan and Kuiper, 2023).  
#Below you find some code to evaluate a causal dominance hypothesis using the GORICA. 
#
#Additionally, html tutorials and R scripts for evaluating informative hypotheses 
#using the GORIC(A) in R can be found on https://github.com/rebeccakuiper/Tutorials.
#
#Altinisik, Y., van Lissa, C., Hoijtink, H., Oldehinkel, A. J., & Kuiper, R. (2021). Evaluation of inequality constrained hypotheses using a generalization of the AIC. Psychological Methods, 26(5), 599-621. https://doi.org/10.1037/met0000406
#Sukpan, C., & Kuiper, R. M. (2023). How to evaluate causal dominance hypotheses in lagged effects models. Structural Equation Modeling: A Multidisciplinary Journal 0 (0): 1–16. https://doi.org/10.1080/10705511.2023.2265065.


# How to evaluate causal dominance in lagged effects models
# R code to run a `wave-specific' parameters model
# A bivariate RI-CLPM with two variables and 5 time points


library(lavaan)
library(restriktor)

# Load the data set into R
data <- read.table("data.dat")

# Hypothesis w.r.t. wave-specific cross-lagged effects
H1ws <- "abs(beta2) < abs(gamma2); abs(beta3) < abs(gamma3); 
         abs(beta4) < abs(gamma4); abs(beta5) < abs(gamma5)" 
# versus it complement, that is, versus all other possibilities 

# Fitting a RI-CLPM; here, a bivariate RI-CLPM with wave-specific parameters:
RICLPM <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
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
RICLPM.fit <- lavaan(RICLPM,
                     data = dat, 
                     missing = "ML", 
                     meanstructure = T, 
                     int.ov.free = T
)
summary(RICLPM.fit, standardized = T)

# One could label the parameters, similarly to example with constrained parameters, 
# but then using unique names.
# Alternatively, one can extract the standardized cross-lagged estimates 
# and their covariance matrix:
#
# Standardize parameter estimates and there covariance matrix
StdEst <- standardizedsolution(RICLPM.fit, type = "std.nox")
vcov_StdEst <- lavInspect(RICLPM.fit, "vcov.std.nox")
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
set.seed(123)
GORICA.Result.ws <- goric(est, VCOV = vcov, 
                          hypotheses = list(H1ws = H1ws), 
                          comparison = "complement", 
                          type = "gorica")
GORICA.Result.ws
#summary(GORICA.Result.ws)

