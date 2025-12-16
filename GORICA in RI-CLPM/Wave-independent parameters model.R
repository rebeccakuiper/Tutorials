#Causal dominance hypothesis evaluation
#When the interest lies in evaluating a theory-based, informative hypothesis, 
#one can use the AIC-type criterion called GORICA (Altinisik et al., 2021). 
#One can then evaluate one or more hypotheses reflecting a priori theories, 
#which then often contain orderings of (standardized) parameters. 
#For example, one may hypothesize that a cross-lagged relationship is 
#higher than another one; often referred to as ‘causal dominance’. 
#Such a causal dominance hypothesis can be evaluated with the GORICA (Sukpan and Kuiper, 2024)
#
#Below you find some code to evaluate a causal dominance hypothesis using the GORICA. 
#
#Additionally, html tutorials and R scripts for evaluating informative hypotheses 
#using the GORIC(A) in R can be found on https://github.com/rebeccakuiper/Tutorials.
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
# R code to run a `wave-independent' parameters model
# A bivariate RI-CLPM with 2 variables and 5 time points

# Using the lavaan object with user-specified parameter labels

library(lavaan)
library(restriktor)

# Load the data set into R: Traditional RI-CLPM
dat <- read.table("RICLPM.dat", 
                  col.names = c(
                    "x1", "x2", "x3", "x4", "x5", 
                    "y1", "y2", "y3", "y4", "y5")
) 

# Standardize the data
dat <- scale(dat)

# Hypothesis w.r.t. random intercept variances
H_RIvar <- "varRIx > 0 & varRIy > 0" 
# versus it complement, that is, versus all other possibilities
# default in case of one hypothesis
#
# Hypothesis w.r.t. cross-lagged effects (as specified in the model)
H_dominance <- "abs(beta) < abs(gamma)" 
# versus it complement, that is, versus all other possibilities 
# (here: versus abs(beta) > abs(gamma))
# default in case of one hypothesis

# Fitting a RI-CLPM; here, a bivariate RI-CLPM with wave-independent parameters:
RICLPM_labelled <- '
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
  # (constrained)
  #
  wx2 ~ alpha*wx1 + beta*wy1 
  wy2 ~ gamma*wx1 + delta*wy1
  #
  wx3 ~ alpha*wx2 + beta*wy2
  wy3 ~ gamma*wx2 + delta*wy2
  #
  wx4 ~ alpha*wx3 + beta*wy3
  wy4 ~ gamma*wx3 + delta*wy3
  #
  wx5 ~ alpha*wx4 + beta*wy4
  wy5 ~ gamma*wx4 + delta*wy4
  #
  # Note that lavaan constraints the unstandarized parameters.
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations, constrained)
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5
  
  # Estimate covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # Estimate variance and covariance of random intercepts
  RIx ~~ varRIx*RIx
  RIy ~~ varRIy*RIy
  RIx ~~ RIy
  
  # Estimate (residual) variance of within-person centered variables 
  # (constrained)
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
  
  # Constrain grand means over time
  x1 + x2 + x3 + x4 + x5 ~ mx*1
  y1 + y2 + y3 + y4 + y5 ~ my*1
'
RICLPM_labelled.fit <- lavaan(RICLPM_labelled, 
                              data = dat, 
                              missing = 'ML', 
                              meanstructure = T, 
                              int.ov.free = T
) 
summary(RICLPM_labelled.fit, standardized = T)


# Compute GORICA values and weights
#
# Hypothesis w.r.t. random intercept variances
# H_RIvar <- "varRIx > 0 & varRIy > 0" 
# versus it complement, that is, versus all other possibilities
set.seed(123)
GORICA.Result_RIvar <- goric(RICLPM_labelled.fit, 
                       hypotheses = list(H_RIvar = H_RIvar)) 
# Defaults: comparison = "complement" 
#           type = "gorica"
#
GORICA.Result_RIvar
#summary(GORICA.Result_RIvar)
#
# Hypothesis w.r.t. cross-lagged effects (as specified in the model)
# H1 <- "abs(beta) < abs(gamma)" 
# versus it complement, that is, versus all other possibilities
# Note that lavaan constraints the unstandarized parameters.
set.seed(123)
GORICA.Result <- goric(RICLPM_labelled.fit,
                       hypotheses = list(H_dominance = H_dominance)
                       ) 
# Defaults: comparison = "complement" 
#           type = "gorica"
#
GORICA.Result
#summary(GORICA.Result)


