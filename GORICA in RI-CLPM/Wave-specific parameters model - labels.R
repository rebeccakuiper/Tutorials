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
dat <- read.table("RICLPM.dat", 
                  col.names = c(
                    "x1", "x2", "x3", "x4", "x5", 
                    "y1", "y2", "y3", "y4", "y5")
)

# Hypothesis w.r.t. wave-specific cross-lagged effects (as specified in the model)
H1ws.l <- "abs(b2) < abs(c2); abs(b3) < abs(c3); 
         abs(b4) < abs(c4); abs(b5) < abs(c5)" 
# versus it complement, that is, versus all other possibilities 

# Fitting a RI-CLPM; here, a bivariate RI-CLPM with wave-specific parameters:
RICLPM.l <- '
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
  wx2 ~ a2*wx1 + b2*wy1 
  wy2 ~ c2*wx1 + d2*wy1
  wx3 ~ a3*wx2 + b3*wy2
  wy3 ~ c3*wx2 + d3*wy2
  wx4 ~ a4*wx3 + b4*wy3
  wy4 ~ c4*wx3 + d4*wy3
  wx5 ~ a5*wx4 + b5*wy4
  wy5 ~ c5*wx4 + d5*wy4
  

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
RICLPM.fit.l <- lavaan(RICLPM.l,
                     data = dat, 
                     missing = "ML", 
                     meanstructure = T, 
                     int.ov.free = T
)
summary(RICLPM.fit.l, standardized = T)


# Compute GORICA values and weights
# Make sure to use: standardized = T
set.seed(123)
GORICA.Result.ws.l <- goric(RICLPM.fit.l, 
                          standardized = T,
                          hypotheses = list(H1ws.l = H1ws.l), 
                          comparison = "complement", 
                          type = "gorica")
GORICA.Result.ws.l
#summary(GORICA.Result.ws.l)

