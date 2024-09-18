
# Example 1: Confirmatory factor analysis
# Using the goric function in the package restriktor


# Load the restriktor and lavaan libraries. 
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(restriktor)
library(lavaan)
#
# Also load the gorica library because it contains the data 'sesamesim'
if (!require("gorica")) install.packages("gorica") # install this package first (once)
library(gorica)


# Specify the confirmatory factor model
# Note: The goric function can sometimes not use the default labeling, so:
# Give your own labels to estimates by including them in the lavaan model:
model1_r <- '
    A =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac 
    B =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc 
'

# Fit the confirmatory factor model using the lavaan sem function
fit1_r <- sem(model1_r, data = sesamesim, std.lv = TRUE)
#
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit1_r, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)

# Hypotheses of interest
# Formulate the hypothesis of interest (here, consisting of 12 order restrictions)
# Notably, using our own labeling
H1.1 <- "
A1 > .6; A2 > .6; A3 > .6; A4 > .6; A5 > .6; A6 > .6; 
B1 > .6; B2 > .6; B3 > .6; B4 > .6; B5 > .6; B6 > .6
"
# vs its complement
#
# Notes: - The restrictions are 'connected' by using ';'
#        - In lavaan output, the labels are sometimes shortened,
#          but our labeling is used and should thus be used above.


# Estimates and CIs
standardizedSolution(fit1_r)[1:12, c(1:4,8:9)]
#   lhs op rhs est.std ci.lower ci.upper
#1    A =~  Ab   0.710    0.643    0.778
#2    A =~  Al   0.811    0.763    0.860
#3    A =~  Af   0.837    0.794    0.880
#4    A =~  An   0.906    0.877    0.935
#5    A =~  Ar   0.698    0.629    0.767
#6    A =~  Ac   0.873    0.837    0.909
#7    B =~  Bb   0.766    0.708    0.824
#8    B =~  Bl   0.648    0.569    0.727
#9    B =~  Bf   0.810    0.760    0.860
#10   B =~  Bn   0.888    0.853    0.923
#11   B =~  Br   0.721    0.654    0.787
#12   B =~  Bc   0.828    0.782    0.874


# AIC
model1_AIC <- '
    A =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac 
    B =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc 
    
    A1 == .6; A2 == .6; A3 == .6; A4 == .6; A5 == .6; A6 == .6; 
    B1 == .6; B2 == .6; B3 == .6; B4 == .6; B5 == .6; B6 == .6
'
# Fit the confirmatory factor model using the lavaan sem function
fit1_AIC <- sem(model1_AIC, data = sesamesim, std.lv = TRUE)
#Warning message:
#  In lav_object_post_check(object) :
#  lavaan WARNING: covariance matrix of latent variables
#is not positive definite;
#use lavInspect(fit, "cov.lv") to investigate.
lavInspect(fit1_AIC, "cov.lv")
#
#
# Alternatively, an approximated AIC can be used:
H1.1_AIC <- "
A1 = .6; A2 = .6; A3 = .6; A4 = .6; A5 = .6; A6 = .6; 
B1 = .6; B2 = .6; B3 = .6; B4 = .6; B5 = .6; B6 = .6
"
# vs its complement (default in case of one hypothesis)
# here, the complement is the unconstrained
#
# GORICA (default)
set.seed(100)
results1_r_AIC <- goric(fit1_r, hypotheses = list(H1.1_AIC = H1.1_AIC), standardized = TRUE) 
summary(results1_r_AIC) # Note: This also includes the comparison of hypotheses


# Call goric
# Default in case of lavaan objects: type = "gorica"
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Calculate GORICA values and weights for H1.1 and its complement (default in case of one hypothesis).
set.seed(100)
results1_r <- goric(fit1_r, hypotheses = list(H1.1 = H1.1), standardized = TRUE) 
results1_r
#summary(results1_r) 
