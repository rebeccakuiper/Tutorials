
# Example 3: Multiple group latent regression
# Using the gorica function in the package gorica


# Load the gorica and lavaan libraries.
if (!require("gorica")) install.packages("gorica") # install this package first (once)
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(gorica)
library(lavaan) # Visit www.lavaan.org for lavaan mini-tutorials, examples, and elaborations


# Specify the multiple group latent regression model
model3 <- '
    postnumb ~ prenumb 
'
# Note: One can also label the estimate of interest and use this (together with labeling below) in the hypotheses.

# Make sure that the variable 'sex' is a factor.
# And assign labels to the groups to be used when formulating hypotheses
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))

# Fit the multiple group latent regression model using the lavaan sem function
fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
#
#if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
#library(lavaanPlot)
#lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = "Helvetica"), 
#           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Hypotheses of interest
#
# Inspect the parameter names
coef(fit3) 
# Note:
# We can see that the default labeling is: postnumb~prenumb & postnumb~prenumb.g2
# Because of our own labelinng, we can nu use: postnumb~prenumb.boy & postnumb~prenumb.girl
#
# Formulate the hypothesis of interest
# Here, using our own labeling
hypothesis3 <- "
  postnumb~prenumb.boy < postnumb~prenumb.girl
"


# Call gorica
# Note: We need standardized estimates for a meaningful comparison ('standardize = TRUE').
#
# Calculate GORICA values and weights for 'hypothesis3' and its complement ('comparison = "complement"').
set.seed(100)
results3 <- gorica(fit3, hypothesis3, comparison = "complement", standardize = TRUE) 
results3
#
#   loglik penalty gorica gorica_weights
#H1 4.420  1.497   -5.846 0.499         
#Hc 4.428  1.503   -5.851 0.501 
# Hence, H1 and its complement are equally likely. 
# Since these hypotheses do not overlap, the best hypothesis is their boundary (i.e., postnumb~prenumb.boy = postnumb~prenumb.girl).


#####################################################################################

# Sensitivity check

# Influence of seed in PT, but negligible:
#
#set.seed(100)
#gorica(fit3, hypothesis3, comparison = "complement", standardize = TRUE)$fit[,2]
results3$fit[,2]
set.seed(101)
gorica(fit3, hypothesis3, comparison = "complement", standardize = TRUE)$fit[,2]
set.seed(102)
gorica(fit3, hypothesis3, comparison = "complement", standardize = TRUE)$fit[,2]
#
# 1.49723 1.50277
# 1.5005  1.4995
# 1.49875 1.50125


#####################################################################################

# The support for 'hypothesis3' is to be expected, when inspecting:
#lavaan::standardizedsolution(fit3)[c(1,6),]
##       lhs op     rhs group est.std    se      z pvalue ci.lower ci.upper
##1 postnumb  ~ prenumb     1   0.680 0.044 15.439      0    0.593    0.766
##6 postnumb  ~ prenumb     2   0.672 0.043 15.566      0    0.587    0.757
#or 
summary(fit3, standardize = T)
#Group 1 [boy]:
#  
#  Regressions:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#postnumb ~                                                            
#  prenumb           0.843    0.085    9.933    0.000    0.843    0.680(!)
#
#Group 2 [girl]:
#  
#  Regressions:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#postnumb ~                                                            
#  prenumb           0.762    0.075   10.144    0.000    0.762    0.672(!)
