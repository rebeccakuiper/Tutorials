
# Example 3: Multiple group latent regression
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


# Make sure that the variable 'sex' is a factor.
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))

# Specify the multiple group regression model, using own labels
model3 <- '
    postnumb ~ c(Pre_b, Pre_g)*prenumb 
'

# Fit the multiple group latent regression model using the lavaan sem function
fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
#
#if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
#library(lavaanPlot)
#lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = "Helvetica"), 
#           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Hypotheses of interest
# Formulate the hypotheses of interest
# Notably, using our own labeling (which should be the same as those used below)
H1.3 <- "Pre_b < Pre_g"
# vs its complement


# Estimates and CIs
standardizedSolution(fit3)[c(1,6), c(1:5,9:10)]
#       lhs op     rhs group est.std ci.lower ci.upper
#1 postnumb  ~ prenumb     1   0.680    0.593    0.766
#6 postnumb  ~ prenumb     2   0.672    0.587    0.757


# AIC
model3_AIC <- '
    postnumb ~ prenumb 
'
# Fit the confirmatory factor model using the lavaan sem function
fit3_AIC <- sem(model3_AIC, data = sesamesim, std.lv = TRUE, group = "sex", group.equal = c("loadings"))
parameterEstimates(fit3_AIC, standardized = T)
standardizedSolution(fit3_AIC)
# Unless the data is standardized such that prenumb ~~  prenumb = 1, this equates the unstandardized parameters and not the standardized ones. 
#
# Alternatively, an approximated AIC can be used:
H1.3_AIC <- "Pre_b = Pre_g"
# vs its complement (default in case of one hypothesis) - which is unconstrained now.
# For the results, see "# AIC" below


# Call goric 
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Here, we extract the estimates ourselves and use that.
# Default in this case: type = "gorica"
#
# Obtain standardized(!) estimates from lavaan object (labelled or unlabelled):
est_3 <- lavaan::standardizedsolution(fit3)[c(1,6), 'est.std']
names(est_3) <- c("Pre_b", "Pre_g") # Note: This should be the same as the labeling used the H1.3.
# Obtain covariance matrix of these standardized(!) estimates:
vcov_3 <- lavInspect(fit3, "vcov.std.all")[c(1,4), c(1,4)] # Note: Use this in the 'VCOV = vcov_3' command.
#
# AIC
results3_r_AIC <- goric(est_3, VCOV = vcov_3, 
                        hypotheses = list(H1.3_AIC = H1.3_AIC))
results3_r_AIC
#summary(results3_r_AIC)
#
#
# Calculate GORICA values and weights for H1.3 and its complement (default in case of one hypothesis)..
results3_r <- goric(est_3, VCOV = vcov_3, 
                    hypotheses = list(H1.3 = H1.3))
results3_r
#(results3_r) 


#####################################################################################
