
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


# Specify the multiple group latent regression model
model3 <- '
    postnumb ~ prenumb 
'

# Make sure that the variable 'sex' is a factor.
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))

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
H1.3 <- "Pre_boy < Pre_girl"


# Call goric ('type = "gorica"')
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Currently, in case of multiple-group latent regression, the goric() cannot use a lavaan object. 
# Now, we should extract the estimates ourselves and use that.
#
# Obtain standardized(!) estimates from lavaan object (labelled or unlabelled):
est_3 <- lavaan::standardizedsolution(fit3)[c(1,6), 'est.std']
names(est_3) <- c("Pre_boy", "Pre_girl") # Note: This should be the same as the labeling used the H1.3.
vcov_3 <- lavInspect(fit3, "vcov.std.all")[c(1,4), c(1,4)] # Note: Use this in the 'VCOV = vcov_3' command.
#
# Calculate GORICA values and weights for H1.3 and its complement ('comparison = "complement"').
results3_r <- goric(est_3, VCOV = vcov_3, hypotheses = list(H1.3), comparison = "complement", type = "gorica") 
summary(results3_r) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty  gorica  gorica.weights
#1        H1.3   4.420    1.500  -5.841           0.498
#2  complement   4.428    1.500  -5.856           0.502


#####################################################################################

# Sensitivity check

# No influence of seed in PT:
#
#set.seed(100)
#goric(est_3, VCOV = VCOV_3, H1.3, comparison = "complement", type = "gorica")$result[,3]
results3_r$result[,3]
set.seed(100100)
goric(est_3, VCOV = VCOV_3, hypotheses = list(H1.3), comparison = "complement", type = "gorica")$result[,3]
set.seed(123456)
goric(est_3, VCOV = VCOV_3, hypotheses = list(H1.3), comparison = "complement", type = "gorica")$result[,3]
#
# 1.5 1.5
# 1.5 1.5
# 1.5 1.5


#####################################################################################

# The support for 'H1.3' is to be expected, when inspecting:
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
