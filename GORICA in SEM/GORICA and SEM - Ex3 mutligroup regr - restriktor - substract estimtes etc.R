
# Example 3: Multiple group latent regression
# Using the goric function in the package restriktor
# Note: here, different input is used than in 'GORICA and SEM - Ex3 mutligroup regr - restriktor - lavaan object.R'.

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
# Note: in lavaan output, the labels are sometimes shortened,
# but our labeling is used -- see coef(fit2_r) -- and should thus be used above.


# Call goric ('type = "gorica"')
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Instead of using a lavaan object (as done in 'GORICA and SEM - Ex3 mutligroup regr - restriktor - lavaan object.R'),
# we can extract the estimates and their covariance matrix ourselves and use that.
#
# Obtain standardized(!) estimates from lavaan object (labelled or unlabelled):
est_3 <- lavaan::standardizedsolution(fit3)[c(1,6), 'est.std']
names(est_3) <- c("Pre_boy", "Pre_girl") # Note: This should be the same as the labeling used the H1.3.
vcov_3 <- lavInspect(fit3, "vcov.std.all")[c(1,4), c(1,4)] # Note: Use this in the 'VCOV = vcov_3' command.
#
# Calculate GORICA values and weights for H1.3 and its complement ('comparison = "complement"').
results3_r <- goric(est_3, VCOV = vcov_3, hypotheses = list(H1.3 = H1.3), comparison = "complement", type = "gorica") 
#summary(results3_r) 
results3_r
# The order-restricted hypothesis ‘H1.3’ has about the same support as its complement.
#
# Possible conclusion:
# - This output table shows that the hypothesis of interest and its compliment 
#   are equally likely, since both have a weight of approximately .50. 
#   That is, they have about the same support.
# - Since the hypotheses do not overlap and are equally complex (i.e., have the 
#   same penalty value), this implies that their boundary is the preferred 
#   hypothesis, that is, H0: Pre_b = Pre_g.
# - Thus, there is support for the boundary of the hypothesis of interest and 
#   its complement, indicating that the relationship between postnumb and 
#   prenumb is equally high for girls and boys.


#####################################################################################


# Sensitivity check

# No influence of seed in PT:
#
set.seed(100100)
results3_r_s1 <- goric(est_3, VCOV = VCOV_3, hypotheses = list(H1.3 = H1.3), comparison = "complement", type = "gorica")$result[,3]
set.seed(123456)
results3_r_s2 <- goric(est_3, VCOV = VCOV_3, hypotheses = list(H1.3 = H1.3), comparison = "complement", type = "gorica")$result[,3]
#
results3_r$result[,3]
results3_r_s1$result[,3]
results3_r_s2$result[,3]


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
