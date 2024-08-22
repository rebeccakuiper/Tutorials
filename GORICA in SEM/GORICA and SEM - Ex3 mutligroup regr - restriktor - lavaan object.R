
# Exercise 3: Multiple group regression
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


# Make sure that the variable 'sex' is a factor with the right labels.
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))


# Specify the multiple group regression model, using own labels
model3 <- '
    postnumb ~ c(Pre_b, Pre_g)*prenumb 
'


# Hypotheses of interest
# Formulate the hypotheses of interest using our own labeling
H1.3 <- "Pre_b < Pre_g"
# Note: in lavaan output, the labels are sometimes shortened,
# but our labeling is used -- see coef(fit2_r) -- and should thus be used above.


# Fit the multiple group regression model using the lavaan sem function
fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
#
#if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
#library(lavaanPlot)
#lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = "Helvetica"), 
#           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Call goric ('type = "gorica"')
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Calculate GORICA values and weights for H1.3 and its complement ('comparison = "complement"').
set.seed(100)
results3_r <- goric(fit3, hypotheses = list(H1.3 = H1.3), comparison = "complement", type = "gorica", 
                  standardized = T) 
#summary(results3) 
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


################################################################################


# Sensitivity check

# No influence of seed in PT:
#
set.seed(100100)
results3_r_s1 <- goric(fit3, hypotheses = list(H1.3 = H1.3), comparison = "complement", 
      type = "gorica", standardized = T)$result[,3]
set.seed(123456)
results3_r_s2 <- goric(fit3, hypotheses = list(H1.3 = H1.3), comparison = "complement", 
      type = "gorica", standardized = T)$result[,3]
#
results3_r$result[,3]
results3_r_s1$result[,3]
results3_r_s2$result[,3]


################################################################################

