
# Example 2: Latent Regression
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


# Specify the latent regression model, using own labels:
model2_r <- '
    A =~ Ab + Al + Af + An + Ar + Ac 
    B =~ Bb + Bl + Bf + Bn + Br + Bc 
    A ~ AB*B + AAge*age + APeabody*peabody
'

# Note on age and peabody (i.e., biological and mental age): 
#cor(sesamesim$peabody, sesamesim$age)
# [1] 0.2396424

# Fit the confirmatory factor model using the lavaan sem function
fit2_r <- sem(model2_r, data = sesamesim, std.lv = TRUE)
#
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit2_r, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Hypotheses of interest
# Formulate the hypotheses of interest
# Notably, using our own labeling
H1.2 <- "AB > APeabody; APeabody = AAge; AAge = 0"
H2.2 <- "AB > APeabody; APeabody > AAge; AAge = 0" 
H3.2 <- "AB > APeabody; APeabody > AAge; AAge > 0"
# Notes: 
# - The restrictions are 'connected' by using ';' or '&'.
#   One could also use, for example: "AB > APeabody > AAge > 0"
# - In lavaan output, the labels are sometimes shortened,
#   but our labeling is used and should thus be used above.

# Call goric 
# Default in case of lavaan objects: type = "gorica"
# Note: We need standardized estimates for a meaningful comparison ('standardize = TRUE').
#
# Because there is more than 1 hypothesis,
# we will use the unconstrained hypothesis as safeguard (which is the default).
set.seed(100)
results2_r <- goric(fit2_r, 
                    hypotheses = list(H1.2 = H1.2, H2.2 = H2.2, H3.2 = H3.2), 
                    standardized = TRUE) 
#summary(results2_r) 
# All three theory-based hypotheses are not weak (nl, better than the unconstrained),
# and H1.2 is the best from the set (nl, highest GORICA weight).

# Note:
# Hypotheses are nested, so hypotheses share support.
# Therefore, we examine the best of these against its compliment (default in case of one hypothesis):
set.seed(100)
results2_c_r <- goric(fit2_r, hypotheses = list(H1.2 = H1.2), standardized = TRUE) 
#summary(results2_c_r) 
results2_c_r
# The order-restricted hypothesis ‘H1.2’ has (> 1 times) more support than its complement.


#####################################################################################


# Sensitivity check

# Influence of seed in PT (of H3), but negligible:
#
set.seed(100100)
results2_r_s1 <- goric(fit2_r, hypotheses = list(H1.2 = H1.2, H2.2 = H2.2, H3.2 = H3.2), standardized = TRUE)$result[,3]
set.seed(123456)
results2_r_s2 <- goric(fit2_r, hypotheses = list(H1.2 = H1.2, H2.2 = H2.2, H3.2 = H3.2), standardized = TRUE)$result[,3]
#
results2_r$result[,3]
results2_r_s1$result[,3]
results2_r_s2$result[,3]


#####################################################################################


# The support for 'H1.2' is to be expected, when inspecting:
#lavaan::standardizedsolution(fit2_r)[13:15,]
##   lhs op     rhs est.std    se      z pvalue ci.lower ci.upper
##13   A  ~       B   0.789 0.030 26.312  0.000    0.730    0.848
##14   A  ~     age   0.000 0.047 -0.010  0.992   -0.093    0.092
##15   A  ~ peabody  -0.016 0.047 -0.330  0.741   -0.108    0.077
#or 
summary(fit2_r, standardize = T)
#Regressions:
#               Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all(!)
#A ~                                                                   
#  B               1.284    0.129    9.941    0.000(!)    0.789    0.789(!)
#age              -0.000    0.012   -0.010    0.992(!)   -0.000   -0.000(!)
#peabody          -0.002    0.005   -0.330    0.741(!)   -0.001   -0.016(!)
