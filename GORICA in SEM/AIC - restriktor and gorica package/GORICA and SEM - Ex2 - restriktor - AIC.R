
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


# Specify the latent regression model
# Note: The goric function can sometimes not use the default labeling, so:
# Give your own labels to estimates by including them in the lavaan model:
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
H1.2 <- "AB > APeabody; APeabody == AAge; AAge == 0"
H2.2 <- "AB > APeabody; APeabody > AAge; AAge == 0" 
H3.2 <- "AB > APeabody; APeabody > AAge; AAge > 0"
# Notes: 
# An equality is represented by '=='; not '='.
# The restrictions are 'connected' by using ';'.
# Each restriction shoudl be specified seperetaly in a hypothesis; not like: "AB > APeabody == AAge == 0".


# Estimates and CIs
standardizedSolution(fit2_r)[13:15, c(1:4,8:9)]
#lhs op     rhs est.std ci.lower ci.upper
#13   A  ~       B   0.789    0.730    0.848
#14   A  ~     age   0.000   -0.093    0.092
#15   A  ~ peabody  -0.016   -0.108    0.077


# AIC
model2_AIC_H1 <- '
A =~ Ab + Al + Af + An + Ar + Ac 
B =~ Bb + Bl + Bf + Bn + Br + Bc 
A ~ AB*B + AAge*age + APeabody*peabody

AB > APeabody; APeabody == AAge; AAge == 0
'
model2_AIC_H2 <- '
A =~ Ab + Al + Af + An + Ar + Ac 
B =~ Bb + Bl + Bf + Bn + Br + Bc 
A ~ AB*B + AAge*age + APeabody*peabody

AB > APeabody; APeabody > AAge; AAge == 0
'
model2_AIC_H3 <- '
A =~ Ab + Al + Af + An + Ar + Ac 
B =~ Bb + Bl + Bf + Bn + Br + Bc 
A ~ AB*B + AAge*age + APeabody*peabody

AB > APeabody; APeabody > AAge; AAge > 0
'
# Fit the confirmatory factor model using the lavaan sem function
fit2_AIC_H1 <- sem(model2_AIC_H1, data = sesamesim, std.lv = TRUE)
fit2_AIC_H2 <- sem(model2_AIC_H2, data = sesamesim, std.lv = TRUE)
fit2_AIC_H3 <- sem(model2_AIC_H3, data = sesamesim, std.lv = TRUE)
#Warning messages
#
# Alternatively, an approximated AIC can be used:
H1.2_AIC <- "AB == APeabody; APeabody == AAge; AAge == 0"
H2.2_AIC <- "APeabody == AAge; AAge == 0" 
H3.2_AIC <- "AAge == 0"
set.seed(100)
results2_r_AIC <- goric(fit2_r, 
                        hypotheses = list(H1.2_AIC = H1.2_AIC, H2.2_AIC = H2.2_AIC, H3.2_AIC = H3.2_AIC), 
                        standardized = TRUE) 
summary(results2_r_AIC)


# Call goric
# Default in case of lavaan objects: type = "gorica"
# Note: We need standardized estimates for a meaningful comparison ('standardize = TRUE').
#
# Because there is more than 1 hypothesis, we cannot use: comparison = "complement".
# We will use the unconstrained hypothesis as safeguard (which is the default in case of multiple hypotheses).
set.seed(100)
results2_r <- goric(fit2_r, 
                    hypotheses = list(H1.2 = H1.2, H2.2 = H2.2, H3.2 = H3.2), 
                    standardized = TRUE) 
results2_r
summary(results2_r) 

# Note:
# Hypotheses are nested, so hypotheses share support.
# Therefore, we examine the best of these against its compliment (default in case of one hypothesis):
set.seed(100)
results2_c_r <- goric(fit2_r, hypotheses = list(H1.2 = H1.2), standardized = TRUE) 
results2_c_r
#summary(results2_c_r) 