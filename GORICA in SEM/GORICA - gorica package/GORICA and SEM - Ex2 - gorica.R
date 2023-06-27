
# Example 2: Latent Regression
# Using the gorica function in the package gorica


# Load the gorica and lavaan libraries.
if (!require("gorica")) install.packages("gorica") # install this package first (once)
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(gorica)
library(lavaan) # Visit www.lavaan.org for lavaan mini-tutorials, examples, and elaborations


# Specify the latent regression model
model2 <- '
A =~ Ab + Al + Af + An + Ar + Ac 
B =~ Bb + Bl + Bf + Bn + Br + Bc
A ~ B + age + peabody
'
# Note: One can also label the estimates of interest and use these in the hypotheses.

# Note on age and peabody (i.e., biological and mental age): 
#cor(sesamesim$peabody, sesamesim$age)
# [1] 0.2396424

# Fit the latent regression model using the lavaan sem function
fit2 <- sem(model2, data = sesamesim, std.lv = TRUE)
#
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Hypotheses of interest
#
# Inspect the parameter names (such that hypotheses can be specified using these)
coef(fit2)
#
# Formulate the hypotheses of interest
hypotheses2 <- "
A~B > A~peabody = A~age = 0; 
A~B > A~peabody > A~age = 0; 
A~B > A~peabody > A~age > 0
"
# Note: The three hypotheses are 'separated' by using ';'


# Call gorica
# Note: We need standardized estimates for a meaningful comparison ('standardize = TRUE').
#
# Because there is more than 1 hypothesis, we cannot use: comparison = "complement".
# We will use the unconstrained hypothesis (Hu) as safeguard (which is the default).
set.seed(100)
results2 <- gorica(fit2, hypotheses2, standardize = TRUE)
results2
#
#   loglik penalty gorica  gorica_weights
#H1 6.836  0.504   -12.663 0.378         
#H2 6.836  0.691   -12.290 0.314         
#H3 6.836  0.823   -12.026 0.275         
#Hu 6.894  3.000   -7.789  0.033   
# Note: Same fit for H1-H3 here, so distinction only via penalty (which is independent of N) and those are also close.
#
# Comparison of hypotheses:
relative.weight <- results2$fit[,4]
relative.weight %*% t(1/relative.weight)


# Note:
# Hypotheses are nested, so hypotheses share support.
# Therefore, we examine the best of these against its compliment:
hypotheses2.H1 <- "A~B > A~peabody = A~age = 0" # Note: We have to create this here, before it was part of the whole set.
set.seed(100)
results2_c <- gorica(fit2, hypotheses2.H1, comparison = "complement", standardize = TRUE)
results2_c
#
#   loglik penalty gorica  gorica_weights
#H1 6.836  0.504   -12.663 0.874         
#Hc 6.894  2.496   -8.798  0.126
#
# Comparison of hypotheses:
relative.weight <- results2_c$fit[,4]
relative.weight %*% t(1/relative.weight)


#####################################################################################

# Sensitivity check

# Influence of seed in PT (of H1-H3), but negligible:
#
#set.seed(100)
#gorica(fit2, hypotheses2, standardize = TRUE)$fit[,2]
results2$fit[,2]
set.seed(100100)
gorica(fit2, hypotheses2, standardize = TRUE)$fit[,2]
set.seed(123456)
gorica(fit2, hypotheses2, standardize = TRUE)$fit[,2]
#
#[1] 0.50437 0.69074 0.82280 3.00000
#[1] 0.50143 0.68815 0.82057 3.00000
#[1] 0.50131 0.68714 0.82416 3.00000


#####################################################################################

# The support for 'hypotheses2.H1' is to be expected, when inspecting:
#lavaan::standardizedsolution(fit2)[13:15,]
##   lhs op     rhs est.std    se      z pvalue ci.lower ci.upper
##13   A  ~       B   0.789 0.030 26.312  0.000    0.730    0.848
##14   A  ~     age   0.000 0.047 -0.010  0.992   -0.093    0.092
##15   A  ~ peabody  -0.016 0.047 -0.330  0.741   -0.108    0.077
#or 
summary(fit2, standardize = T)
#Regressions:
#               Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#A ~                                                                   
#  B               1.284    0.129    9.941    0.000(!)    0.789    0.789(!)
#age              -0.000    0.012   -0.010    0.992(!)   -0.000   -0.000(!)
#peabody          -0.002    0.005   -0.330    0.741(!)   -0.001   -0.016(!)
