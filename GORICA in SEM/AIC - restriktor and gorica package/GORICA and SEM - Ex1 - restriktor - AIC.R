
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
# Note: The goric function cannot use the default labeling, so:
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
# Note: The restrictions are 'connected' by using ';'.


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
# Alternatively, an approximated AIC can be used:
H1.1_AIC <- "
A1 == .6; A2 == .6; A3 == .6; A4 == .6; A5 == .6; A6 == .6; 
B1 == .6; B2 == .6; B3 == .6; B4 == .6; B5 == .6; B6 == .6
"
# Note: An equality is represented by '=='; not '='.
set.seed(100)
results1_r_AIC <- goric(fit1_r, H1.1_AIC, comparison = "complement", type = "gorica", standardized = TRUE) # Note: Complement is Unconstrained here which is the default
summary(results1_r_AIC) # Note: This also includes the comparison of hypotheses
#
#        model    loglik  penalty   gorica  gorica.weights
#1    H1.1_AIC  -373.626    0.000  747.253           0.000
#2  complement    33.581   12.000  -43.162           1.000


# Call goric ('type = "gorica"')
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Calculate GORICA values and weights for H1.1 and its complement ('comparison = "complement"').
set.seed(100)
results1_r <- goric(fit1_r, H1.1, comparison = "complement", type = "gorica", standardized = TRUE) 
summary(results1_r) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty   gorica  gorica.weights
#1        H1.1  33.581    8.139  -50.884           0.988
#2  complement  32.879   11.886  -41.985           0.012


# Note: The default way of calculating the PT in goric() is slow(er) when the number of parameters is large. 
# Here, there are 12 parameters, so may want to use: 'mix.weights = "boot"'.
# That is, we will use bootstrap in the calculation of the level probabilities (LPs) needed in PT. 
# The results of course do not change, but the computation time may.
# 
# Determine number of cores that can be used, to fasten the calculation of PT even more.
if (!require("parallel")) install.packages("parallel") # install this package first (once)
library(parallel)
nrCPUcores <- detectCores(all.tests = FALSE, logical = TRUE)
#
set.seed(100)
results1_r_b <- goric(fit1_r, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
                   mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)
summary(results1_r_b) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty   gorica  gorica.weights
#1        H1.1  33.581    8.148  -50.866           0.988
#2  complement  32.879   11.883  -41.993           0.012


#####################################################################################

# Sensitivity check

# Influence of seed in PT, but negligible:
#
#set.seed(100)
#goric(fit1_r, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
#      mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
results1_r_b$result[,3]
set.seed(100100)
goric(fit1_r, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
       mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
set.seed(123456)
goric(fit1_r, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
       mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
#
#[1]  8.147561 11.882759
#[1]  8.134921 11.893199
#[1]  8.140621 11.886719


#####################################################################################

# The support for 'H1.1' is to be expected, when inspecting:
#lavaan::standardizedsolution(fit1_r)[1:12,]
#lhs op rhs est.std    se      z pvalue ci.lower ci.upper
#1    A =~  Ab   0.710 0.034 20.747      0    0.643    0.778
#2    A =~  Al   0.811 0.025 33.057      0    0.763    0.860
#3    A =~  Af   0.837 0.022 38.181      0    0.794    0.880
#4    A =~  An   0.906 0.015 60.904      0    0.877    0.935
#5    A =~  Ar   0.698 0.035 19.743      0    0.629    0.767
#6    A =~  Ac   0.873 0.018 47.946      0    0.837    0.909
#7    B =~  Bb   0.766 0.030 25.781      0    0.708    0.824
#8    B =~  Bl   0.648 0.040 16.095      0    0.569    0.727
#9    B =~  Bf   0.810 0.025 31.906      0    0.760    0.860
#10   B =~  Bn   0.888 0.018 50.031      0    0.853    0.923
#11   B =~  Br   0.721 0.034 21.241      0    0.654    0.787
#12   B =~  Bc   0.828 0.024 35.082      0    0.782    0.874
#or 
summary(fit1_r, standardize = T)
#Latent Variables:
#  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#A =~                                                                  
#  Ab                3.885    0.312   12.432    0.000    3.885    0.710
#  Al                9.583    0.637   15.038    0.000    9.583    0.811
#  Af                3.367    0.213   15.779    0.000    3.367    0.837
#  An               10.906    0.607   17.965    0.000   10.906    0.906
#  Ar                1.790    0.147   12.140    0.000    1.790    0.698
#  Ac                4.424    0.262   16.882    0.000    4.424    0.873
#B =~                                                                  
#  Bb                4.433    0.323   13.722    0.000    4.433    0.766
#  Bl                5.054    0.462   10.935    0.000    5.054    0.648
#  Bf                3.200    0.215   14.912    0.000    3.200    0.810
#  Bn                8.588    0.498   17.240    0.000    8.588    0.888
#  Br                1.997    0.159   12.596    0.000    1.997    0.721
#  Bc                3.685    0.239   15.422    0.000    3.685    0.828
