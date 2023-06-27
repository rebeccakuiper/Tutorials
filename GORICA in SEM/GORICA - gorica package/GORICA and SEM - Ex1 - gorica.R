
# Example 1: Confirmatory factor analysis
# Using the gorica function in the package gorica


# Load the gorica and lavaan libraries. 
if (!require("gorica")) install.packages("gorica") # install this package first (once)
#remotes::install_github("cjvanlissa/gorica@contingency_tables")
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(gorica)
library(lavaan) # Visit www.lavaan.org for lavaan mini-tutorials, examples, and elaborations


#  Specify the confirmatory factor model
model1 <- '
    A =~ Ab + Al + Af + An + Ar + Ac 
    B =~ Bb + Bl + Bf + Bn + Br + Bc 
'
# Note: One can also label the estimates of interest and use these in the hypotheses.

# Fit the confirmatory factor model using the lavaan sem function
fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
#
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Hypotheses of interest
#
# Inspect the parameter names (such that hypotheses can be specified using these)
coef(fit1)
#
# Formulate the hypothesis of interest (here, consisting of 12 order restrictions)
hypotheses1 <- "
A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac > .6 & 
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc > .6
"
# Note: The restrictions are 'connected' by using '&'


# Call gorica
# Note: We need standardized estimates for a meaningful comparison ('standardize = TRUE').
#
# Calculate GORICA values and weights for 'hypotheses1' and its complement ('comparison = "complement"').
set.seed(100)
results1 <- gorica(fit1, hypotheses1, comparison = "complement", standardize = TRUE) 
results1
#
#   loglik penalty gorica   gorica_weights
#H1 33.581 8.147   -50.867 0.988         
#Hc 32.879 11.883  -41.993 0.012    


#####################################################################################

# Sensitivity check

# Influence of seed in PT, but negligible:
#
#set.seed(100)
#gorica(fit1, hypotheses1, comparison = "complement", standardize = TRUE)$fit[,2]
results1$fit[,2]
set.seed(100100)
gorica(fit1, hypotheses1, comparison = "complement", standardize = TRUE)$fit[,2]
set.seed(123456)
gorica(fit1, hypotheses1, comparison = "complement", standardize = TRUE)$fit[,2]
#
#[1]  8.14744 11.88276
#[1]  8.13477 11.89320
#[1]  8.14062 11.88672


#####################################################################################

# The support for 'hypotheses1' is to be expected, when inspecting:
#lavaan::standardizedsolution(fit1)[1:12,]
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
summary(fit1, standardize = T)
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
