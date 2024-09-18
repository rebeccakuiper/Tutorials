if (!require("metafor")) install.packages("metafor")
library(metafor)
#
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function

#library(devtools) # Make sure you have Rtools (and a version which is compatible with your R version).
#install_github("rebeccakuiper/ICweights")
#library(ICweights)
##?IC.weights # This also contains examples of how to use the function
##citation("ICweights") # In case you use this function, please cite it


#######################################


## Example van Raudenbush (2009) ##
# based on http://www.metafor-project.org/doku.php/analyses:raudenbush2009
#
#Raudenbush (2009) is an excellent chapter in The handbook of research synthesis and meta-analysis (2nd ed.) and describes in detail many of the models and methods that are implemented in the rma() function (including the meta-analytic random- and mixed-effects models). The dataset that is used for the illustration of the various models and methods is actually the same that is used in the Raudenbush and Bryk (1985) and provides the results from 19 studies examining how teachers' expectations about their pupils can influence actual IQ levels (Raudenbush, 1984). A reproduction of the analyses described in Raudenbush and Bryk (1985) can be found here.
#
#Here, I will reproduce the results from Raudenbush (2009). The data are provided in Table 16.1 (p. 300) in the chapter and can be loaded with:
data <- dat.raudenbush1985
data
#(I copy the dataset into 'data', which is a bit shorter and therefore easier to type further below). The contents of the dataset are:
#
#Raudenbush (2009) uses REML estimation to fit a random-effects model. Since REML estimation is the default for the rma() function, we can obtain the same results with:
metaan <- rma(yi, vi, data=data, digits=3)
metaan
# these are the sults reported in Table 16.2 (p. 301). In particular, ^τ2 = .019, ^μ = .084, SE[^μ] = .052, and z = 1.62 for the test H0: μ = 0.


# AIC #
#
#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta")
se_est <- metaan$se
VCOV_est <- vcov(metaan)
#
# Apply AIC #
# Hypothesis of interest
H0 <- "theta = 0"
# vs its complement (default in case of one hypothesis); which is the unconstrained here.
#
# Apply GORICA (default here) to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0)) 
results_H0
#
# The order-restricted hypothesis ‘H0’ has < 1 times more, so less, support than its complement.
#
# So, support for H0 lower than the support for Hunc, 
# but no quantification for (lack of) support for the hypothesis of interest H1.


# GORICA #
#
#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta")
se_est <- metaan$se
VCOV_est <- vcov(metaan)
#
# Apply GORICA #
# Hypothesis of interest (fictional)
H1 <- "theta > 0"
# vs its complement (default in case of one hypothesis)
#
# Apply GORICA (default here)
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H1))
results_H1
#
# The order-restricted hypothesis ‘H1’ has (> 1 times) more support than its complement.
# Support for 'theta > 0' is stronger than that for 'theta < 0'.


# Alternative: H0 and H1
H0 <- "theta = 0"
H1 <- "theta > 0"
# and unconstrained (default in case of multiple hypotheses)
# GORICA (default here)
set.seed(123) # set seed: to obtain the same results when you re-run it 
goric(est, VCOV = VCOV_est, 
      hypotheses = list(H0 = H0, H1 = H1))
#
# Note: H1 and H0 are subsets of Hunc (and H0 is also part of H1).
#
# It is better to combine results of H0 with that of H1 and its complement:
library(devtools) # Make sure you have Rtools (and a version which is compatible with your R version).
install_github("rebeccakuiper/ICweights")
library(ICweights)
#?IC.weights
#citation("ICweights")
#
#results_H0 <- goric(est, VCOV = VCOV_est, H0, comparison = "complement", type = "gorica")
#results_H1 <- goric(est, VCOV = VCOV_est, H1, comparison = "complement", type = "gorica")
ICw <- IC.weights(c(results_H0$result[1,4], results_H1$result[,4]), Name_Hypo = c("H0", "H1", "compl_H1"))
ICw
#
# Most support for H1 (which includes also some support for H0). 
# H1 (> 1 times) more supported than H0 and
# H1 (> 1 times) times more than its complement (cf. results first analysis).
#
# Note: support for 'theta >= 0' vs 'theta < 0' is 0.2589079 + 0.5840564 vs 0.1570356
IC.weights_Hypo <- c(0.2589079 + 0.5840564, 0.1570356)
names(IC.weights_Hypo) <- c("theta >= 0", "theta < 0")
rel.IC.weights_Hypo <- IC.weights_Hypo[1] / IC.weights_Hypo[2]
names(rel.IC.weights_Hypo) <- c("theta >= 0 vs theta < 0")
IC.weights_Hypo
rel.IC.weights_Hypo
#
#
# or: support for 'theta > 0' vs 'theta <= 0' is 0.2589079 vs 0.5840564 + 0.1570356
IC.weights_Hypo <- c(0.2589079, 0.5840564 + 0.1570356)
names(IC.weights_Hypo) <- c("theta > 0", "theta <= 0")
rel.IC.weights_Hypo <- IC.weights_Hypo[1] / IC.weights_Hypo[2]
names(rel.IC.weights_Hypo) <- c("theta > 0 vs theta <= 0")
IC.weights_Hypo
rel.IC.weights_Hypo


#######################################

