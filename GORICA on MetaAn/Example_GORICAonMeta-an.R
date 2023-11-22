if (!require("metafor")) install.packages("metafor")
library(metafor)
#
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function
#library(devtools) # Make sure you have Rtools (and a version which is compatible with your R version).
#install_github("LeonardV/restriktor")
#library(restriktor)

library(devtools) # Make sure you have Rtools (and a version which is compatible with your R version).
install_github("rebeccakuiper/ICweights")
library(ICweights)
#?IC.weights # This also contains examples of how to use the function
#citation("ICweights") # In case you use this function, please cite it


#######################################


## Example van Raudenbush (2009) / Raudenbush and Bryk (1985) ##
# based on http://www.metafor-project.org/doku.php/analyses:raudenbush2009
# http://www.metafor-project.org/doku.php/analyses:raudenbush1985
#
#
# Here, standardized mean difference (in IQ) are meta-analyzed 
# from 19 studies investigating how the expectations of teachers 
# about their 330 pupils can # influence actual IQ levels of the pupils. 

# Data
data <- dat.raudenbush1985
data


########


#Random-Effects Model
# REML estimation of random-effects model (default in rma()). 
metaan <- rma(yi, vi, data=data, digits=3)

# Notes: 
# The interest lies in the 'standardized mean difference', i.e., Cohen's d.
# This will be denoted by 'theta'.
# 
# Cohen offered the following guidelines for interpreting the magnitude of 
# Cohen's d in the social sciences: 
# small: d = 0.2; medium, d = 0.5; and large, d = 0.8.

# Example 1.1: 
# H1.1: Is there a positive effect (theta > 0)

# Example 1.2: 
# H1.2: Is there a quite small to non-existing effect (0 < theta < .2).


###


# Null hypothesis testing # 

# Example 1.1

# Hypothesis
# H0: theta = 0 # This tests whether nothing is going on.
# In case of a one-sided test: H0: theta >= 0 (thus, H1.1).
# 

# Results
metaan
# Thus: theta = .084, SE[theta] = .052, z = 1.62, and p = .11.
# If a one-sided test, then: p = .105/2 = .05(25), since 
# the sign of the estimate (0.084) is in agreement with the hypothesis.

# Conclusion
# Do not reject H0.
# Thus, in a way, we found evidence for H0 (but we cannot quantify it).
# This implies that nothing is going on.
# In case of the one-sided test: This implies that there is a positive effect.
# Notably, in both cases, no indication nor quantification for support for H1.1.


# Example 1.2

# Hypothesis
# H0: theta = 0.1
# Cannot test for a range (like hypothesis of interest H1.2: 0 < theta < .2).
# Could test theta > 0.1, but that includes small to large effects as well.

# Results
# To test whether μ equals some pre-specified value,
# one should check whether that value lies within the confidence interval:
confint(metaan, fixed = T, random = F)
# or read from the columns "ci.lb   ci.ub" in the object metaan.

# Conclusion
# Do not reject H0, because 0.1 lies within the interval (-0.018, 0.185).
# Thus, in a way, we found evidence for H0.
# Notably, we cannot quantify the evidence in favor of H0 nor H1.2.

  

# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta")
VCOV_est <- vcov(metaan)


# Example 1.1

# Hypothesis
H0 <- "theta == 0" # Hypothesis which will be evaluated
# Cannot test for direction (like hypothesis of interest H1.1: theta > 0).

# Results
# Apply GORICA to obtain AIC weights:
results_H0_1.1 <- goric(est, VCOV = VCOV_est, 
                        hypotheses = list(H0 = H0), comparison = "complement", 
                        type = "gorica") 
# Note that the complement is the unconstrained Hunc now.
results_H0_1.1
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   0.731    0.000  -1.462           0.422
#2  complement   2.044    1.000  -2.089           0.578
#---
#  The order-restricted hypothesis ‘H0’ has  0.731 times more support 
#                                                 than its complement.
# So, the support for H0 just below the support for Hunc.
# Notably, Hunc also includes H0.

# Conclusion
# Some (not compelling) evidence against H0.
# This may imply that there is a positive effect,
# but could also indicate a negative effect.
# Notably, no quantification for (lack of) support for H1.1, only H0.


# Example 1.2

# Hypothesis
H0 <- "theta == 0.1" # Hypothesis which will be evaluated
# Cannot test for a range (like hypothesis of interest H1.2: 0 < theta < .2).

# Results
# Apply GORICA to obtain AIC weights:
results_H0_1.2 <- goric(est, VCOV = VCOV_est, 
                        hypotheses = list(H0 = H0), comparison = "complement", 
                        type = "gorica") 
# Note that the complement is the unconstrained Hunc now.
results_H0_1.2
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   1.995    0.000  -3.989           0.721
#2  complement   2.044    1.000  -2.089           0.279
#---
#  The order-restricted hypothesis ‘H0’ has  2.586 times more support 
#                                                 than its complement.
# So, the support for H0 (2.6 times) higher than the support for Hunc.
# Notably, Hunc also includes H0.

# Conclusion
# Evidence in favor of H0 (a quite small effect).
# Notably, no quantification for support for H1.2, only H0.


# One can also evaluate multiple, competing hypotheses; e.g.:
#H2.1 <- "theta == 0"   # Indicating no effect
#H2.2 <- "theta == 0.2" # Indicating a small effect
#H2.3 <- "theta == 0.5" # Indicating a medium effect
#H2.4 <- "theta == 0.8" # Indicating a large effect



# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta")
VCOV_est <- vcov(metaan)


# Example 1.1

# Hypothesis
H1 <- "theta > 0" # Hypothesis of interest H1.1

# Results
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1_1.1 <- goric(est, VCOV = VCOV_est, 
                        hypotheses = list(H1), comparison = "complement", 
                        type = "gorica")
# Note that the complement is all other orderings now; here: 'theta < 0'.
results_H1_1.1
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   2.044    0.500  -3.089           0.788
#2  complement   0.731    0.500  -0.462           0.212
#---
#  The order-restricted hypothesis ‘H1’ has  3.719 times more support 
#                                                 than its complement.
# Thus, the support for 'theta > 0' versus 'theta < 0' is about 3.7 as strong.

# Conclusion
# Evidence in favor of H1.1: theta > 0.
# Quantification for support for H1.1: About 3.7 times more than for theta < 0.


# Example 1.2

# Hypothesis
H1 <- "0 < theta < .2" # Hypothesis of interest H1.2

# Results
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1_1.2 <- goric(est, VCOV = VCOV_est, 
                        hypotheses = list(H1), comparison = "complement", 
                        type = "gorica")
# Note that the complement is all other orderings now; 
# here: 'theta < 0' and 'theta > 0.2'.
results_H1_1.2
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   2.044    0.000  -4.089           0.910
#2  complement   0.731    1.000   0.538           0.090
#---
#  The order-restricted hypothesis ‘H1’ has 10.110 times more support 
#                                                 than its complement.

# Conclusion
# Compelling evidence in favor of H1.2: 0 < theta < .2, a quite small effect.
# Quantification for support for H1.2: About 10 times more than other effects.



# Alternative:
# One can also evaluate multiple, competing hypotheses.
# In a confirmatory setting, one should only use those that are based on
# theory (previous research) and expertise.
# In an exploratory setting, one can look at all possibilities.
# When looking at an effect size measure, one can easily do the latter,
# as will be shown next,
# which then can lead to generating one or more hypotheses for future research.

# Hypothesis of interest
H2.0 <- "theta < 0"         # Indicating no improvement   
H2.1 <- "0   < theta < 0.2" # Indicating no effect
H2.2 <- "0.2 < theta < 0.5" # Indicating a small effect
H2.3 <- "0.5 < theta < 0.8" # Indicating a medium effect
H2.4 <- "theta > 0.8"       # Indicating a large effect
# Note: These cover all possibilities.
# Hence, we do not need a fail-safe hypothesis.

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_all <- goric(est, VCOV = VCOV_est, 
                     hypotheses = list(H2.0 = H2.0, H2.1 = H2.1, 
                                       H2.2 = H2.2, H2.3 = H2.3, H2.4 = H2.4), 
                     comparison = "none", 
                     type = "gorica")
results_all
round(results_all$ratio.gw, digits = 2)
#
#Results:
#   model   loglik  penalty   gorica  gorica.weights
#1   H2.0    0.731    0.500   -0.462           0.131
#2   H2.1    2.044    0.000   -4.089           0.805
#3   H2.2   -0.491    0.000    0.981           0.064
#4   H2.3  -30.442    0.000   60.883           0.000
#5   H2.4  -94.135    0.500  189.270           0.000
#---
#
#> round(results_all$ratio.gw, digits = 2)
#     vs. H2.0 vs. H2.1 vs. H2.2     vs. H2.3     vs. H2.4
#H2.0     1.00     0.16     2.06 2.093783e+13 1.583837e+41
#H2.1     6.13     1.00    12.62 1.283913e+14 9.712130e+41
#H2.2     0.49     0.08     1.00 1.017521e+13 7.697015e+40
#H2.3     0.00     0.00     0.00 1.000000e+00 7.564477e+27
#H2.4     0.00     0.00     0.00 0.000000e+00 1.000000e+00
#
# Thus, 'H2.1: 0 < theta < 0.2' has the highest support and is thus preferred.
# The support for H2.1 (no effect) versus 'H2.0: theta < 0' (no improvement)
# is about 6.1 times as strong (see ratio.gw or use .805/.131).
# Its support versus that of 'H2.2: 0.2 < theta < 0.5' (small effect) 
# is about 12.6 higher.
# The support for H2.1 versus H2.3 (medium) and H2.4 (large effect) is infinite.
# Hence, there is quite some support for H2.1: 0 < theta < 0.2 (no effect).
#
# This hypothesis can now be suggested to be evaluated in future research.


########


#Mixed-Effects Model
#
# Data change:
# All week values greater than 3 are recoded to 3:
data$weeks.c <- ifelse(data$weeks > 3, 3, data$weeks)
#
# Mixed-effects model:
metaan <- rma(yi, vi, mods = ~ weeks.c, data=data, digits=3)

# Notes: 
#
# 1) 
# The interest lies in the 'standardized mean difference', i.e., Cohen's d,
# and in the effect of the moderator 'week'.
# This will be denoted by 'theta' and 'beta', respectively.
#
# 2)
# If the effect of week will be compared to
# a specific non-zero value or to each other,
# then one needs to standardize this variable.

# Hypothesis of interest
# H1 <- "theta > 0; beta < 0"
#
# One can, of course, also specify one or more hypotheses regarding
# only one of the parameters (either theta or beta).
# Some examples for theta can be found above (in the RE-model).
# Here, both parameters are used and only one hypothesis.
# Note that null hypothesis testing can only test one hypothesis at the time.

metaan
#
#The residual amount of heterogeneity is now τ2≈0 and 
#the test statistic for H0: τ2 = 0 is QE(df=17) = 16.57.
#
#The estimated model is E(di) = .407 − 0.157 xi, 
#where xi is the number of prior contact weeks. 
#
#The standard errors of the model coefficients are 
#SE[b0] = SE[intrcpt] = .087 and SE[b1] = SE[weeks.c] = .036. 
#The test statistics are z0 = 4.68 and z1 = −4.39, respectively.


###


# Null hypothesis testing #

# Results
metaan

# From the confidence interval (CI) of the intercept, that is,
# the effect in case of a mean level of prior contact weeks: (0.237, 0.578),
# one may conclude that the effect size is small to medium. 
# However, its support cannot be quantified.
# It can also not be quantified when testing, for instance, theta = .4.
# One can conclude that the effect is positive, 
# but without quantifying the evidence for that.

# From the CI of the moderator weeks.c, that is,
# the effect of a unit increase in prior contact weeks: (-0.227, -0.087),
# one can conclude that the effect is negative.
# However, one cannot quantify the evidence for that.

# Moreover, one cannot test the two hypotheses simultaneously.
# However, it would be possible when having the multivariate CIs.
# Nevertheless, one can still not quantify the evidence for that.



# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta", "beta")
VCOV_est <- vcov(metaan)

# Hypothesis
# H1 <- "theta > 0; beta < 0" # Hypothesis of interest
H0 <- "theta == 0; beta == 0" # Hypothesis that can be evaluated

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#  Results:
#  model  loglik  penalty  gorica  gorica.weights
#1          H0  -6.197    0.000  12.394           0.000
#2  complement   4.802    2.000  -5.605           1.000
#---
#  The order-restricted hypothesis ‘H0’ has  0.000 times more support 
#                                                 than its complement.
#
# So, full support for complement, which is the unconstrained in this case.
# This includes the hypothesis of interest H1.
# Notably, no quantification for (lack of) support for 
# the hypothesis of interest H1.



# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta", "beta")
VCOV_est <- vcov(metaan)

# Hypothesis of interest 
H1 <- "theta > 0; beta < 0"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica")
results
#
#Results:
#  model  loglik  penalty  gorica  gorica.weights
#1          H1   4.802    1.362  -6.880           1.000
#2  complement  -4.827    1.138  11.929           0.000
#---
#  The order-restricted hypothesis ‘H1’ has 12141.770 times more support 
#                                                    than its complement.
#
# Full support for H1.


# Alternative:
# which shows what the GORICA can do even more.
#
H2 <- "0.2 < theta < 0.8; beta < 0" 
# theta > 0.2; theta < 0.8: indicating a small to medium effect.
# beta < 0:                 indicating a negative effect of the moderator.
#
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2_1 <- goric(est, VCOV = VCOV_est, 
                      hypotheses = list(H2 = H2), comparison = "complement", 
                      type = "gorica")
results_H2_1
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   4.802    0.497  -8.610           0.987
#2  complement   1.969    2.000   0.062           0.013
#---
#The order-restricted hypothesis ‘H2’ has 76.387 times more support 
#                                               than its complement.



#######################################



## Example Berkey et al. (1998) ##
# Based on http://www.metafor-project.org/doku.php/analyses:berkey1998
#
# Here, surgical and non-surgical treatments for medium-severity periodontal 
# disease is compared in five trials for two outcomes: 
# attachment level (AL) and probing depth (PD) one year after the treatment. 
#
# The effect size to be aggregated is the (raw) mean difference, 
# where non-surgical treatment is the reference category. 
# The latter implies that a positive value indicates that 
# surgery was more effective than non-surgical treatment. 
# Note that the outcomes are negatively related: 
# a positive estimate indicates effectiveness of surgery in 
# either increasing the attachment level or decreasing the probing depth.

# Data
data <- dat.berkey1998
data
#
# A multivariate random-effects model (for two outcomes simultaneously).
# Then, one needs the covariance matrix:
V <- bldiag(lapply(split(data[,c("v1i", "v2i")], data$trial), as.matrix))
# Meta-analysis:
metaan <- rma.mv(yi, V, mods = ~ outcome - 1, random = ~ outcome | trial, 
                 struct="UN", data=data, method="ML")
# Notes:
# - The interest lies in the '(raw) mean difference'.
#   This will be denoted by 'theta'.
# - The function rma.mv() uses restricted maximum likelihood (REML) estimation 
#   by default, so method="ML" must be explicitly requested, 
#   which is used to mimic Berkey et al. (1998).

# Hypothesis/-es
#
# Set 1
# H1.1: theta_AL < 0; theta_PD > 0 
# H1.2: theta_AL < 0; theta_PD < 0
#
# Set 2
# H2: |theta_AL| > 0.2; |theta_PD| > 0.2
#
# Set 3
# H3: |theta_AL| < |theta_PD|


print(metaan, digits=3)
#
#The results show that the amount of heterogeneity 
#in the attachment level (AL) outcome (i.e., .026) is larger than 
#the amount of heterogeneity in the probing depth (PD) outcome (i.e., .007). 
#Furthermore, the true outcomes appear to correlate quite strongly (i.e., .70). 
#On average, surgery is estimated to lead to 
#significantly greater decreases in probing depth (i.e., .35), 
#but non-surgery is more effective in increasing the attachment level (i.e., -.34).


###


# Null hypothesis testing results #

# Simultaneous test for 'H0: theta_AL = 0 and theta_PD = 0', 
# which now equals the omnibus test which is part of the output:
#Test of Moderators (coefficients 1:2):
#  QM(df = 2) = 155.773, p-val < .001
# or by using:
anova(metaan)
#anova(metaan, btt=1:2)

# Reject H0: theta_AL = 0 and theta_PD = 0.
# Cannot quantify the support for H0, nor for
# H1.1 <- "theta_AL < 0; theta_PD > 0" 
# H1.2 <- "theta_AL < 0; theta_PD < 0" 



# AIC #

# Notes: 
#
# 1)
# metafor can give IC values as well:
#  metaan$fit.stats # for model that is estimated
# but are not helpful here:
# Metafor cannot create a competing model with constraints on the parameters! 
# It can compare different REs models or FE vs RE, 
# but that is not of interest here.
#
# 2)
# MASEM can provide the AIC values, but
# - one needs to specify and run each model separately.
# - one needs to calculate the AIC weights (e.g., by using the ICweights package).
#
# Therefore, I will use GORICA weights (AIC weights) as proxy to Akaike weights.

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta_AL", "theta_PD")
VCOV_est <- vcov(metaan)

# Hypothesis of interest - no order restriction, because AIC is used.
#
# Set 1
# H1.1 <- "theta_AL < 0; theta_PD > 0" 
# H1.2 <- "theta_AL < 0; theta_PD < 0" 
H01 <- "theta_AL == 0; theta_PD == 0" # i.e., theta_AL == 0, theta_PD == 0
H02 <- "theta_PD == 0" # i.e., theta_AL, theta_PD == 0
H03 <- "theta_AL == 0" # i.e., theta_AL == 0, theta_PD 
# Note: By default, the unconstrained hypothesis is added to the set.
#
# Set 2
# H2 <- "abs(theta_AL) > 0.2; abs(theta_PD) > 0.2"
H04 <- "abs(theta_AL) == 0.2; abs(theta_PD) == 0.2"
# Note: This can be compared to its complement, but because of the equality,
# the complement equals the unconstrained hypothesis.
#
# Set 3
# H3 <- "abs(theta_AL) < abs(theta_PD)"
H05 <- "abs(theta_AL) == abs(theta_PD)"
# Note: This can be compared to its complement, but because of the equality,
# the complement will equal the unconstrained hypothesis.

# Apply GORICA to obtain AIC weights
#
# Set 1
results_AIC_Set1 <- goric(est, VCOV = VCOV_est, 
                          hypotheses = list(H01 = H01, H02 = H02, H03 = H03), 
                          type = "gorica") 
results_AIC_Set1
#
# Set 2
results_AIC_Set2 <- goric(est, VCOV = VCOV_est, 
                          hypotheses = list(H04 = H04), 
                          comparison = "complement", 
                          type = "gorica") 
results_AIC_Set2
#
# Set 3
results_AIC_Set3 <- goric(est, VCOV = VCOV_est, 
                          hypotheses = list(H05 = H05), 
                          comparison = "complement", 
                          type = "gorica") 
results_AIC_Set3
#
#
# Set 1
#  Results:
#           model   loglik  penalty   gorica  gorica.weights
#1            H01  -73.975    0.000  147.950           0.000
#2            H02  -20.393    1.000   42.787           0.000
#3            H03   -5.063    1.000   12.127           0.000
#4  unconstrained    3.912    2.000   -3.824           1.000
#---
# From this, it is concluded that the unconstrained hypothesis is 
# the best hypothesis and has even full support. 
# This implies that the other three hypotheses are weak hypotheses.
#
# Set 2
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1         H04  -9.560    0.000  19.120           0.000
#2  complement   3.912    2.000  -3.824           1.000
#---
#  The order-restricted hypothesis ‘H04’ has  0.000 times more support 
#                                                   than its complement.
# Hence, there is no support for $H_{04}$, or stated differently,
# there is full support for the unconstrained (the complement of $H_{04}$).
# Note that the unconstrained contains both values above and below 0.2.
#
# Set 3
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   3.910    1.000  -5.820           0.731
#2  complement   3.912    2.000  -3.824           0.269
#---
#  The order-restricted hypothesis ‘H0’ has  2.713 times more support 
#                                                 than its complement.
# This may not seem to be convincing evidence, 
# but this has to do with evaluating an equality. 
# First, equality restrictions are hardly ever exactly true. 
# This can be seen from the maximum log likelihood, 
# which will be just below the maximum of the maximum log likelihood 
# (i.e., the maximum log likelihood of the complement 
# which is the unconstrained in this case). 
# Second, when comparing to the unconstrained hypotheses 
# (or another hypothesis which includes the one of interest), 
# there is a maximum support based solely on the penalty of the hypotheses. 
#
#
# Notably, in all sets, there is no quantification for (lack of) support for 
# the hypotheses of interest (only the equality-restricted hypotheses).



# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta_AL", "theta_PD")
VCOV_est <- vcov(metaan)


# Set 1.1

# Hypothesis of interest
H1.1 <- "theta_AL < 0; theta_PD > 0" 

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1.1 <- goric(est, VCOV = VCOV_est, 
                      hypotheses = list(H1.1 = H1.1), 
                      comparison = "complement", 
                      type = "gorica") 
results_H1.1
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1        H1.1   3.912    0.799  -6.226           1.000
#2  complement  -5.063    1.701  13.529           0.000
#---
#  The order-restricted hypothesis ‘H1.1’ has 19482.703 times more support 
#                                                      than its complement.
#
# Thus, there is overwhelming (full) support for H1 versus its complement.
# Note that the complement contains "theta_AL > 0; theta_PD < 0", 
# but is not equal to it, since it also contains 
# "theta_AL < 0; theta_PD < 0"  and "theta_AL > 0; theta_PD > 0".


# Set 1.2

# Hypothesis of interest
H1.1 <- "theta_AL < 0; theta_PD > 0" 
H1.2 <- "theta_AL < 0; theta_PD < 0" 
# Note: By default, the unconstrained hypothesis is added to the set.

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H1.1 = H1.1, H1.2 = H1.2), 
                    type = "gorica") 
#summary(results_H1)
#
results_H1
round(results_H1$ratio.gw, digits = 2)
#
#  Results:
#           model   loglik  penalty  gorica  gorica.weights
#1           H1.1    3.912    0.799  -6.226           0.769
#2           H1.2  -20.393    1.201  43.189           0.000
#3  unconstrained    3.912    2.000  -3.824           0.231
#
#> round(results_H1$ratio.gw, digits = 2)
#              vs. H1.1    vs. H1.2 vs. unconstrained
#H1.1               1.0 53728634328              3.32
#H1.2               0.0           1              0.00
#unconstrained      0.3 16165185252              1.00


# Set 2

# Hypothesis of interest
H2 <- "abs(theta_AL) > 0.2; abs(theta_PD) > 0.2"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H2 = H2), comparison = "complement", 
                    type = "gorica") 
results_H2
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   3.912    0.799  -6.226           0.917
#2  complement   2.417    1.701  -1.431           0.083
#---
#  The order-restricted hypothesis ‘H2’ has 10.996 times more support 
#                                                 than its complement.
#
# Thus, there is compelling support for H2.


# Set 3

# Hypothesis of interest
H3 <- "abs(theta_AL) < abs(theta_PD)"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H3 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H3 = H3), comparison = "complement", 
                    type = "gorica") 
results_H3
# Weights based on penalty values
IC.weights(2*results_H3$result[,3])$IC.weights 
# Note that the penalty is 2*`penalty'
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H3   3.912    1.500  -4.824           0.500
#2  complement   3.910    1.500  -4.820           0.500
#---
#  The order-restricted hypothesis ‘H3’ has  1.002 times more support 
#                                                 than its complement.
#
# Thus, both hypotheses are equally likely. 
# The log likelihood values are nearly the same and 
# thus the weights largely depend on the penalty values.
# Since both hypotheses are of the same size (i.e., have the same penalty), 
# the weights are the same.
# This implies support for the border of the hypotheses: |theta_AL| = |theta_PD| 
# (i.e., equal absolute strength).


# Alternative

# Hypothesis of interest
# Instead of inspecting H0 (with AIC) one can also inspect an about-equality, 
# that is, a range restriction, with the GORICA.
# The range restriction here will be based on the standard error of theta_PD:
# sqrt(diag(VCOV_est)) # se(theta_PD) = 0.04945984
H0_range <- "abs(theta_AL) > abs(theta_PD) - 0.1*0.04945984; 
             abs(theta_AL) < abs(theta_PD) + 0.1*0.04945984"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_range <- goric(est, VCOV = VCOV_est, 
                       hypotheses = list(H0_range = H0_range), 
                       comparison = "complement", 
                       type = "gorica") 
results_range
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1    H0_range   3.912    1.000  -5.823           0.731
#2  complement   3.912    2.000  -3.824           0.269
#---
#  The order-restricted hypothesis ‘H0_range’ has  2.718 times more support 
#                                                       than its complement.
#
#Restriktor message: Since the constraint matrix is not full row-rank, 
#the level probabilities are calculated using mix.weights = "boot" 
#(the default is mix.weights = "pmvnorm").
#For more information see ?restriktor.
#
# Notably, in case a hypothesis is not a closed convex cone, 
# which is the case for range restrictions, 
# the restriction matrix is not full row-rank. 
# In that case, the penalty is calculated using another function 
# (nl, using bootstrap).
#
#
# Since the loglik of the two hypotheses are very close 
#(and in case of using the unconstrained instead of the complement, even equal)
# The gorica.weights approximately equal the penalty weights:
round( IC.weights(2*results_range$result[,3])$IC.weights, 3)
# Note that the penalty is 2*`penalty'
#   H1    H2 
#0.731 0.269
#
# Therefore, there is support for the boundary/overlap of these hypotheses, 
# which implies H0.


########


#Multivariate Mixed-Effects Meta-Regression Model
#
# A mixed-effects model using year of publication as moderator:
metaan <- rma.mv(yi, V, mods = ~ outcome + outcome:I(year - 1983) - 1, 
                 random = ~ outcome | trial, struct="UN", data=data, method="ML")
print(metaan, digits=3)
#Note that publication year was centered at 1983, as was done by the authors. 
#
#To be able to test whether the slope of publication year actually differs 
#for the two outcomes, we can fit the same model with:
res <- rma.mv(yi, V, mods = ~ outcome*I(year - 1983) - 1, 
              random = ~ outcome | trial, struct="UN", data=data, method="ML")
print(res, digits=3)
#The output is identical, except for the last part
#the slope is actually not significantly different for the two outcomes (p = .553). 
#In fact, it does not appear as if publication year is at all related to the two outcomes.

# Notes: 
#
# 1) 
# The interest lies in the '(raw) mean difference'
# and in the effect/slope of the moderator publication year.
# This will be denoted by 'theta_..' and 'beta_Year_..', respectively.
#
# 2)
# If the effect/slope of publication year will be compared 
# to a specific non-zero value or to each other,
# then one needs to standardize this variable.


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta_AL", "theta_PD", "beta_Year_AL", "beta_Year_PD")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# H1 <- "theta_AL < 0; theta_PD > 0; abs(beta_Year_AL) > abs(beta_Year_PD)"
H0 <- "theta_AL == 0; theta_PD == 0; beta_Year_AL == beta_Year_PD"

# Apply GORICA to obtain AIC weights
# set.seed(123) # set seed: to obtain the same results when you re-run it
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#Results:
#        model   loglik  penalty   gorica  gorica.weights
#1          H0  -61.532    1.000  125.063           0.000
#2  complement   10.119    4.000  -12.238           1.000
#---
#  The order-restricted hypothesis ‘H0’ has  0.000 times more support 
#                                                 than its complement.
#
# So, full support for complement, which is the unconstrained in this case.


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("theta_AL", "theta_PD", "beta_Year_AL", "beta_Year_PD")
VCOV_est <- vcov(metaan)

# Hypothesis of interest 
H1 <- "theta_AL < 0; theta_PD > 0; abs(beta_Year_AL) > abs(beta_Year_PD)"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica")
results
#
#Results:
#        model  loglik  penalty   gorica  loglik.weights  penalty.weights  gorica.weights
#1          H1  10.119    2.806  -14.625           1.000            0.708           1.000
#2  complement   1.041    3.693    5.304           0.000            0.292           0.000
#--- 
#  The order-restricted hypothesis ‘H1’ has 21259.850 times more support 
#                                                 than its complement.
#
#Restriktor message: Since the constraint matrix is not full row-rank, the level probabilities 
#are calculated using mix.weights = "boot" (the default is mix.weights = "pmvnorm").
#For more information see ?restriktor.


#Alternative:
est <- coef(metaan)
names(est) <- c("theta_AL", "theta_PD", "beta_Year_AL", "beta_Year_PD")
VCOV_est <- vcov(metaan)
H1_mod <- "beta_Year_AL < beta_Year_PD"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_mod <- goric(est, VCOV = VCOV_est, 
                     hypotheses = list(H1_mod = H1_mod), comparison = "complement", 
                     type = "gorica")
results_mod
#
#Results:
#        model  loglik  penalty   gorica  loglik.weights  penalty.weights  gorica.weights
#1      H1_mod  10.119    3.500  -13.238           0.544            0.500           0.544
#2  complement   9.943    3.500  -12.887           0.456            0.500           0.456
#--- 
#  The order-restricted hypothesis ‘H1_mod’ has 1.192 times more support 
#                                                     than its complement.
#
#
# Note that this is the same as:
est_res <- coef(res)
names(est_res) <- c("theta_AL", "theta_PD", "beta_Year_AL", "beta_Year_diff")
VCOV_est_res <- vcov(res)
H1_mod_res <- "beta_Year_diff > 0"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_mod_res <- goric(est_res, VCOV = VCOV_est_res, 
                         hypotheses = list(H1_mod_res = H1_mod_res), 
                         comparison = "complement", type = "gorica")
results_mod_res
#
#Results:
#        model  loglik  penalty   gorica  loglik.weights  penalty.weights  gorica.weights
#1  H1_mod_res  10.119    3.500  -13.238           0.544            0.500           0.544
#2  complement   9.943    3.500  -12.887           0.456            0.500           0.456
#--- 
#  The order-restricted hypothesis ‘H1_mod_res’ has 1.192 times more support  
#                                                         than its complement.


#######################################


## Example van Houwelingen et al. (2002) ##
# based on http://www.metafor-project.org/doku.php/analyses:vanhouwelingen2002
#
# Data
data <- dat.colditz1994
data
#
# Calculate the individual log odds ratios and corresponding sampling variances with:
data <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=data)
#
# In addition, we can recode the year variable, center it at 1900:
data$year <- data$year - 1900
#
# A random-effects (using maximum likelihood estimation):
metaan <- rma(yi, vi, data=data, method="ML")
metaan

# Note: 
# The interest lies in the 'log odds'.
# This will be denoted by 'LogOdds'.


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# H1 <- "LogOdds < 0"
H0 <- "LogOdds == 0" 

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0  -7.885    0.000  15.770           0.000
#2  complement   0.807    1.000   0.385           1.000
#---
#  The order-restricted hypothesis ‘H0’ has  0.000 times more support 
#                                                 than its complement.
#
# So, no support for H0 and full support for Hunc. 
# Notably, no quantification for (lack of) support for 
# the hypothesis of interest H1 (or H2).


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
H1 <- "LogOdds < 0" 

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica") 
results
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   0.807    0.500  -0.615           1.000
#2  complement  -7.885    0.500  16.770           0.000
#---
#  The order-restricted hypothesis ‘H1’ has 5955.893 times more support 
#                                                   than its complement.
#
# Thus, there is overwhelming (full) support for H1 versus its complement 
# (i.e., LogOdds > 0).


# In case you want to specify some value 
# based on literature or expertise:

# Hypothesis of interest
H2 <- "LogOdds < -0.5" 

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H2 = H2), comparison = "complement", 
                    type = "gorica") 
results_H2
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   0.807    0.500  -0.615           0.716
#2  complement  -0.117    0.500   1.234           0.284
#---
#  The order-restricted hypothesis ‘H2’ has  2.520 times more support 
#                                                 than its complement.
#
# Now, less support of course (note: complement is here 'LogOdds > -0.5').


########


# Meta-Regression
#
# It may be possible to account for (at least part of) 
# the heterogeneity in the treatments effects (i.e., log odds ratios) 
# based on one or more moderator variables 
# (i.e., study characteristics that may influence the size of the effect). 
 
# A meta-regression model, with the absolute latitude of the study location 
# and publication year as predictors of the treatment effect:
#metaan <- rma(yi, vi, mods = ~ ablat + year, data=data, method="ML")
# Remarks: 
# - year was centered at 1900
# - ablat denotes absolute latitude
# - year and ablat are somewhat correlated 
#   (i.e., cor(data$year, data$ablat) = approx -0.66).
#metaan

# Notes: 
#
# 1) 
# The interest lies in the 'log odds' (for the reference situation*)
# and in the effect/slopes of the moderators year and ablat.
# * the situation where the (continuous) moderators take on their mean levels.
# This will be denoted by 'LogOdds_ref' and 'IncreaseLogOddsPer...', respectively.
#
# 2)
# If the effect/slope of year and/or ablat will be compared 
# to a specific non-zero value or to each other,
# then one needs to standardize these variables.

# Data change
data$year <- scale(data$year)
data$ablat <- scale(data$ablat)

# Meta-Regression
metaan <- rma(yi, vi, mods = ~ ablat + year, data=data, method="ML")
# Remarks: 
# - year is standardized
# - ablat denotes absolute latitude and is standardized
# - year and ablat are somewhat correlated 
#   (i.e., cor(data$year, data$ablat) = approx -0.66).
metaan


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds_ref", "IncreaseLogOddsPerYear", "IncreaseLogOddsPerAblat")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# H1 <- "abs(IncreaseLogOddsPerYear) > abs(IncreaseLogOddsPerAblat)"
H0 <- "abs(IncreaseLogOddsPerYear) == abs(IncreaseLogOddsPerAblat)"

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#Results:
#        model   loglik  penalty   gorica  gorica.weights
#1          H0  -34.256    2.000  72.512           0.000
#2  complement    6.065    3.000  -6.130           1.000
#---
#  The order-restricted hypothesis ‘H0’ has  0.000 times more support 
#                                                 than its complement.
#
# So, no support for H0 and full support for its complement (i.e., Hunc). 
# Notably, no quantification for (lack of) support for the hypothesis of interest H1.


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("LogOdds_ref", "IncreaseLogOddsPerYear", "IncreaseLogOddsPerAblat")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
H1 <- "abs(IncreaseLogOddsPerYear) > abs(IncreaseLogOddsPerAblat)" 

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H1 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H1), comparison = "complement", 
                    type = "gorica") 
results_H1
#
#Results:
#        model   loglik  penalty   gorica  gorica.weights
#1          H1    6.065    2.500  -7.130           1.000
#2  complement  -34.256    2.500  73.512           0.000
#---
#  The order-restricted hypothesis ‘H1’ has 324506692665273152.000 
#                           times more support than its complement.


# In case you want to specify some value:

# Hypothesis of interest
H2 <- "abs(IncreaseLogOddsPerYear) - abs(IncreaseLogOddsPerAblat) > 0.2"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H2 = H2), comparison = "complement", 
                    type = "gorica") 
results_H2
#
#Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H2   6.065    2.500  -7.130           1.000
#2  complement  -7.202    2.500  19.403           0.000
#---
#  The order-restricted hypothesis ‘H2’ has 577632.025 times more support 
#                                                     than its complement.


#######################################


## Example Viechtbauer (2007) based on Linde et al. (2005) ##
# Based on http://www.metafor-project.org/doku.php/analyses:viechtbauer2007b
#
# Data
data <- escalc(measure="RR", ai=ai, ci=ci, n1i=n1i, n2i=n2i, data=dat.linde2005)
data <- data[c(7:10,13:25), c(13:16,18:19,11,6,7,9)]
data$dosage <- (data$dosage * 7) / 1000
rownames(data) <- 1:nrow(data)
data
#Variables ai and ci indicate the number of participants with 
#significant improvements between baseline and the follow-up assessment 
#in the treatment and the placebo group, respectively, 
#variables n1i and n2i are the corresponding group sizes, 
#variable yi is the log of the relative improvement rate 
#(i.e., the improvement rate in the treatment group divided by 
#the improvement rate in the placebo group), 
#vi is the corresponding sampling variance, 
#dosage is the weekly dosage (in grams) of the Hypericum extract used in each study, 
#major indicates whether a study was restricted to participants with 
#major depression or not (1 or 0, respectively), 
#baseline denotes the average score on the 
#Hamilton Rating Scale for Depression (HRSD) at baseline (i.e., before treatment begin), 
#and duration indicates the number of treatment weeks before response assessment. 
#Variables yi and vi are not actually included in the original dataset 
#and were added by means of the escalc() function.
#
#Note that, for illustration purposes, only a subset of the data from 
#the Linde et al. (2005) meta-analysis are actually included in this example. 
#Therefore, no substantive interpretations should be attached 
#to the results of the analyses given below.
#
#
#Meta-Regression
#Assuming that the heterogeneity in the true (log) relative rates is not random, 
#that is, assuming that differences in the relative rates are (at least in part) 
#systematic and related to study-level variables (moderators), 
#such as the treatment intensity and the severity of the depression 
#in the group of patients being treated (and their interaction). 

#Notably, treatment intensity will be expressed in terms of a single moderator 
#that indicates the total dosage (in grams) administered during the course of each study:
data$dosage <- data$dosage * data$duration
data
#
#cor(data$dosage, data$dosage*data$baseline) # 0.9679045
#cor((data$dosage-34), (data$dosage-34)*(data$baseline-20)) # 0.6871161
#
# A mixed-effects meta-regression:
metaan <- rma(yi, vi, mods = ~ I(dosage-34) * I(baseline-20), data=data, method="DL")
metaan
#
# Null hypothesis testing results:
#It appears that St. John's wort is more effective for lower baseline HRSD scores 
#(the coefficient is negative, but just misses being significant at α = .05 with p = .06). 
#On the other hand, the total dosage of St. John's wort administered 
#during the course of a study does not appear to be related to 
#the treatment effectiveness (p= .56) 
#and there does not appear to be an interaction between the two moderators (p = .65).

# Notes: 
#
# 1) 
# The interest lies in the 'risk ratio' (of the reference situation*)
# and in the effect/slopes of the moderators: 
# dosage, baseline (depression), and their interaction.
# * the situation where the (continuous) moderators take on their mean values.
# This will be denoted by 'RR_ref' and 'beta_...', respectively.
#
# 2)
# If the effect/slope of the moderators will be compared 
# to a specific non-zero value or to each other,
# then one needs to standardize these variables.
#
# Data change
#data$dosage <- scale(data$dosage)
#data$baseline <- scale(data$baseline)
#
# A mixed-effects meta-regression:
#metaan <- rma(yi, vi, mods = ~ dosage * baseline, data=data, method="DL")
#metaan


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("RR_ref", "beta_dosage", "beta_baseline", "beta_interaction")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# H1 <- "beta_baseline < 0; beta_dosage < 0; beta_interaction == 0"
H0 <- "beta_baseline == 0; beta_dosage == 0; beta_interaction == 0" 
# stating that they are equal to 0.

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H0   8.318    1.000  -14.635           0.113
#2  complement  13.382    4.000  -18.763           0.887
#---
#  The order-restricted hypothesis ‘H0’ has  0.127 times more support 
#                                                 than its complement.
#
# So, some support for Ho and quite some support for Hunc, 
# which includes the hypothesis of interest H1.
# Notably, no quantification for (lack of) support for 
# the hypothesis of interest H1.


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("RR_ref", "beta_dosage", "beta_baseline", "beta_interaction")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
H1 <- "beta_baseline < 0; beta_dosage < 0; beta_interaction == 0"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica") 
results
#
#Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H1  13.278    1.917  -22.722           0.827
#2  complement  13.382    3.583  -19.597           0.173
#---
#  The order-restricted hypothesis ‘H1’ has  4.769 times more support 
#                                                 than its complement.


# It is possible to have competing hypotheses (but be aware in case of overlapping ones):
H1a <- "beta_baseline < 0; beta_dosage == 0; beta_interaction == 0" 
H1b <- "beta_baseline < 0; beta_dosage < 0; beta_interaction == 0" 
set.seed(123) # set seed: to obtain the same results when you re-run it
results_ab <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H1a = H1a, H1b = H1b),  
                    type = "gorica") # default: unconstrained
results_ab
#
#Results:
#           model  loglik  penalty   gorica  gorica.weights
#1            H1a  11.609    1.500  -20.219           0.201
#2            H1b  13.278    1.917  -22.722           0.702
#3  unconstrained  13.382    4.000  -18.763           0.097
#---
#
# Both H1a and H1b not weak, especially H1b (.702 / .097 >> 1).
# Note: H1a is a subset of H1b, which implies that 
# all support for H1a is also support for H1b.
# Thus, compelling evidence for H1b, 
# as we saw when evaluating H1b (i.e., H1) against its complement.


# As another example:
H2 <- "beta_baseline < 0" # so, other two parameters are free / unrestricted
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H2 = H2), comparison = "complement", 
                    type = "gorica") 
results_H2
#
#Results:
#        model  loglik  penalty   gorica  gorica.weights
#1          H2  13.382    3.500  -19.763           0.861
#2  complement  11.560    3.500  -16.120           0.139
#---
#  The order-restricted hypothesis ‘H2’ has  6.181 times more support 
#                                                 than its complement.


########


# Instead of looking at a linear effect of dosage, 
# one can also make groups (based on theory).
# I am not an expert, so for ease I split up the dosage variable up in to 3 groups:
# 0-30, 30-40, 40-70.
# Notably, one could have chosen other (and/or another number of) cut-off values.
data$dosage3 <- cut(data$dosage, breaks = c(0, 30, 40, 70))
metaan <- rma(yi, vi, mods = ~ I(dosage3) * I(baseline-20), data=data, method="DL")
metaan


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("RR_ref", "beta_dosage2", "beta_dosage3", "beta_baseline", 
                "beta_interact2", "beta_interact3")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# Expectation: Increasing dose a bit lowers the mean RR compared to RR_ref, 
# but increasing dose too much will increase the RR compared to RR_ref.
# Hence, the mean RR for a medium dose is lower than for small dose 
# and also for a large dose.
# That is, RR_ref + beta_dosage2 < RR_ref and 
# RR_ref + beta_dosage2 < RR_ref + beta_dosage3.
# Thus, beta_dosage2 < 0 and beta_dosage2 < beta_dosage3.
# Because of equalities: beta_dosage2 == 0 and beta_dosage2 == beta_dosage3
#
# H1 <- "beta_dosage2 < 0; beta_dosage2 < beta_dosage3"
H0 <- "beta_dosage2 == 0; beta_dosage2 == beta_dosage3" 

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   4.941    4.000  -1.882           0.551
#2  complement   6.736    6.000  -1.472           0.449
#---
#  The order-restricted hypothesis ‘H0’ has  1.227 times more support 
#                                                 than its complement.
#
# So, support for H0 but not very compelling evidence. 
# Moreover, no quantification for (lack of) support for the hypothesis of interest H1.


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("RR_ref", "beta_dosage2", "beta_dosage3", "beta_baseline", 
                "beta_interact2", "beta_interact3")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# Expectation: Increasing dose a bit lowers the mean RR compared to RR_ref, 
# but increasing dose too much will increase the RR compared to RR_ref.
# Hence, the mean RR for a medium dose is lower than for small dose 
# and also for a large dose.
# That is, RR_ref + beta_dosage2 < RR_ref and 
# RR_ref + beta_dosage2 < RR_ref + beta_dosage3.
# Thus, beta_dosage2 < 0 and beta_dosage2 < beta_dosage3.
H1 <- "beta_dosage2 < 0; beta_dosage2 < beta_dosage3"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica") 
results
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   6.736    5.010  -3.453           0.785
#2  complement   5.922    5.490  -0.863           0.215
#---
#  The order-restricted hypothesis ‘H1’ has  3.652 times more support 
#                                                 than its complement.


#######################################


## Example 'Testing Factors and Linear Combinations of Parameters' ##
# Dataset for the BCG vaccine meta-analysis (Colditz et al., 1994)
# Based on http://www.metafor-project.org/doku.php/tips:testing_factors_lincoms?s[]=ano
#
#Testing Factors
#
#The (log) risk ratios and corresponding sampling variances:
#library(metafor)
data <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
data
#
#For easier interpretation, "center" moderators at their minimum values 
#(1948 and 13 degrees, respectively):
data$year  <- data$year  - 1948
data$ablat <- data$ablat - 13
#
# A mixed-effects meta-regression model with moderators:
# publication year (year; continuous), absolute latitude (ablat; continuous), and 
# the method of treatment allocation (alloc; a factor / grouping variable) 
metaan <- rma(yi, vi, mods = ~ factor(alloc) + year + ablat, data=data, method="DL")
metaan
#
#By default, alternate was made the reference level 
#(since the first letter "a" comes before "r" and "s"), 
#so the corresponding dummy variable for this level has been left out.
#Therefore, the model intercept (coefficients b0) is the estimated (average) 
#log risk ratio for alternate allocation in 1948 at 13 degrees absolute latitude. 
#Coefficients b1 and b2 indicate how much lower/higher the estimated (average) 
#log risk ratio is for random and systematic allocation, respectively, 
#compared to alternate allocation. 
#Finally, the coefficients for year and absolute latitude (i.e., b3 and b4) 
#estimate how the (average) log risk ratio changes for a one-unit increase 
#in the respective moderator.
#
#The results under "Test of Moderators" indicate that 
#we can (just) reject the null hypothesis 
#H0: β1 = β2 = β3 = β4 = 0
#(i.e., this is a so-called omnibus test of coefficients 2 through 5 – 
#the first being the intercept, which is typically denoted as β0 in model equations 
#and is not included in the test by default). 
#In other words, it appears as if at least part of the heterogeneity 
#in the true effects is related to some of the predictors/moderators 
#included in the model.
#
#Now consider the two coefficients for the allocation factor. 
#Neither coefficient is significantly different from 0 
#(as indicated by the corresponding p-values), 
#but this isn't a proper test of the factor as a whole. 
#Instead, we want to simultaneously test whether both coefficients are 
#simultaneously equal to 0. We can do this with:
#anova(metaan, btt=2:3)
#This is a (Wald-type) chi-square test of the null hypothesis 
#H0: β1 = β2 = 0
#with two degrees of freedom. 
#It indicates that the factor as a whole is not significant.
#
# Notes:
# - There are also alternatives in R, leading of course to the same result.
# - One could also use 'Knapp & Hartung Adjustment', leads to same conclusion.
# - One could also perform a Likelihood Ratio Test to investigate whether a variable should be included or not.
#
#Testing Linear Combinations
#One can also test linear combinations of model coefficients. 
#For example, one could test whether random and systematic allocation lead to 
#significantly different effects with:
anova(metaan, L=c(0,1,-1,0,0))
#Apparently, there is no significant difference between random and systematic allocation.
#
#As another example, we could test whether the estimated average log risk ratio 
#based on random allocation in 1970 at an absolute latitude of 30 degrees is 
#significantly different from zero. 
#Recall that these two moderators were "centered" at 1948 and 13, respectively. 
#So, this linear hypothesis can be tested with:
anova(metaan, L=c(1,1,0,1970-1948,30-13)) 
# intrcpt + factor(alloc)random + 22*year + 17*ablat = 0
#So, the estimated average log risk ratio for this combination of moderator values is 
#significantly different from zero (-0.8013, QM(df = 1) = 16.4714, p-val < .0001).
#
#We can obtain the corresponding estimated average log risk ratio with:
tmp <- predict(metaan, newmods=c(1,0,1970-1948,30-13))
tmp
#In addition, the corresponding standard error, confidence interval, and 
#prediction interval are given. Note that the intercept is automatically added 
#(by default) and hence should not be included in the vector specified for 
#the newmods argument for the predict() function.
#
#
#Notably, specifying hypotheses can be easier when 
#the log risk ratios (logRR) is estimated per group 
#(instead of a logRR for the reference group and then 
#the differences in logRR with the other groups).
#Then, one should use the following specifications 
#(and specify the hypotheses accordingly)
metaanG <- rma(yi, vi, mods = ~ factor(alloc) + year + ablat - 1, data=data, method="DL")
metaanG


# Notes: 
#
# 1) 
# The interest lies in the 'log risk ratio' (of the reference situation*)
# and in the effect/slopes of the moderators:
# publication year (year; continuous), absolute latitude (ablat; continuous), and 
# the method of treatment allocation (alloc; a factor / grouping variable) 
# * the situation where the continuous moderators take on their mean values.
# This will be denoted by 'logRR_ref' and 'beta_...', respectively.
#
# 2)
# If the effect/slope of the moderators will be compared 
# to a specific non-zero value or to each other,
# then one needs to standardize these variables.

# Data change
data$year <- scale(data$year)
data$ablat <- scale(data$ablat)
#
# A mixed-effects meta-regression:
metaan_orig <- metaan # store it, since it is needed again later on
metaan <- rma(yi, vi, mods = ~ factor(alloc) + year + ablat, data=data, method="DL")
metaan
metaanG <- rma(yi, vi, mods = ~ factor(alloc) + year + ablat - 1, data=data, method="DL")
metaanG


# AIC - to see whether variable allocation has predictive power #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("logRR_ref", "beta_allocR", "beta_allocS", 
                "beta_year", "beta_ablat")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
H0_alloc <- "beta_allocR == 0; beta_allocS == 0" 
# 
# Apply GORICA to obtain AIC weights
results_H0_alloc <- goric(est, VCOV = VCOV_est, 
                          hypotheses = list(H0_alloc = H0_alloc), 
                          comparison = "complement", 
                          type = "gorica") 
results_H0_alloc
#
#Results:
#        model  loglik  penalty  gorica  gorica.weights
#1    H0_alloc   2.567    3.000   0.865           0.784
#2  complement   3.276    5.000   3.447           0.216
#---
#  The order-restricted hypothesis ‘H0_alloc’ has  3.636 times more support 
#                                                       than its complement.
#
# So, (some) support for H0_alloc. 


# In case of the logRR per group specification #

#Subtract estimates from meta-an, to be used in goric function
estG <- coef(metaanG)
names(estG) <- c("beta_allocA", "beta_allocR", "beta_allocS", 
                 "beta_year", "beta_ablat")
VCOV_estG <- vcov(metaanG)

# Hypothesis of interest
H0_alloc_G <- "beta_allocA == beta_allocR; beta_allocR == beta_allocS" 
# 
# Apply GORICA to obtain AIC weights
results_H0_alloc_G <- goric(estG, VCOV = VCOV_estG, 
                            hypotheses = list(H0_alloc_G = H0_alloc_G), 
                            comparison = "complement", 
                            type = "gorica") 
results_H0_alloc_G
# Which evidently renders the same result as above.


# AIC #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("logRR_ref", "beta_allocR", "beta_allocS", 
                "beta_year", "beta_ablat")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# One could argue that the logRR for 
# the random allocation is the lowest, implying:
#H1 <- "beta_allocR < 0; beta_allocR < beta_allocS"
# Because of needing equalities, this reduces to:
H0 <- "beta_allocR == 0; beta_allocR == beta_allocS" 

# Apply GORICA to obtain AIC weights
results_H0 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H0 = H0), comparison = "complement", 
                    type = "gorica") 
results_H0
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H0   2.567    3.000   0.865           0.784
#2  complement   3.276    5.000   3.447           0.216
#---
#  The order-restricted hypothesis ‘H0’ has  3.636 times more support 
#                                                 than its complement.
#
# So, support for H0. 
# Notably, no quantification for (lack of) support for 
# the hypothesis of interest H1.


# GORICA #

#Subtract estimates from meta-an, to be used in goric function
est <- coef(metaan)
names(est) <- c("logRR_ref", "beta_allocR", "beta_allocS", 
                "beta_year", "beta_ablat")
VCOV_est <- vcov(metaan)

# Hypothesis of interest
# One could argue that the logRR for 
# the random allocation is the lowest, implying:
H1 <- "beta_allocR < 0; beta_allocR < beta_allocS"

# Apply GORICA
set.seed(123) # set seed: to obtain the same results when you re-run it
results <- goric(est, VCOV = VCOV_est, 
                 hypotheses = list(H1), comparison = "complement", 
                 type = "gorica") 
results
#
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H1   3.276    4.104   1.655           0.659
#2  complement   2.908    4.396   2.975           0.341
#---
#  The order-restricted hypothesis ‘H1’ has  1.935 times more support 
#                                                 than its complement.
#
# Some support for H1


#As an alternative example:
H2 <- "abs(beta_ablat) > abs(beta_year)"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H2 <- goric(est, VCOV = VCOV_est, 
                    hypotheses = list(H2 = H2), comparison = "complement", 
                    type = "gorica") 
results_H2
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1          H2   3.276    4.500   2.447           0.574
#2  complement   2.979    4.500   3.042           0.426
#---
#  The order-restricted hypothesis ‘H2’ has  1.346 times more support 
#                                                 than its complement.
#
# Hence, no compelling support for H2.


#As an alternative example, one can specify both simultaneously:
H12 <- "beta_allocR < 0; beta_allocR < beta_allocS; 
        abs(beta_ablat) > abs(beta_year)"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H12 <- goric(est, VCOV = VCOV_est, 
                     hypotheses = list(H12 = H12), comparison = "complement", 
                     type = "gorica") 
results_H12
#  
#  Results:
#        model  loglik  penalty  gorica  loglik.weights  penalty.weights  gorica.weights
#1         H12   3.276    4.105   1.658           0.591            0.572           0.659
#2  complement   2.908    4.395   2.973           0.409            0.428           0.341
#--- 
#  The order-restricted hypothesis ‘H12’ has 1.930 times more support
#                                                  than its complement.
#
#Restriktor message: Since the constraint matrix is not full row-rank, 
#the level probabilities are calculated using mix.weights = "boot" 
#(the default is mix.weights = "pmvnorm").
#For more information see ?restriktor.
#
# Hence, (some) support for H12.


# Other example:
#
# One could evaluate whether the estimated average log risk ratio 
# based on random allocation 
# in 1970 
# at an absolute latitude of 30 degrees 
# is smaller than 0.
#
# It is important to note that now we need to use the model with unstandardized data,
# since we are predicting scores, which needs 1 steps increase in the moderators
# (instead of standard error steps in the moderators).
est <- coef(metaan_orig)
names(est) <- c("logRR_ref", "beta_allocR", "beta_allocS", 
                "beta_year", "beta_ablat")
VCOV_est <- vcov(metaan_orig)
#
# Recall that, in that model, the two moderators were "centered" 
# at 1948 and 13, respectively. 
#So, this linear hypothesis can be evaluated with:
# intrcpt + factor(alloc)random + (1970-1948)*year + (30-13)*ablat < 0, 
# which equals
# intrcpt + factor(alloc)random + 22*year + 17*ablat < 0, 
# that is,
H3a <- "logRR_ref + beta_allocR + (1970-1948)*beta_year + (30-13)*beta_ablat < 0"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H3a <- goric(est, VCOV = VCOV_est, 
                     hypotheses = list(H3a = H3a), comparison = "complement", 
                     type = "gorica") 
results_H3a
#
#  Results:
#        model     loglik  penalty    gorica  loglik.weights  penalty.weights  gorica.weights
#1         H3a  -1538.106    4.500  3085.213           0.000            0.500           0.000
#2  complement      8.338    4.500    -7.677           1.000            0.500           1.000
#--- 
#  The order-restricted hypothesis ‘H3a’ has 0.000 times more support 
#                                                  than its complement.


# Likewise, one may want to compare two (or more) different settings:
#
#logRR_ref + beta_allocR + 22*beta_year + 17*beta_ablat < 
#logRR_ref + beta_allocS +  5*beta_year +  5*beta_ablat
#implying:
H3b <- "beta_allocR - beta_allocS + (-4)*beta_year + 12*beta_ablat < 0"
set.seed(123) # set seed: to obtain the same results when you re-run it
results_H3b <- goric(est, VCOV = VCOV_est, 
                     hypotheses = list(H3b = H3b), comparison = "complement", 
                     type = "gorica") 
results_H3b
#  
#  Results:
#        model  loglik  penalty  gorica  gorica.weights
#1         H3b   8.338    4.500  -7.677           0.884
#2  complement   6.306    4.500  -3.612           0.116
#---
#  The order-restricted hypothesis ‘H3b’ has  7.633 times more support
#                                                  than its complement.


#######################################

