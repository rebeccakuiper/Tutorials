###

# Needed Packages

# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#install_github("LeonardV/restriktor", force = TRUE)
#
# Or possibly:
if (!require("remotes")) install.packages("remotes") 
library(remotes) 
#remotes::install_github("LeonardV/restriktor", ref = "Branch_Rebec", force = TRUE)
remotes::install_github("LeonardV/restriktor@Branch_Rebec") #, force = TRUE)
library(restriktor) # for evSyn and goric function
#
# If you want to use restriktor from CRAN:
#if (!require("restriktor")) install.packages("restriktor")
#library(restriktor) # for evSyn and goric function
#
#
if (!require("psych")) install.packages("psych")
library(psych) # for descriptive stats

###

# Below, you find some possible answers to the lab material.
# Notably, the lab html also contains code and conclusions.

###

# Exercise 1: Aggregating evidence from two studies
  
# Read in data 
JU <- read.table("JU.txt", header=TRUE)

# Make the variable group a factor #
#Since we loaded a txt file, R does not know the measurement levels of the  
# variables and assumes all to be continuous (so, interval or ratio). 
#Hence, we need to tell R that the variable `group` is a factor 
#           (i.e., a grouping / categorical / nominal variable): 
JU$g <- factor(JU$g)  
# this command tells R that g is a factor and not a continuous variable. 

# Inspect the data #
head(JU) # Look at first (6) rows of the data.

# Compute descriptives for each group #
descrstat <- describeBy(JU$z, JU$g, mat = TRUE)
descrstat


## Compute the GORIC ##

## JU data

# First, we need the R object with unconstrained estimates:
lm_fit_JU <-  lm(z ~ g-1, data = JU) # `lm` stands for linear model.
# Note that:
# 1. `y ~ group - 1` instructs the function `lm` to regress y on group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means in each group, resulting here in five group means. 
#    On the other hand,  `y ~ group' would estimate an intercept, 
#    representing the mean of the reference group, and 
#    the mean differences between the other (here, four) groups and the 
#    reference group.
#    Note that if you would have multiple factors, 
#    you may want to leave the intercept in to improve the interpretation of the parameters; 
#    which aids in specifying your hypotheses.
# 3. The results are collected in, what is called, an R-object, named 
#    `lm_fit_Ju`.

# Check the names used in model
names(coef(lm_fit_JU))
# Specify restrictions using those names.

# Hypotheses Set
#
H1_JU <- 'grh > gph > grl > gpl' 
H2_JU <- 'grh > gph = grl > gpl'  
# Note, these are used as competing hypotheses. 
#
# It is possible to have two theories which are not competing. Say, one 
# addressing comparison between men and women and another one addressing 
# different education levels. Then, do not evaluate them together, but each 
# separately against their own complement.


# Calculate GORIC values and weights
set.seed(123) # Set seed value
goric_JU <- goric(lm_fit_JU, hypotheses = list(H1_JU = H1_JU, H2_JU = H2_JU))

# Show output
goric_JU
#summary(goric_JU)


# Conclusion:
#
# Both H1_JU and H2_JU are not weak hypotheses, since their support is stronger 
# than the support for the unconstrained.
# Since at least one of the competing hypotheses is not weak, one can compare their support.
#
# It can be seen that H1_JU receives the most support. 
# But H2_JU does obtain some support as well.
# More precisely, H1_JU is 2.502 times more supported / more likely than H2_JU.
#
# So, we found support for central theory 'H1'.


### Replication of the JU study: C

# Read in data 
C <- read.table("C.txt", header = TRUE)

C$g <- factor(C$g) 
# this command tells R that 'g' is a factor 
# and not a continuous variable like 'at'

## Inspect data
#head(C)

## Compute descriptives for each group
#descrip <- describeBy(C$z,C$g,mat=TRUE)
#print(descrip)


# lm object (of ANOVA model)
lm_fit_C <-  lm(z ~ g-1, data = C)

# Check names used in model
names(coef(lm_fit_C))
# Specify restrictions using those names


# Hypotheses Set
#
H1_C <- 'grh > gph > grl > gpl' 
H2_C <- 'grh > gph = grl > gpl'  
# Note, these are used as competing hypotheses. 


# Calculate GORIC values and weights
set.seed(123) # Set seed value
goric_C <- goric(lm_fit_C, hypotheses = list(H1_C = H1_C, H2_C = H2_C))

# Show output
goric_C
#summary(goric_C)


# Conclusion:
#
# H1_C is not a weak hypothesis, since its support is stronger than the support  
# for the unconstrained.
# Since at least one of the competing hypotheses is not weak, one can compare 
# their support.
#
# It can be seen that H1_C receives the most support. 
# So, also in the replication study, we found support for central theory 'H1'.


# GORICA Evidence synthesis #

# Extract GORIC values from the studies
GORIC_studies <- list(goric_JU$result[,4], goric_C$result[,4])

# Evidence synthesis
evSyn_anchor <- evSyn(GORIC_studies, hypo_names = goric_JU$result[,1])

# Show output
evSyn_anchor
# Final ratios of GORICA weights:
#round(evSyn_anchor$Final_ratio_GORICA_weights, 2) 
# Study-specific and final/overall output:
#summary(evSyn_anchor)
# Study-specific GORICA weights:
#evSyn_anchor$GORICA_weight_m
# evSyn plot:
#plot(evSyn_anchor)
#
# Note: With this input, you do not obtain output regarding log-likelihood values and weights.
# For that, one needs to use, for example, the 'LL & PT' as input or the goric objects themselves;
# see other examples and/or 'Tutorial_GORIC_restriktor_evSyn' on https://github.com/rebeccakuiper/Tutorials.


# Conclusion:
#
# When aggregating the results, we find (perhaps unsurprisingly) that ‘H1’ is the best (and thus not weak).
# More precise, because of the added-evidence approach, 
# it is many times more likely that overall theory ‘H1’ is correct in both studies 
# than that ‘H2’ would be correct in both studies. 
#
# Hence, there is support for the overall theory ‘H1’: 
# The amount in which a number differs from an anchor number is the highest 
# when the anchor value is rounded and the motivation is high (i.e., an instruction is given); 
# followed by the case/group where the anchor value is precise and the motivation is high; 
# next, the case/group where the anchor value is rounded and the motivation is low (i.e., no instruction is given); 
# and, the perceived amount of difference is lowest when the anchor value is precise and the motivation is low.


###

  
# Exercise 2: Aggregating evidence from two studies
  
# Study 1 (M)
monin <- read.table("monin.txt",header=TRUE)
monin$group <- factor(monin$group)       # this command tells R that group is a factor and not a continuous variable like 'attract'.
fit.lm_monin <-  lm(attract ~ group-1, data=monin)
#
# Study 2 (H)
holubar <- read.table("holubar.txt",header=TRUE)
holubar$gr <- factor(holubar$gr)       # this command tells R that gr is a factor and not a continuous variable like 'at'.
# lm object (of ANOVA model)
fit.lm_holubar <-  lm(at ~ gr-1, data=holubar)

### Input evidence synthesis: estimates and their covariance matrix
est_M <- coef(fit.lm_monin)
est_H <- coef(fit.lm_holubar)
# names(est_H) <- names(est_M) # re-label estimate names as the same
Param_studies <- list(est_M, est_H)
Param_studies # To check the resulting list
#
vcov_est_M <- vcov(fit.lm_monin)
vcov_est_H <- vcov(fit.lm_holubar)
CovMx_studies <- list(vcov_est_M, vcov_est_H)

# Set of hypotheses for each study 
# Note: in this case the same set for each study, but different names are used.
# Either re-label estimate names as the same (as suggested above)
# or use study-specific estimates (as done next).
#
# names(est_M) # Specify restrictions using those names
H1_M <- 'group1 = group2 > group3' #  equals 'group2 = group1 > group3'
H2_M <- 'group2 > group1 > group3'   
# names(est_2) # Specify restrictions using those names
H1_H <- 'gr1 = gr2 > gr3'
H2_H <- 'gr2 > gr1 > gr3'
# Note: overall theory H2 states that the attraction to moral rebels 
# after a self-confidence boost is higher than 
# the attraction to a person that is obedient 
# which are both higher than the attraction to moral rebels after a bogus task.
#
# Evaluate H1 and H2 (with Hunc as failsafe):
Hypo_studies <- list(Set_M = list(H1 = H1_M, H2 = H2_M), 
                     Set_H = list(H1 = H1_H, H2 = H2_H))
# Note that the "unconstrained" is used by default in case of 2 or more hypotheses.

# Evidence synthesis
evSyn_params <- evSyn(Param_studies, VCOV = CovMx_studies, 
                      hypotheses = Hypo_studies
                      #type = "added", # default
                      #comparison = "unconstrained" # default
                      ) 

# Show output
evSyn_params
# Final ratios of GORICA weights:
#round(evSyn_params$Final_ratio_GORICA_weights, 2) 
# Study-specific and final/overall output:
#summary(evSyn_params)
# Study-specific GORICA weights:
evSyn_params$GORICA_weight_m
# evSyn plot:
#plot(evSyn_params)


# Conclusion:
#
# When inspecting the study-specific results (cf. evSyn_params$GORICA_weight_m), 
# one can see that the studies do not prefer the same hypothesis: 
# That is, the result in the first study ('H2' is best and bout equally likely as 'H1') is not replicated by the second ('unconstrained' is best).
#
# When aggregating the mixed results, we find that overall theory ‘H1’ is the best.
# More precise, because of the added-evidence approach, it is is more likely that overall theory ‘H1’ is correct in both studies than that ‘H2’ would be correct in both studies. 
# However, it is only 1.118 times more likely.
# Additionally, the 'unconstrained' also receives quite some support.
#
# Since the log-likelihood (LL) for the 'unconstrained' is much higher than 
# the LL for the informative hypotheses / central theories,
# I would conclude that neither of the two is preferred in both studies. 
# One may need more studies to say something about the population with more certainty;
# and/or one should update the theories;
# and/or one should inspect differences between these studies which may explain this finding. 


###


# Exercise 3: Aggregating evidence from four heterogeneous studies

#### 4 trust studies described in Kuiper et al (2013): H0, Hpos, Hneg ####
#In this example, we will use the data and hypotheses used in Kuiper et al. (2013). 
#This article describes four diverse trust studies where each have a theory about the sign of the parameter of interest ($H_0$, $H_{pos}$ and $H_{neg}$). 
#
#Please note the following:
#* You could also choose to discard the uninformative hypothesis $H_0$.
#* The GORICA only requires the structural parameters! Hence, we do not need to use all the parameters, but only the one of interest.

# In this example, all four studies have the same number of parameters of interest (namely 1; i.e., the hypotheses below address only one parameter).
est_1 <- c(beta1 = 0.09)
est_2 <- c(beta1 = 0.14)
est_3 <- c(beta1 = 1.09)
est_4 <- c(beta1 = 1.781)
Param_studies <- list(est_1, est_2, est_3, est_4)
Param_studies # To check the resulting list

#Furthermore, we need to specify the covariance matrix of the parameter estimates; for this matrix it is not necessary to specify the column names. 
#Since there is only one parameter in the example, we only need the squared standard errors of the parameter estimate value - for each of the four studies. 
vcov_est_1 <- matrix(c(0.029^2), nrow = 1)
vcov_est_2 <- matrix(c(0.054^2), nrow = 1)
vcov_est_3 <- matrix(c(0.093^2), nrow = 1)
vcov_est_4 <- matrix(c(0.179^2), nrow = 1)
CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4)


## Evidence synthesis using the GORICA ##

# Set hypotheses #

# Now, we need to specify the hypotheses for all studies. 
# In this example, all studies have the same set of hypotheses (consisting of three hypotheses)
H0 <- "beta1 = 0"
Hpos <- "beta1 > 0"
Hneg <- "beta1 < 0"
Hypo_studies <- list(H0, Hpos, Hneg)

# Then, we also need to set a safeguard-hypothesis, to prevent choosing a best hypothesis from a set of weak hypotheses. 
# In this example, the whole space of theories is covered by the three hypotheses. 
# Therefore, we do not need a safeguard-hypothesis here.
safeguard <- "none"
# Note that the "unconstrained" is used by default in case of 2 or more hypotheses;
# so, we need to overwrite this.

# Evidence synthesis #
# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis: 
# type = "added" (default) or type = "equal"
# In this case, we will use the default added-evidence approach.
evSyn_trust <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
                     hypotheses = Hypo_studies,
                     #type = "added", # Default
                     comparison = safeguard)

# Show output
evSyn_trust
# Final ratios of GORICA weights:
#round(evSyn_trust$Final_ratio_GORICA_weights, 2) 
# Study-specific and final/overall output:
#summary(evSyn_trust)
# Study-specific GORICA weights:
#evSyn_trust$GORICA_weight_m
# evSyn plot:
plot(evSyn_trust)


# Conclusion:
#
# Support for Hpos is highest: thus, favor Hpos over H0 and Hneg.
# More precise, because of the added-evidence approach, 
# it is most likely that Hpos is correct in all studies. 
#
# Hence, previous experience has a positive effect on trust; 
# which receives full support.


###


# Exercise 4: Aggregating evidence from homogeneous studies

# For possible answers, see 'Example 2' in 'Tutorial_GORIC_restriktor_evSyn' 
# on https://github.com/rebeccakuiper/Tutorials.
#
# There, you can find code and conclusions for multiple possible sets of hypotheses.
# Notably, the lab html also contains code and conclusions (for one set of hypotheses).

  