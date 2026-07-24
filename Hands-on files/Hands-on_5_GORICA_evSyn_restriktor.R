
#### Updating/Aggregating evidence obtained with GORIC and GORICA: 
# GORIC Evidence Synthesis Example

# Load libraries. 
# This contains the 'evSyn' function that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.

## First, install the package, if you have not done this already:
#if (!require("restriktor")) install.packages("restriktor")

## Then, load the package:
#library(restriktor) # for evSyn and also goric function

# If you want to use restriktor from github: 
#if (!require("pak")) install.packages("pak") 
#library(pak) 
#pak::pak("LeonardV/restriktor") 
##pak::pkg_install("LeonardV/restriktor") 
#library(restriktor) # for goric and evSyn function 
#
# Or possibly (for extra functionalities): 
if (!require("remotes")) install.packages("remotes") 
library(remotes) 
# remotes::install_github("LeonardV/restriktor",
#                        ref = "Branch_Rebec",
#                        force = TRUE)
remotes::install_github("LeonardV/restriktor@Branch_Rebec") #, force = TRUE)
library(restriktor) # for goric and evSyn function

# print docs in the help-tab to view arguments and explanations for the function
#?evSyn


###################################################################################


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
#
Param_studies <- list(est_1, est_2, est_3, est_4)
Param_studies # To check the resulting list

#Furthermore, we need to specify the covariance matrix of the parameter estimates; for this matrix it is not necessary to specify the column names. 
#Since there is only one parameter in the example, we only need the squared standard errors of the parameter estimate value - for each of the four studies. 
vcov_est_1 <- matrix(c(0.029^2), nrow = 1)
vcov_est_2 <- matrix(c(0.054^2), nrow = 1)
vcov_est_3 <- matrix(c(0.093^2), nrow = 1)
vcov_est_4 <- matrix(c(0.179^2), nrow = 1)
#
CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4)


## Evidence synthesis using the GORICA ##

# Set of hypotheses #

# Now, we need to specify the hypotheses for all studies. 
# In this example, all studies have the same set of hypotheses (consisting of three hypotheses)
H0 <- "beta1 = 0"
Hpos <- "beta1 > 0"
Hneg <- "beta1 < 0"
#
Hypo_studies <- list(H0 = H0, Hpos = Hpos, Hneg = Hneg) # here, you can (re-)label hypotheses:
# Now, in the output the hypotheses are labelled 'H0', 'Hpos', and 'Hneg'.
# Note: if no labels/names are used, then they will be labelled 'H1', 'H2', and 'H3'.
#
# Then, we also need to set a safeguard-hypothesis, to prevent choosing a best hypothesis from a set of weak hypotheses. 
# In this example, the whole space of theories is covered by the three hypotheses. 
# Therefore, we do not need a safeguard-hypothesis here.
safeguard <- "none"
# Notably, if not specified, the unconstrained would be used by default.


# GORICA evidence synthesis (the added-evidence approach) #

# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis: 
# type = "added" (default) or type = "equal"
# In this case, we will use the default added-evidence approach.
evSyn_trust_inclNull <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
                              hypotheses = Hypo_studies,
                              #type_ev = "added", # Default
                              comparison = safeguard)

# Show output
evSyn_trust_inclNull
# Final ratios of GORICA weights:
#round(evSyn_trust_inclNull$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(evSyn_trust_inclNull)
# evSyn plot:
plot(evSyn_trust_inclNull)


# Alternatively, you could do (which is what I would do):
Hypo_studies <- list(Hpos = Hpos)
# versus its complement: 
# comparison = "complement" # which is the default option in case of one hypothesis of interest
#
evSyn_trust <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
                     hypotheses = Hypo_studies
                     #type_ev = "added", # Default
                     #comparison = "complement" # Default
                     )
#
# Show output
evSyn_trust
# Final ratios of GORICA weights:
#round(evSyn_trust$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(evSyn_trust)
# evSyn plot:
plot(evSyn_trust)


#Conclusion:
#Support for Hpos is highest: favor Hpos over H0 and Hneg, resp., its complement (Hneg).
#Hence, previous experience has a positive effect on trust; which receives full support.


###


# GORICA evidence synthesis (the equal-evidence approach) #

# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis: 
# type_ev = "added" (default) or type_ev = "equal"
# Next, we will use the equal-evidence approach:

# Set of hypotheses #
Hypo_studies <- list(Hpos = Hpos)
#versus its complement, which is the default option in case of one hypothesis of interest

# GORICA evidence synthesis -- the equal-evidence approach #
evSyn_trust_eq <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
                        hypotheses = Hypo_studies,
                        type_ev = "equal")

# Show output
evSyn_trust_eq
# Final ratios of GORICA weights:
#round(evSyn_trust_eq$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(evSyn_trust_eq)
# evSyn plot:
plot(evSyn_trust_eq)


#Conclusion:
#Support for Hpos is highest; thus, favor Hpos over its complement (Hneg).
#Hence, previous experience has a positive effect on trust, which receives full support.

#Note:
#In the case of added-evidence: There is full support that all studies favor Hpos.
#In the case of equal-evidence: There is full support that, on average (based on all studies) Hpos is best.


###################################################################################


### Berkey example (using data.frame) ###

# Data #
#if (!require("metafor")) install.packages("metafor")
library(metafor)
dat <- dat.berkey1998
#
# print docs in the help-tab to view explanations for the data set
#?dat.berkey1998
# and/or check out: https://www.metafor-project.org/doku.php/analyses:berkey1998


# (study-specific) hypotheses #
#
#Parameter names to be used in the hypotheses:
#- If one outcome: 'theta'.
#- Labels used in specified outcome column/variable:
# Here, 'AL' and 'PD', levels of the variable 'outcome',
# which then denotes the labels of our parameters of interest.
# NOTE:
# Below, I use 'outcome_col = "outcome"', but you can leave it out:
# Then, it is NULL but finds a variable 'outcome' (default name).
unique(dat$outcome)
#
# All studies have the same hypotheses in the set:
H_absComp_Gr <- "abs(AL) > abs(PD)" 
# Thus, we compare the absolute values of the parameters and expect that that of AL is greater than that of PD.
# This will be evaluated against its complement, which is included by default.
#
# Here, the hypotheses are the same for all studies,
# then, only specify one list:
Hypo_studies <- list(H_absComp = H_absComp_Gr) # here, you can (re-)label hypotheses:
# Now, in the output the hypothesis is labelled 'H_absComp'.
# Note: if no label/name is used, then it will be labelled 'H1'.


# Study names #
# Instead of using numbers for the studies, one can also attach names to them.
# Here, the author names will be used, listed in the variable author in the data.frame:
study_names <- dat[dat$outcome=="AL",]$author


# GORICA evidence synthesis (the added-evidence approach) #
#
#if (!require("restriktor")) install.packages("restriktor")
#library(restriktor)
results_Set3_Gr <- evSyn(object = dat, 
                         outcome_col = "outcome", # column with 'AL' & 'PD'; default
                         hypotheses = Hypo_studies,
                         #type = "added", # Default
                         #comparison = "complement", # default (if 1 hypothesis)
                         study_names = study_names # Optional
                         ) 
#
# Show output
results_Set3_Gr
# Final ratios of GORICA weights:
#round(results_Set3_Gr$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(results_Set3_Gr)
# evSyn plot:
plot(results_Set3_Gr)

# # Save plot
# #if (!require("ggplot2")) install.packages("ggplot2")
# library(ggplot2)
# evSyn_plot_Berkey_Set3_Gr <- plot(results_Set3_Gr)
# ggsave(
#   filename = "EvSyn_Berkey_Set3_Gr.png",  # file name
#   plot = evSyn_plot_Berkey_Set3_Gr,             # the plot object
#   width = 8,                                    # width in inches
#   height = 6,                                   # height in inches
#   dpi = 300                                     # resolution
# )


#Conclusion:
#
#Support for H_absComp is highest: favor H_absComp over its complement.
#
#On study level (cf. round(results_Set3_Gr$Final_ratio_GORICA_weights, 2)): mixed support; 
#combined: preference for H_absComp.
#
#H_absComp is 0.744 / 0.256 = approx. 2.9 times more likely than its complement.
#Henec, preference for central theory that 'the difference in symptom reduction between surgery and non-surgery is larger for AL than for PD'.


########## evSyn options ########## 


# (re-)order studies #
#
# It can be helpful to order the studies based on study-specific support for the overall best hypothesis 
# (order_studies = "ascending" or "descending"). 
# This way, it is more clear which studies do not find support for the overall best hypothesis, 
# and one can look at study characteristics to find a possible alternative explanation. 
#
# This of course does not affect the overall evidence/support.

#if (!require("restriktor")) install.packages("restriktor")
#library(restriktor)
results_Set3_Gr_asc <- evSyn(object = dat, 
                         outcome_col = "outcome", # column with 'AL' & 'PD'; default
                         hypotheses = Hypo_studies, # list(H_absComp = H_absComp_Gr)
                         #type = "added", # Default
                         #comparison = "complement", # Default
                         study_names = study_names,
                         order_studies = "ascending")
#
#results_Set3_Gr_asc
summary(results_Set3_Gr_asc)
plot(results_Set3_Gr_asc)

# # Save plot
# #if (!require("ggplot2")) install.packages("ggplot2")
# library(ggplot2)
# evSyn_plot_Berkey_Set3_Gr_asc <- plot(results_Set3_Gr_asc)
# ggsave(
#   filename = "EvSyn_Berkey_Set3_Gr_asc.png",  # file name
#   plot = evSyn_plot_Berkey_Set3_Gr_asc,             # the plot object
#   width = 8,                                    # width in inches
#   height = 6,                                   # height in inches
#   dpi = 300                                     # resolution
# )


#If of interest, one can also specify their own order perhaps based on one of the study characteristics:
order_ascYear <- order(dat[dat$outcome=="AL",]$year)
results_Set3_Gr_ascYear <- evSyn(object = dat, outcome_col = "outcome", # default
                                 hypotheses = Hypo_studies, # list(H_absComp = H_absComp_Gr)
                                 study_names = study_names,
                                 order_studies = order_ascYear)
results_Set3_Gr_ascYear
plot(results_Set3_Gr_ascYear)

# # Save plot
# #if (!require("ggplot2")) install.packages("ggplot2")
# library(ggplot2)
# evSyn_plot_Berkey_Set3_Gr_ascYear <- plot(results_Set3_Gr_ascYear)
# ggsave(
#   filename = "EvSyn_Berkey_Set3_Gr_ascYear.png",  # file name
#   plot = evSyn_plot_Berkey_Set3_Gr_ascYear,     # the plot object
#   width = 8,                                    # width in inches
#   height = 6,                                   # height in inches
#   dpi = 300                                     # resolution
# )



# Leave 1 study out #
#
#The function `leave1studyout` can be used to obtain insight into possible outlier or influential studies; 
#or stated otherwise: to do a sensitivity analyses on the GORICA Evidence Synthesis.

leave1out_Set3_Gr <- leave1studyout(results_Set3_Gr)
leave1out_Set3_Gr 
#leave1out_Set3_Gr$OverallGoricaWeights

#Conclusion:
#
#In this example, the results are sensitive to the choice of set of studies.
#
#The conclusion even changes when study nr. 2 called 'Lindhe et al.' is left out. 
#
#When one would leave that study out, the complement of H_absComp_Gr is the overall best hypothesis (or better: central theory), 
#receiving almost full support.
#When one of the other studies would be left out, 
#then the overall GORICA weight for H_absComp_Gr varies between 0.7 and 1.


#In case you want to perform GORICA evidence synthesis for the set of studies leaving the second one out:
dat_subset <- dat[dat$trial!=2,]
study_names_subset <- study_names[-1]
results_Set3_Gr_asc_subset <- evSyn(object = dat_subset, 
                                    outcome_col = "outcome", # default
                                    hypotheses = Hypo_studies, # list(H_absComp = H_absComp_Gr)
                                    order_studies = "ascending",
                                    study_names = study_names_subset)
results_Set3_Gr_asc_subset
plot(results_Set3_Gr_asc_subset)


# Study weights #

results_Set3_Gr_stw <- evSyn(object = dat, 
                         outcome_col = "outcome", # column with 'AL' & 'PD'; default
                         hypotheses = Hypo_studies,
                         #type = "added", # Default
                         #comparison = "complement", # Default
                         study_names = study_names, # Optional
                         study_weights = c(1/10, 2/10, 3/10, 2/10, 2/10)
) 
#
# Show output
results_Set3_Gr_stw
# Final ratios of GORICA weights:
#round(results_Set3_Gr_stw$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(results_Set3_Gr_stw)
# evSyn plot:
plot(results_Set3_Gr_stw)


####

# GORICA evidence synthesis (the equal-evidence approach) #

# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis: 
# type_ev = "added" (default) or type_ev = "equal"
# Next, we will use the equal-evidence approach:

# Set of hypotheses #
Hypo_studies <- list(H_absComp = H_absComp_Gr)
#versus its complement, which is the default option in case of one hypothesis of interest

# GORICA evidence synthesis -- the equal-evidence approach #
results_Set3_Gr_eq <- evSyn(object = dat, 
                            outcome_col = "outcome", # column with 'AL' & 'PD'; default
                            hypotheses = Hypo_studies,
                            type = "equal", 
                            #comparison = "complement", # Default
                            study_names = study_names # Optional
                           ) 
#
# Show output
results_Set3_Gr_eq
# Final ratios of GORICA weights:
#round(results_Set3_Gr_eq$Final_ratio_GORICA_weights, 2)
# Study-specific and final/overall output:
#summary(results_Set3_Gr_eq)
# evSyn plot:
plot(results_Set3_Gr_eq)


###################################################################################


### WORK IN PROGRESS ###

# Publication bias #

# Weight overall preferred hypothesis
n <- dat[dat$outcome=="AL",]$ni 
ICweights <- results_Set3_Gr$GORICA_weight_m[,1]
#
plot(n, ICweights)

# Ratio of weights overall preferred hypothesis; 
# here, H_absComp (preferred) vs Complement; thus column 1 vs 2:
n <- dat[dat$outcome=="AL",]$ni 
ratioICweights <- (results_Set3_Gr$GORICA_weight_m[,1] / results_Set3_Gr$GORICA_weight_m[,2])
#round(ratioICweights, 2)
#
plot(n, ratioICweights)
abline(a = 1, b = 0, col = "gray") # when 1, then H_absComp and Complement equally likely.
#
plot(n, log(ratioICweights))
abline(a = 0, b = 0, col = "gray") # when 0, then H_absComp and Complement equally likely.


# Measures of variation #

# Weight overall preferred hypothesis; 
# here, H_absComp and thus column 1:
ICweights <- results_Set3_Gr$GORICA_weight_m[,1]
round(quantile(ICweights, probs = c(0, .25, .50, .75, 1.00)), 2)

# Ratio of weights overall preferred hypothesis; 
# here, H_absComp (preferred) vs Complement; thus column 1 vs 2:
ratioICweights <- results_Set3_Gr$GORICA_weight_m[,1] / results_Set3_Gr$GORICA_weight_m[,2]
round(quantile(ratioICweights, probs = c(0, .25, .50, .75, 1.00)), 2)


###################################################################################

