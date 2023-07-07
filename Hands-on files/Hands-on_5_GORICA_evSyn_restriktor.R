
#### Updating/Aggregating evidence obtained with GORIC and GORICA: 
# GORIC Evidence Synthesis Example

# Load libraries. 
# This contains the 'evSyn' function that will be used in this R code. 
# Each time you re-open this R file you have to execute this step.
#
# Install the packages (once) 
# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#install_github("LeonardV/restriktor", force = T)
#library(restriktor) # for goric function
# If from CRAN:
if (!require("restriktor")) install.packages("restriktor")
#
# Load 
library(restriktor) # for evSyn and also goric function

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

# Set hypotheses #

# Now, we need to specify the hypotheses for all studies. 
# In this example, all studies have the same set of hypotheses (consisting of three hypotheses)
H0 <- "beta1 == 0"
Hpos <- "beta1 > 0"
Hneg <- "beta1 < 0"
#
Hypo_studies <- list(H0, Hpos, Hneg)
#
# Then, we also need to set a safeguard-hypothesis, to prevent choosing a best hypothesis from a set of weak hypotheses. 
# In this example, the whole space of theories is covered by the three hypotheses. 
# Therefore, we do not need a safeguard-hypothesis here.
safeguard <- "none"


# GORICA evidence synthesis #
# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis: 
# type = "added" (default) or type = "equal"
# In this case, we will use the default added-evidence approach.
evSyn_trust <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
                          hypotheses = Hypo_studies,
                          #type = "added", # Default
                          comparison = safeguard)

evSyn_trust
summary(evSyn_trust)
plot(evSyn_trust)

###################################################################################

