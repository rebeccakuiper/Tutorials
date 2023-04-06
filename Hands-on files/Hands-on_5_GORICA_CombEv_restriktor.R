
#### Updating/Aggregating evidence obtained with GORIC and GORICA: 
# GORIC Evidence Synthesis Example


# NOTE 1: This is work in progress. 
# So, please only use the upcoming function yourself, and do not distribute this to others until the package is freely available on GitHub (now, it is 'privately').
# Also, let me know if you think things could be improved (e.g., way of asking for input, what should be reported as output, et cetera).

# NOTE 2: Make sure you use the right version of restriktor, namely version 0.2-800 (or higher).
# So, you should obtain the following:
library(restriktor)
#>This is restriktor 0.2-800
#>restriktor is BETA software! Please report any bugs.
# Version 0.2-500 might seem to work, but renders wrong results if the complement is used.



#First, we need to load the required libraries (after installing them once). 
#Since the `GoricEvSyn` package is only on GitHub (not on CRAN), we need `devtools` to install it. 
#
if (!require("devtools")) install.packages("devtools")
library(devtools)
#
#install_github("rebeccakuiper/GoricEvSyn") #, auth_token = "6ae25b58364f3c0a2beeef95ba3fd0ad114566a8") # This way you get acces to this 'private' package
library(GoricEvSyn)

# If you want to see the help/description files of this function, you can use the following code:
?GoricEvSyn
?GoricEvSyn_IC
?GoricEvSyn_LLandPT



#### 4 trust studies described in Kuiper et al (2013): H0, Hpos, Hneg ####
#In this example, we will use the data and hypotheses used in Kuiper et al. (2013). 
#This article describes four diverse trust studies where each have a theory about the sign of the parameter of interest ($H_0$, $H_{pos}$ and $H_{neg}$). 
#
#Please note the following:
#* You could also choose to discard the uninformative hypothesis $H_0$.
#* The GORICA only requires the structural parameters! Hence, we do not need to use all the parameters, but only the one of interest.

# Set the number of primary studies for which the evidence for a central theory should be determined and synthesized:
S <- 4

# In this example, all studies have the same number of parameters of interest (namely 1; i.e., the hypotheses below address only one parameter).
# In that case, we can collect the study-specific estimates (i.e., the $\beta$-values from the studies) in one matrix: 
# The estimates of the studies are placed in separated rows, while the parameter estimates are placed in the columns. 
# In this example, there are $S = 4$ rows and $nrParam = 1$ column. 
# We also specify the name of this column accordingly to what it represents, namely `beta1`. Please make sure to name the column the same way as in the hypotheses below.
nrParam <- 1
param_studies <- matrix(c(0.09, 0.14, 1.09, 1.781), nrow = S, ncol = nrParam)
colnames(param_studies) <- "beta1"
param_studies # To check the resulting matrix

#Furthermore, we need to specify the covariance matrix of the parameter estimates; for this matrix it is not necessary to specify the column names. 
#Since there is only one parameter in the example, we only need the squared standard errors of the $\beta$-values. 
covmx_studies <- matrix(c(0.029^2, 0.054^2, 0.093^2, 0.179^2), nrow = S, ncol = nrParam)


## Evidence synthesis using the GORICA ##

# Set hypotheses #

# Now, we need to specify the hypotheses for all studies. 
# In this example, all studies have the same set of hypotheses ('same_hypo <- 1'), consisting of three hypotheses ('nrHypos <- 3').
same_hypo <- 1
nrHypos <- 3
#
H0 <- "beta1 = 0"
Hpos <- "beta1 > 0"
Hneg <- "beta1 < 0"
#
hypo_studies <- c(H0, Hpos, Hneg)

# Then, we also need to set a safeguard-hypothesis, to prevent choosing a best hypothesis from a set of weak hypotheses. 
# In this example, the whole space of theories is covered by the three hypotheses. Therefore, we do not need a safeguard-hypothesis here.
safeguard <- "none"


# Evidence synthesis #
# Before we can start with the evidence-synthesis, we need to set the type of evidence-synthesis. 
# In this case, we will set it to 1 ('type_ev <- 1'), the added-evidence approach.
type_ev <- 1
GoricEvSyn(type_ev, S, param_studies, covmx_studies, same_hypo, nrHypos, hypo_studies, safeguard)
#
# From the last line in 'Cumulative.GORICA.weights', it can be seen that 'H2' (i.e., $H_{pos}$) is the most supported hypothesis and even has the maximum support.
# From 'Final.rel.GORICA.weights', you can also see that is many more times supported than the other hypotheses.


###################################################################################

