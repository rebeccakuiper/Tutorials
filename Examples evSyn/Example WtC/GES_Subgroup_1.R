
#### Updating/Aggregating evidence obtained with GORIC and GORICA: 
# GORIC Evidence Synthesis Example


# NOTE 1: This is work in progress. 
# So, please only use the upcoming function yourself, and do not distribute this to others until the package is freely available on GitHub (now, it is 'privately').
# Also, let me know if you think things could be improved (e.g., way of asking for input, what should be reported as output, et cetera).

# NOTE 2: Make sure you use the right version of restriktor, namely version 0.2-800 (or higher).
# So, you should obtain the following:
# use restriktor from github:
if (!require("devtools")) install.packages("devtools") # gorica
library(devtools)
library(restriktor) # for goric function
#>This is restriktor 0.2-800
#>restriktor is BETA software! Please report any bugs.
# Version 0.2-500 might seem to work, but renders wrong results if the complement is used.



### Example 2: 2 ANOVA studies (Monin and Holubar) ###
### We will use the estimates of the ANOVA models

S <- 5
est_1 <- c(.442, -.149, .048) #Kim
names(est_1) <- c("PCC", "CA", "M") # ALLEMAAL DEZELFDE NAAM (de studies (1 t/m 6) en de groepen)
vcov_est_1 <- diag(c(.082, .095, .044))
est_2 <- c(.403, .084, .103) #Cetinkaya
names(est_2) <- c("PCC", "CA", "M") # Use different names to show use of different set of hypotheses
vcov_est_2 <- diag(c(043, .042, .057))
est_3 <- c(-.161, -.042, -.285) #Fallah
names(est_3) <- c("PCC", "CA", "M")
vcov_est_3 <- diag(c(.064, .057, .084))
est_4 <- c(.193, -.045, .000) #Yu
names(est_4) <- c("PCC", "CA", "M")
vcov_est_4 <- diag(c(.030, .019, .001))
est_5 <- c(56.152, -5.039, -4.538) #Robson
names(est_5) <- c("PCC", "CA", "M") # Use different names to show use of different set of hypotheses
vcov_est_5 <- diag(c(48113.630, 3953.334, 3732.966))

# If number of parameters differ per study (but can also be used when they are the same): make lists
#
# beta values from the analyses
Param_studies <- list(est_1, est_2, est_3, est_4, est_5)
#
# standard error of the beta's (from the S primary studies)
CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4, vcov_est_5)

H1 <- 'abs(PCC) > abs(CA) > abs(M)'  # eigen hypothese
# Note: in tekst gebruik je bijv |PCC|, maar hier in code dan abs(PCC)

Hypo_studies <- list(H1 = H1)
#
# Evidence synthesis
out2 <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
              hypothesis = Hypo_studies,
              type = "added", 
              comparison = "complement") 

out2
out2$Cumulative.GORICA.weights
out2$Final.ratio.GORICA.weights

