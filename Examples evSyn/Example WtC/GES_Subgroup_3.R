
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




### estimates 
S <- 9
  
est_1 <- c(-.018, -.734, -.492) #Baker I
names(est_1) <- c("PCC", "CA", "M") 
vcov_est_1 <- diag(c(.083, .096, .097))
est_2 <- c(.518, .005, -.081) #Baker NI
names(est_2) <- c("PCC", "CA", "M") 
vcov_est_2 <- diag(c(.076, .060, .061))
est_3 <- c(.264, -.422, .154) #Denies
names(est_3) <- c("PCC", "CA", "M") 
vcov_est_3 <- diag(c(.038, .042, .060))
est_4 <- c(.093, .116, .373) #Hashimoto
names(est_4) <- c("PCC", "CA", "M") 
vcov_est_4 <- diag(c(.164, .138, .151))
est_5 <- c(.847, -.280, .258) # K&C
names(est_5) <- c("PCC", "CA", "M") 
vcov_est_5 <- diag(c(.136, .123, .112))
est_6 <- c(.233, -.346, .270) #Lee
names(est_6) <- c("PCC", "CA", "M") 
vcov_est_6 <- diag(c(.079, .067, .064))
est_7 <- c(.394, -.192, .060) #McIntyre2003
names(est_7) <- c("PCC", "CA", "M") 
vcov_est_7 <- diag(c(.057, .057, .054))
est_8 <- c(.224, -.207, .137) #McIntyre
names(est_8) <- c("PCC", "CA", "M") 
vcov_est_8 <- diag(c(.098, .096, .083))
est_9 <- c(.296, .149, .388) #Mulyono
names(est_9) <- c("PCC", "CA", "M") 
vcov_est_9 <- diag(c(.089, .086, .091))

# If number of parameters differ per study (but can also be used when they are the same): make lists
#
# beta values from the analyses
Param_studies <- list(est_1, est_2, est_3, est_4, est_5, est_6, est_7, est_8, est_9)
#
# standard error of the beta's (from the S primary studies)
CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4, vcov_est_5, vcov_est_6, vcov_est_7, vcov_est_8, vcov_est_9)

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

