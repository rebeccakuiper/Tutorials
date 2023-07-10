###
# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#install_github("LeonardV/restriktor", force = TRUE)
#
# If you want to use restriktor from CRAN:
if (!require("restriktor")) install.packages("restriktor")
library(restriktor) # for evSyn and goric function
###


# First subgroup
# 5 diverse studies

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


# Data
#
# beta values (from the primary studies)
Param_studies_Sub1 <- list(est_1, est_2, est_3, est_4, est_5)
#
# standard error of the beta's (from the primary studies)
CovMx_studies_Sub1 <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4, vcov_est_5)


# Central theory & Study-specific hypotheses
#
H1 <- 'abs(PCC) > abs(CA) > abs(M)'  
#
Hypo_studies <- list(H1 = H1)
# versus its complement


# GORICA evidence synthesis
GES_Sub1 <- evSyn(object = Param_studies_Sub1, 
                 VCOV = CovMx_studies_Sub1, 
                 hypotheses = Hypo_studies,
                 #type = "added", # default
                 comparison = "complement") 


GES_Sub1
#summary(GES_Sub1)
plot(GES_Sub1)

