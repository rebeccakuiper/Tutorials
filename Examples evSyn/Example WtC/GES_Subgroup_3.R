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


# Third subgroup
# 9 diverse studies
  
est_13 <- c(-.018, -.734, -.492) #Baker I
names(est_13) <- c("PCC", "CA", "M") 
vcov_est_13 <- diag(c(.083, .096, .097))
est_14 <- c(.518, .005, -.081) #Baker NI
names(est_14) <- c("PCC", "CA", "M") 
vcov_est_14 <- diag(c(.076, .060, .061))
est_15 <- c(.264, -.422, .154) #Denies
names(est_15) <- c("PCC", "CA", "M") 
vcov_est_15 <- diag(c(.038, .042, .060))
est_16 <- c(.093, .116, .373) #Hashimoto
names(est_16) <- c("PCC", "CA", "M") 
vcov_est_16 <- diag(c(.164, .138, .151))
est_17 <- c(.847, -.280, .258) # K&C
names(est_17) <- c("PCC", "CA", "M") 
vcov_est_17 <- diag(c(.136, .123, .112))
est_18 <- c(.233, -.346, .270) #Lee
names(est_18) <- c("PCC", "CA", "M") 
vcov_est_18 <- diag(c(.079, .067, .064))
est_19 <- c(.394, -.192, .060) #McIntyre2003
names(est_19) <- c("PCC", "CA", "M") 
vcov_est_19 <- diag(c(.057, .057, .054))
est_20 <- c(.224, -.207, .137) #McIntyre
names(est_20) <- c("PCC", "CA", "M") 
vcov_est_20 <- diag(c(.098, .096, .083))
est_21 <- c(.296, .149, .388) #Mulyono
names(est_21) <- c("PCC", "CA", "M") 
vcov_est_21 <- diag(c(.089, .086, .091))


# Data
#
# beta values (from the primary studies)
Param_studies_Sub3 <- list(est_13, est_14, est_15, est_16, est_17, est_18, est_19, est_20, est_21)
#
# standard error of the beta's (from the primary studies)
CovMx_studies_Sub3 <- list(vcov_est_13, vcov_est_14, vcov_est_15, vcov_est_16, vcov_est_17, vcov_est_18, vcov_est_19, vcov_est_20, vcov_est_21)


# Central theory & Study-specific hypotheses
#
H1 <- 'abs(PCC) > abs(CA) > abs(M)'  
#
Hypo_studies <- list(H1 = H1)
# versus its complement


# GORICA evidence synthesis
GES_Sub3 <- evSyn(object = Param_studies_Sub3, 
                  VCOV = CovMx_studies_Sub3, 
                  hypotheses = Hypo_studies,
                  #type = "added", # default
                  comparison = "complement") 


GES_Sub3
#summary(GES_Sub3)
plot(GES_Sub3)