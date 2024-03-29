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


# 21 diverse studies
# 3 subgroups

# Subgroup 1
est_1 <- c(.442, -.149, .048) #Kim
names(est_1) <- c("PCC", "CA", "M") 
vcov_est_1 <- diag(c(.082, .095, .044))
est_2 <- c(.403, .084, .103) #Cetinkaya
names(est_2) <- c("PCC", "CA", "M") 
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

# Subgroup 2
est_6 <- c(0.574, -.065, .010) #Yashima et al.
names(est_6) <- c("PCC", "CA", "M") 
vcov_est_6 <- diag(c(.094, .086, .042))
est_7 <- c(.499, -.051, .048) #Ghonsooly E
names(est_7) <- c("PCC", "CA", "M") 
vcov_est_7 <- diag(c(.083, .081, .064))
est_8 <- c(.285, .105, .004) #Khajavy
names(est_8) <- c("PCC", "CA", "M") 
vcov_est_8 <- diag(c(.098, .092, .031))
est_9 <- c(.344, -.115, .018) #Ghonsooly H
names(est_9) <- c("PCC", "CA", "M") 
vcov_est_9 <- diag(c(.096, .094, .066))
est_10 <- c(.368, -.167, .111) # P&W
names(est_10) <- c("PCC", "CA", "M") 
vcov_est_10 <- diag(c(.094, .085, .030))
est_11 <- c(.653, -.057, .047) #Ruane
names(est_11) <- c("PCC", "CA", "M") 
vcov_est_11 <- diag(c(.064, .062, .042))
est_12 <- c(.416, -.163, -.004) #Yashima 2.0
names(est_12) <- c("PCC", "CA", "M") 
vcov_est_12 <- diag(c(.044, .043, .024))

# Subgroup 3
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
Param_studies <- list(est_1, est_2, est_3, est_4, est_5, est_6, est_7, est_8, est_9, est_10, est_11, est_12, est_13, est_14, est_15, est_16, est_17, est_18, est_19, est_20, est_21)
#
# standard error of the beta's (from the primary studies)
CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4, vcov_est_5, vcov_est_6, vcov_est_7, vcov_est_8, vcov_est_9, vcov_est_10, vcov_est_11, vcov_est_12, vcov_est_13, vcov_est_14, vcov_est_15, vcov_est_16, vcov_est_17, vcov_est_18, vcov_est_19, vcov_est_20, vcov_est_21)


# Central theory & Study-specific hypotheses
#
H1 <- 'abs(PCC) > abs(CA) > abs(M)'  
#
Hypo_studies <- list(H1 = H1)
# versus its complement


# GORICA evidence synthesis
GES_All <- evSyn(object = Param_studies, 
                 VCOV = CovMx_studies, 
                  hypotheses = Hypo_studies,
                  #type = "added", # default
                  comparison = "complement") 


GES_All
#summary(GES_All)
plot(GES_All)
