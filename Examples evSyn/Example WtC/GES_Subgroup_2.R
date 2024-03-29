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


# Second subgroup
# 7 diverse studies

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


# Data
#
# beta values (from the primary studies)
Param_studies_Sub2 <- list(est_6, est_7, est_8, est_9, est_10, est_11, est_12)
#
# standard error of the beta's (from the primary studies)
CovMx_studies_Sub2 <- list(vcov_est_6, vcov_est_7, vcov_est_8, vcov_est_9, vcov_est_10, vcov_est_11, vcov_est_12)


# Central theory & Study-specific hypotheses
#
H1 <- 'abs(PCC) > abs(CA) > abs(M)'  
#
Hypo_studies <- list(H1 = H1)
# versus its complement


# GORICA evidence synthesis
GES_Sub2 <- evSyn(object = Param_studies_Sub2, 
                  VCOV = CovMx_studies_Sub2, 
                  hypotheses = Hypo_studies,
                  #type = "added", # default
                  comparison = "complement") 


GES_Sub2
#summary(GES_Sub2)
plot(GES_Sub2)
