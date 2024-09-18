if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function
#
library(devtools)
install_github("rebeccakuiper/CTmeta")
library(CTmeta)
?CTmeta
citation("CTmeta")

# Input needed in example below with q=2 variables and S=3 primary studies
#
N <- matrix(c(643, 651, 473))
DeltaT <- matrix(c(2, 3, 1))   # Time interval used in the 3 primary studies
DeltaTStar <- 1                # Targeted time interval
#
# Here, the example matrices stored in the package are used.
# These contain the estimates for all 3 primary studies, stacked in one matrix.
Phi <- myPhi
SigmaVAR <- mySigmaVAR

## Example CTmeta: random effects model ("FEorRE = 2") without moderators ##
out_CTmeta <- CTmeta(N, DeltaT, DeltaTStar, Phi, SigmaVAR, FEorRE = 2)
out_CTmeta

## Evaluate dominance of cross-lagged relationships ##
#
# Specify hypothesis
H1 <- "abs(overallPhi12) < abs(overallPhi21)"
# vs its complement (default in case of one hypothesis)
#
#
# Evaluate dominance of cross-lagged relationships via GORICA 
# Next, two options, which lead to some results of course
#
# Option 1: Extract the vectorized overall standardized Phi matrix and its covariance matrix
est <- coef(out_CTmeta)  # or: est  <- out_CTmeta$Overall_vecStandPhi_DeltaTStar
VCOV <- vcov(out_CTmeta) # or: VCOV <- out_CTmeta$CovMx_OverallPhi_DeltaTStar
# GORICA (default)
set.seed(123) # for reproducability of results and possible sensitivity check of penalty
goricaResult <- goric(est, VCOV = VCOV, hypotheses = list(H1))
#summary(goricaResult)
goricaResult
#
# Option 2: Use CTmeta object
# GORICA (default)
set.seed(123)  # for reproducability of results and possible sensitivity check of penalty
goricaResult <- goric(out_CTmeta, hypotheses = list(H1))
# In this example, the complement is "overallPhi12 > overallPhi21"
#summary(goricaResult)
goricaResult
