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
# Extract the vectorized overall standardized Phi matrix and its covariance matrix
est <- out_CTmeta$Overall_standPhi_DeltaTStar
VCOV <- out_CTmeta$CovMx_OverallPhi_DeltaTStar
# Specify hypothesis
H1 <- "overallPhi12 < overallPhi21"
# Evaluate dominance of cross-lagged relationships via GORICA 
goricaResult <- goric(est, VCOV = VCOV, hypotheses = list(H1), comparison = "complement")
# In this example, the complement is "overallPhi12 > overallPhi21"
summary(goricaResult)