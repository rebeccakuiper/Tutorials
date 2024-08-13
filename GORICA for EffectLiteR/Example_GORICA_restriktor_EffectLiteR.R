
# https://github.com/rebeccakuiper/Tutorials/blob/main/Tutorial_GORIC_restriktor_General.R

if (!require("EffectLiteR")) install.packages("EffectLiteR") # install this package first (once)
library(EffectLiteR)
# effectLiteGUI() ## shiny interface
#
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor)

############## Example 1: Adjusted Means ##################

# Data
d <- example01 # Example data from EffectLiteR

# effectLite
m1 <- effectLite(y="dv", x="x", z=c("z1","z2"), data=d, method="sem")

# Parameter estimates and their covariance matrix (needed for GORICA)
parnames <- c("adjmean0","adjmean1","adjmean2")
est <- m1@results@est[parnames]
VCOV <- m1@results@vcov.def[parnames,parnames]

# Hypotheses of interest
H0 <- "adjmean0 = adjmean1 = adjmean2"
H1 <- "adjmean1 < adjmean0 < adjmean2"
# Btw only include H0 when it is really of interest

# GORICA
#
# H0 and H1 and unconstrained as failsafe #
out_01u <- goric(est, VCOV=VCOV, 
                 hypotheses=list(H0=H0, H1=H1) 
                 #type="gorica" # recognized because of input (est & VCOV)
                 )
out_01u
#summary(out_01u)
# H0 and H1 both not weak (nl more support than the unconstrained), thus,
# we can compare H0 vs H1: H0 has 0.553/0.340 more support than H1:
out_01u$ratio.gw # see element (1,2) for H0 vs H1
#H0 has 1.6280686 more support than H1. 
# Note that H1 also contains H0.
#
#
# H0 versus its complement = the unconstrained #
# Ho is the best and, for future research, 
# you may like the support of H1 versus its complement
out_0c <- goric(est, VCOV=VCOV, 
                hypotheses=list(H0=H0), comparison = 'complement')
out_0c
#summary(out_0c)
# H0 has 5.16 times more support than its complement, that is, the unconstrained.
#
#
# H1 versus its complement #
# If H0 is not of interest or 
# when H1 would have been the best, and, for future research, 
# you like the support of H1 versus its complement
out_1c <- goric(est, VCOV=VCOV, 
                hypotheses=list(H1=H1), comparison = 'complement')
out_1c
#summary(out_1c)
# H1 has the highest weight, but the loglik values resemble.
# Thus, we should not inspect the GORICA weights (ratio).
# This indicates that there is support for the overlap, here, border / boundary, 
# see the guidelines on https://github.com/rebeccakuiper/Tutorials.
# Hence, this indicates support for a hypothesis 
# where at least one of the inequalities is replaced by an equality (e.g., H0).
# Now, you can, for future research, explore this (see guidelines).



############## Example 2: Average Effects ##################

d <- nonortho # Example data from EffectLiteR

m1 <- effectLite(y="y", x="x", k="z", data=d, method="sem")

parnames <- c("Eg1","Eg2")
est <- m1@results@est[parnames]
VCOV <- m1@results@vcov.def[parnames,parnames]

H0 <- "Eg1 = Eg2" 
H1 <- "Eg1 < Eg2"
H2 <- "Eg1 > Eg2"
# When these are you hypotheses of interest, 
# I would only evaluate H1 (versus its complement).
# Notably, when H0 is true, then both have approximately the same loglik value,
# and thus both hypotheses will have approximately the same support then.
# See also Example 1 above.

# If all hypotheses:
#out_012u <- goric(est, VCOV=VCOV, 
#                  hypotheses=list(H0=H0, H1=H1, H2=H2, H3=H3) 
#                  # type="gorica" # recognized because of input
#                   )
#out_012u
#summary(out_012u)

# If H1 vs its complement
out_1c <- goric(est, VCOV=VCOV, 
                hypotheses=list(H1=H1), comparison = 'complement')
out_1c
#summary(out_1c)
# H1 has 5.8 times more support than its complement.
# Btw the loglik values do not resemble, so we can interpret the ratio.



############## Example 3: Average Effect ##################

d <- example_multilevel # Example data from EffectLiteR

m1 <- effectLite(y="y", x="x", z="z", data=d, method="lm")

parnames <- c("Eg1")
est <- m1@results@est[parnames]
VCOV <- as.matrix(m1@results@vcov.def[parnames,parnames]) # make this a matrix

H1 <- "Eg1 < 0"

# H1 vs its complement
out_1c <- goric(est, VCOV=VCOV, 
                hypotheses=list(H1=H1), comparison = 'complement')
out_1c
#summary(out_1c)
# The complement of H1 has full support.
# So, no support for H1; but for "Eg1 > 0".

