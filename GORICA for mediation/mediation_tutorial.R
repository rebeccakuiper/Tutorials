# This tutorial shows how to use GORICA to evaluate mediation hypotheses in lavaan

# Load packages (if needed, install first)
if (!require("lavaan")) install.packages("lavaan")
if (!require("restriktor")) install.packages("restriktor")
library(lavaan)
library(restriktor)


# Simulate a simple mediation model
sim_model <- "X =~ X1 + X2 + X3 + X4 + X5
              Y =~ Y1 + Y2 + Y3 + Y4 + Y5
              M =~ M1 + M2 + M3 + M4 + M5
              M ~ .3*X
              Y ~ .5*M
              Y ~ .3*X"
set.seed(123) # for reproducibility
sim_data <- lavaan::simulateData(sim_model)

# Specify (a blind) model
model <- "X =~ X1 + X2 + X3 + X4 + X5
          Y =~ Y1 + Y2 + Y3 + Y4 + Y5
          M =~ M1 + M2 + M3 + M4 + M5
          M ~ a*X
          Y ~ b*M
          Y ~ c*X
          indirect := a*b
          direct := c
"
# Note that we define the direct relationship, labelled as c, here as well.
# This is needed to be able to obtain the covariance between 
# the direct and indirect relationship.

# Fit the model
fit <- lavaan::sem(model, sim_data)

# See the results
summary(fit, std = T)


# Below, we demonstrate how several mediation hypotheses can be evaluated.
# This can be done using the 
# A. fit object (and using the goric argument 'standardized = TRUE')
# B. extracted standardized estimates of the defined parameters and their covariance matrix 
#
# If B.:
# In the hypotheses below, we address both the indirect and direct relationships;
# So, we need to extract those estimates and their covariance matrix.
# Btw If the hypothesis (set) only regards indirect (like in H_any), 
# it suffices to only have that estimate and its variance.
label_names <- c("indirect", "direct")
est <- as.vector(standardizedSolution(fit)['est.std'])$est.std
names(est) <- standardizedSolution(fit)$label
est <- est[label_names]
#est
VCOV <- lavInspect(fit, "vcov.def.std.all")[label_names, label_names] 
# VCOV matrix of defined parameters. 
# This is why we needed to define the direct relationship as well.



# Now, let's evaluate various mediation hypotheses

# 1. Is there a non-negligible indirect effect/relationship?
# Recall the concept of SESOI (smallest effect size of interest)

H_any <- "abs(indirect) > 0.05"
# vs its compliment

set.seed(123) # for reproducibility & possibly sensitivity check
# A.
gorica_indirect <- restriktor::goric(fit, standardized = TRUE,
                                     hypotheses = list(H_any = H_any))
# B.
#gorica_indirect <- restriktor::goric(est, VCOV = VCOV,
#                                  hypotheses = list(H_any = H_any))
gorica_indirect

# Here, we evaluate support for the presence of an indirect effect/relationship 
# of a given (non-negligible) magnitude in any direction.


# 2. Is there partial mediation?

H_part <- "abs(indirect) > 0.05; abs(direct) > 0.05"
# vs its compliment

set.seed(123) # for reproducibility
restriktor::goric(fit, standardized = TRUE,
                  hypotheses = list(H_part = H_part))
#restriktor::goric(est, VCOV = VCOV,
#                  hypotheses = list(H_part = H_part))

# Here, we evaluate support for the presence of 
# partial mediation of a given magnitude in any direction.
# In other words, we evaluate whether both the direct and the indirect 
# effect/relationship of a non-negligible size (in any direction) are present.


# 3. Is there full mediation?
# with an approximate equality for the direct relationship

H_full <- "abs(indirect) > 0.05; -0.05 < direct < 0.05"
# vs its compliment

set.seed(123) # for reproducibility
restriktor::goric(fit, standardized = TRUE,
                  hypotheses = list(H_full = H_full))
#restriktor::goric(est, VCOV = VCOV,
#                  hypotheses = list(H_full = H_full))

# Here, we evaluate support for the presence of 
# full mediation of any magnitude and direction.
# Namely, we evaluate whether a meaningful indirect relationship is present, 
# while the direct relationship is, at most, negligible.


# 2. & 3. Partial vs Full mediation

H_part <- "abs(indirect) > 0.05; abs(direct) > 0.05"
H_full <- "abs(indirect) > 0.05; -0.05 < direct < 0.05"
# and unconstrained as failsafe

# In case you want to select the best fitting mediation type, 
# you can compare both hypotheses together with the unconstrained 
# to make sure none of them is weak.

set.seed(123) # for reproducibility
restriktor::goric(fit, standardized = TRUE,
                  hypotheses = list(H_part = H_part, 
                                    H_full = H_full))
#restriktor::goric(est, VCOV = VCOV,
#                  hypotheses = list(H_part = H_part, 
#                                    H_full = H_full))

# Second, you can compare the best hypothesis against its complement.
restriktor::goric(fit, standardized = TRUE,
                  hypotheses = list(H_part = H_part))
#restriktor::goric(est, VCOV = VCOV,
#                  hypotheses = list(H_part = H_part))


# 4. Directional hypothesis

# You can also specify any effect's/relationship's direction.

H_full_pos <- "indirect > 0.05; -0.05 < direct < 0.05"
# vs its compliment

set.seed(123) # for reproducibility
restriktor::goric(fit, standardized = TRUE,
                  hypotheses = list(H_full_pos = H_full_pos))
#restriktor::goric(est, VCOV = VCOV,
#                  hypotheses = list(H_full_pos = H_full_pos))


