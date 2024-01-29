
if (!require("restriktor")) install.packages("restriktor")
library(restriktor)


# Data
y <- c(9.00, 9.50, 9.75, 10.00, 13.00, 9.50,
       11.00, 10.00, 10.00, 11.75, 10.50, 15.00,
       13.25, 11.50, 12.00, 13.50, 11.50)
D <- as.factor(c(rep(1,6), rep(2,6), rep(3,5)))

# Model
fit.lm <- lm(y ~ -1 + D) 
fit.lm

# Hypothesis of interest
H1 <- "D1 = D2 = D3" 
# this reflects mu1 = mu2 = mu3
#
# Note that the data (or at least X) possibly has to be  
# standardized to compare standardized parameters.

# AIC values and AIC weights
AIC <- goric(fit.lm, hypotheses = list(H1), comparison = "complement") 
AIC
# The order-restricted hypothesis ‘H1’ has 0.360 times more support than its complement.

# AICc values and AICc weights
AICc <- goric(fit.lm, hypotheses = list(H1), comparison = "complement",
              type = 'goricc') 
AICc
#The order-restricted hypothesis ‘H1’ has 1.241 times more support than its complement.