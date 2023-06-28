
## First, install the packages, if you have not done this already:
#if (!require("restriktor")) install.packages("restriktor")

# If you want to use restriktor from github:
if (!require("devtools")) install.packages("devtools")
library(devtools) 
install_github("LeonardV/restriktor")
#library(restriktor) # for goric function

library(restriktor)

###

#eenzaamheid ~ a*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#mentale_gezondheid ~ b*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#stress ~ c*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#H1 <- 'a < b < c'

###

LL <- list(volwassenen     = c(H1 =  12.032, Hc = 12.188),
           ouderen         = c(H1 =  12.984, Hc = 12.809),
           jongvolwassenen = c(H1 =   8.411, Hc =  8.934),
           jeugd           = c(H1 =  -5.449, Hc = 11.987))

PT = list(volwassenen     = c(H1 = 1.834, Hc = 2.666),
          ouderen         = c(H1 = 1.829, Hc = 2.671),
          jongvolwassenen = c(H1 = 1.884, Hc = 2.616),
          jeugd           = c(H1 = 1.873, Hc = 2.627))


evSyn.equal <- evSyn(object = LL, PT = PT, hypo_names = c("H1", "Hc"),
                     type = "equal")
evSyn.equal
summary(evSyn.equal)
plot(evSyn.equal)

