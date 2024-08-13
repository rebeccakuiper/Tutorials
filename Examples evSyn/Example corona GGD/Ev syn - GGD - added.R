
###
# First, install the packages, if you have not done this already:
#
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


#eenzaamheid ~ a*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#mentale_gezondheid ~ b*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#stress ~ c*pos_gevolgen_corona + geslacht + leeftijd + gezondheid
#H1 <- 'a < b < c'

###

# Data
#
LL <- list(volwassenen     = c(H1 =  12.032, Hc = 12.188),
           ouderen         = c(H1 =  12.984, Hc = 12.809),
           jongvolwassenen = c(H1 =   8.411, Hc =  8.934),
           jeugd           = c(H1 =  -5.449, Hc = 11.987))
#
PT = list(volwassenen     = c(H1 = 1.834, Hc = 2.666),
          ouderen         = c(H1 = 1.829, Hc = 2.671),
          jongvolwassenen = c(H1 = 1.884, Hc = 2.616),
          jeugd           = c(H1 = 1.873, Hc = 2.627))


# GORIC evidence synthesis
evSyn.added <- evSyn(object = LL, PT = PT, 
                     #type = "added", # default
                     hypo_names = c("H1", "Hc"))

evSyn.added
#summary(evSyn.added)
plot(evSyn.added)

