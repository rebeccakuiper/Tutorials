
# Tutorials:

#Link to R-tutorials for GORIC and GORICA: 
# https://github.com/rebeccakuiper/Tutorials

# Some links to R scripts for GORICA on several type of statistical models:
# - Structural equation modeling
#   https://github.com/rebeccakuiper/GORICA_in_SEM
#   This is material that belongs to the article on 
#   https://www.tandfonline.com/doi/full/10.1080/10705511.2020.1836967
# - cross-lagged panel model (CLPM)
#   https://github.com/rebeccakuiper/GORICA_in_CLPM
# - meta-analysis
#   https://github.com/rebeccakuiper/GORICA_on_MetaAn
# - CTmeta-analysis (meta-analysis for lagged effects models)
#   https://github.com/rebeccakuiper/GORICA_on_CTmeta


##############################

# Install and load packages

## First, install the packages, if you have not done this already:
if (!require("restriktor")) install.packages("restriktor")

## Then, load the packages:
library(restriktor) # for the goric function

# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function

########################


###
#For information regarding interpreting the GORIC(A) output,
#see 'Guidelines_output_GORIC' (https://github.com/rebeccakuiper/Tutorials).
###


# Example Palmer & Gough

# Data
PandG_data <- read.table("data/Data_PalmerAndGough.txt", header=TRUE)
PandG_data$group <- factor(PandG_data$group) 


# Fit object, also NHST
fit.PandG <- lm(Importance ~ group - 1, data = PandG_data)
#summary(fit.PandG) # NHST


# NHST: pairwise testing
pairwise.t.test(PandG_data$Importance, PandG_data$group, p.adj = 'bonferroni')


# (Informative) hypotheses
H0 <- 'group1 = group2 = group3' 
H1 <- 'group1 > group2 > group3' 
# By default, the unconstrained is used as a failsafe hypothesis


# GORIC
set.seed(123) # Set seed value
goric.PandG <- goric(fit.PandG, 
                     hypotheses = list(H0 = H0, H1 = H1))
#goric.PandG$result[,1] <- c("H0","H1","Hu")
goric.PandG
#goric.PandG$result
#summary(goric.PandG)
goric.PandG$ratio.gw


# Most probably, H0 is not of interest, then do:
H1 <- 'group1 > group2 > group3' 
# vs its complement (default in case of 1 informative hypothesis)
set.seed(123) # Set seed value
goric.PandG <- goric(fit.PandG, hypotheses = list(H1))
goric.PandG
#goric.PandG$result
#summary(goric.PandG)



# Example Lucas

# Data
Lucas_data <- read.table("data/Data_Lucas.txt", header=TRUE)
Lucas_data$group <- factor(Lucas_data$group) 

# NHST: pairwise testing
pairwise.t.test(Lucas_data$Influence, Lucas_data$group, 
                p.adj = 'bonferroni')

# Fit object, also NHST
fit.Lucas <- lm(Influence ~ group - 1, data = Lucas_data)
#summary(fit.Lucas) # NHST


# (Informative) hypotheses
H1 <- 'group5 = group3 > group1 > group2, group3 > group4 > group2'
# vs its complement (default in case of 1 informative hypothesis)

# GORIC
set.seed(123) # Set seed value
goric.Lucas <- goric(fit.Lucas, 
                     hypotheses = list(H1))
goric.Lucas
#goric.Lucas$result
#summary(goric.Lucas)

# GORICA
set.seed(123) # Set seed value
gorica.Lucas <- goric(fit.Lucas, 
                      hypotheses = list(H1), 
                      type = 'gorica')
gorica.Lucas
#gorica.Lucas$result
#summary(gorica.Lucas)

