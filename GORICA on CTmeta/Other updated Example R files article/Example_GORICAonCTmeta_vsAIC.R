if (!require("expm")) install.packages("expm")
library(expm)
if (!require("metafor")) install.packages("metafor")
library(metafor)
if (!require("tsDyn")) install.packages("tsDyn")
library(tsDyn)
if (!require("vars")) install.packages("vars")
library(vars)
#
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function
#
library(devtools)
install_github("rebeccakuiper/CTmeta")
library(CTmeta)
?CTmeta
citation("CTmeta")


q <- 2 # Number of variables
Phi_pop <- matrix(c(0.50, 0.15,
                    0.25, 0.40), nrow=q, byrow=TRUE) # population Phi matrix (i.e., lagged effects matrix)
A_pop <- logm(Phi_pop) # underlying population drift matrix
Phi_pop <- expm(A_pop)
vecPhi_pop <- as.vector(t(Phi_pop)) # vectorized population Phi
Gamma_pop <- matrix(c(1,    0.3,
                      0.3,    1), nrow=q, byrow=TRUE) # population stationary covariance matrix
# Since Gamma is now a correlation matrix, Phi is already standardized
SigmaVAR_pop <- Gamma_pop - Phi_pop %*% Gamma_pop %*% t(Phi_pop) # population residual covariance matrix

#######################################

# Used from Work engagement & burnout paper
N <- matrix(c(
  643,
  651,
  473,
  387,
  187,
  209,
  2897,
  160,
  1964,
  848,
  926,
  274,
  433,
  256,
  409,
  926,
  162,
  262,
  247,
  102,
  171,
  201,
  309,
  77,
  67), byrow=TRUE)

S <- length(N)

# Time intervals ('time lags'), using Work engagement & burnout paper
TI <- matrix(c(
  12,
  12,
  12,
  4,
  9,
  12,
  12,
  2,
  36,
  12,
  12,
  8,
  24,
  12*(1/365),
  24,
  12,
  2,
  14,
  12,
  10,
  48,
  12,
  12,
  1,
  8), byrow=TRUE)
TI = TI/12 # now in years



#######################################################################################################################################

## Example ##

# Sample - for each study s in the meta-analysis - data based on population values, N_s, and DeltaT_s
S <- length(N)
Phi <- array(NA, dim = c(S*q, q))
SigmaVAR <- array(0, dim = c(S*q, q))
#
vecPhi <- array(NA, dim = c(S*q*q))
CovMx <- array(0, dim = c(S*q*q, S*q*q))
vecPhi1 <- array(NA, dim = c(S*q*q))
CovMx1 <- array(0, dim = c(S*q*q, S*q*q))
#
G <- length(unique(TI))
vecPhi_G <- array(NA, dim = c(S*q*q, G))
CovMx_G <- array(0, dim = c(S*q*q, S*q*q, G))
Phi_G <- array(NA, dim = c(S*q, q, G))
SigmaVAR_G <- array(NA, dim = c(S*q, q, G))
#
set.seed(123)
s <- 1 # index for number of studies, with in total S studies.
while(s <= S){
  Y_mu <- VAR.sim(Phi_pop, n = N[s], lag = 1, include = c("none"), starting = NULL, varcov = SigmaVAR_pop)
  Y <- scale(Y_mu, scale = FALSE) # substracts the means (so , it centers, does not standardize now because of scale = F)
  colnames(Y) <- paste0("y", 1:q)
  outcomeVAR <- VAR(Y, p = 1)
  Phi_VARest <- Acoef(outcomeVAR)[[1]]
  if(any(Re(eigen(Phi_VARest)$values) < 0)){
    s <- s # No CTM-equivalent, so do not proceed
  }else{  
    SigmaVAR1_VARest <- cov(resid(outcomeVAR))
    Gamma_VARest <- cov(Y)
    # Standardize parameters! 
    Sxy <- sqrt(diag(diag(Gamma_VARest)))
    Gamma_VARest <- solve(Sxy) %*% Gamma_VARest %*% solve(Sxy) 
    Phi_VARest <- solve(Sxy) %*% Phi_VARest %*% Sxy
    SigmaVAR1_VARest <- solve(Sxy) %*% SigmaVAR1_VARest %*% solve(Sxy) 
    #
    invGamma <- solve(Gamma_VARest)
    B_VARest <- -logm(Phi_VARest)/1
    #
    Phi_VARest <- expm(-B_VARest*TI[s])
    Phi[((s-1)*(q)+1):(s*q), 1:q] <- Phi_VARest
    SigmaVAR_VARest <- Gamma_VARest - Phi_VARest %*% Gamma_VARest %*% t(Phi_VARest)
    SigmaVAR[((s-1)*(q)+1):(s*q), 1:q] <- SigmaVAR_VARest
    vecPhi[((s-1)*(q*q)+1):(s*q*q)] <- as.vector(t(Phi_VARest))
    CovMx[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2)] <- kronecker(SigmaVAR_VARest, invGamma) / (N[s]-q)
    if(any( eigen( CovMx[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2)] )$values < 0 )){
      s <- s # Cov mx should be pos def
    }else{
      #
      Phi_VARest <- expm(-B_VARest*1)
      vecPhi1[((s-1)*(q*q)+1):(s*q*q)] <- as.vector(t(Phi_VARest))
      SigmaVAR_VARest <- Gamma_VARest - Phi_VARest %*% Gamma_VARest %*% t(Phi_VARest)
      CovMx1[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2)] <- kronecker(SigmaVAR_VARest, invGamma) / (N[s]-q)
      if(any( eigen( CovMx1[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2)] )$values < 0 )){
        s <- s # Cov mx should be pos def
      }else{
        for(g in 1:G){
          Phi_VARest <- expm(-B_VARest*unique(TI)[g])
          vecPhi_G[((s-1)*(q*q)+1):(s*q*q),g] <- as.vector(t(Phi_VARest))
          Phi_G[((s-1)*(q)+1):(s*q), 1:q, g] <- Phi_VARest
          #
          SigmaVAR_VARest <- Gamma_VARest - Phi_VARest %*% Gamma_VARest %*% t(Phi_VARest)
          CovMx_G[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2),g] = kronecker(SigmaVAR_VARest, invGamma) / (N[s]-q)
          SigmaVAR_G[((s-1)*(q)+1):(s*q), 1:q, g] <- SigmaVAR_VARest
        }
        if(any( apply(x <- CovMx_G[((s-1)*q^2+1):(s*q^2),((s-1)*q^2+1):(s*q^2),], 3, function(x){eigen(x)$values}) < 0 )){
          s <- s # Cov mx should be pos def
        }else{
          s <- s+1
        }
      }
    }
  }
}

any(eigen(CovMx1)$val < 0)
# FALSE = pos def; like we want! & TRUE = not pos def!
#
any(eigen(CovMx)$val < 0)


###########################################################################################################


# Multivariate Meta-analyses
out_CTmeta <- CTmeta(N, DeltaT = TI, DeltaTStar = 1, Phi, SigmaVAR)
#
#
## Evaluate dominance of cross-lagged effects ##
#
# Specify hypothesis
H1 <- "overallPhi12 < overallPhi21"
# vs its complement (default in case of one hypothesis); here: "overallPhi12 > overallPhi21"
# Btw if signs can be negative one perhaps better use:
# H1 <- "abs(overallPhi12) < abs(overallPhi21)"
#
# Evaluate dominance of cross-lagged effects via AIC-type criterion called the GORICA (Altinisik, Nederhof, Hoijtink, Oldehinkel, Kuiper, unpublished).
# GORICA (default)
set.seed(123) # for reproducability of results and possible sensitivity check of penalty
goricaResult <- goric(out_CTmeta, hypotheses = list(H1))
#summary(goricaResult)
goricaResult
#
#
## AIC
H0 <- "overallPhi12 = overallPhi21"
# or: H0 <- "abs(overallPhi12) = abs(overallPhi21)"
goricaResult_H0 <- goric(out_CTmeta, hypotheses = list(H0))
#summary(goricaResult_H0)
goricaResult_H0


##### Automate this such that I obtain easier/faster the results for multiple time-intervals:

# Needed in the GLS/multivariate meta-an
sub = NULL
for(i in 1:q){
  sub = c(sub, paste(i, 1:q, sep=""))
}
outcome <- rep(sub, S) 


# CTmeta for multiple targeted time-intervals
G <- length(unique(TI))
Phi_Trans <- matrix(NA, ncol=(q^2), nrow=G)
sePhi_Trans <- matrix(NA, ncol=(q^2), nrow=G)
CovMxPhi_Trans <- array(NA, dim = c(G, q^2, q^2))
for(g in 1:G){
  #g <- 1
  metaan <- rma.mv(yi=vecPhi_G[,g], V=CovMx_G[,,g], mods = ~ outcome - 1, method = "FE") 
  Phi_Trans[g,] <- coef(metaan)
  sePhi_Trans[g,] <- metaan$se
  CovMxPhi_Trans[g,,] <- metaan$vb
}


## Apply GORICA ##
H0 <- "Phi12 = Phi21"
H1 <- "Phi12 < Phi21"
#
# On overall Phi #
GORICAweights_g <- array(NA, dim = c(2, G))
GORICA_g <- array(NA, dim = c(2, G))
LL_g <- array(NA, dim = c(2, G))
PT_g <- array(NA, dim = c(2, G))
GORICAweights_g_H0 <- array(NA, dim = c(2, G))
GORICA_g_H0 <- array(NA, dim = c(2, G))
LL_g_H0 <- array(NA, dim = c(2, G))
PT_g_H0 <- array(NA, dim = c(2, G))
for(g in 1:G){
  #g <- 1
  est <- Phi_Trans[g,]
  names(est) <- c("Phi11", "Phi12", "Phi21", "Phi22")
  VCOV <- CovMxPhi_Trans[g,,]
  set.seed(123)
  results_g <- goric(est, VCOV = VCOV, hypotheses = list(H1)) 
  #summary(results_g)
  GORICAweights_g[, g] <- results_g$result[,5]
  GORICA_g[, g] <- results_g$result[,4]
  LL_g <- results_g$result[,2]
  PT_g <- results_g$result[,3]
  #
  # AIC: H0 vs Hunc
  set.seed(123)
  results_g_H0 <- goric(est, VCOV = VCOV, hypotheses = list(H0)) 
  #summary(results_g_H0)
  GORICAweights_g_H0[, g] <- results_g_H0$result[,5]
  GORICA_g_H0[, g] <- results_g_H0$result[,4]
  LL_g_H0 <- results_g_H0$result[,2]
  PT_g_H0 <- results_g_H0$result[,3]
}
#
# On study-specific Phi; on Phi from each of the primary studies #
GORICAweights_g_s <- array(NA, dim = c(2, S, G))
GORICA_g_s <- array(NA, dim = c(2, S, G))
LL_g_s <- array(NA, dim = c(2, S, G))
PT_g_s <- array(NA, dim = c(2, S, G))
GORICAweights_g_s_H0 <- array(NA, dim = c(2, S, G))
GORICA_g_s_H0 <- array(NA, dim = c(2, S, G))
LL_g_s_H0 <- array(NA, dim = c(2, S, G))
PT_g_s_H0 <- array(NA, dim = c(2, S, G))
for(g in 1:G){
  for(s in 1:S){
    #g <- 1; s <- 1
    est <- vecPhi_G[((s-1)*(q*q)+1):(s*q*q), g]
    names(est) <- c("Phi11", "Phi12", "Phi21", "Phi22")
    VCOV <- CovMx_G[((s-1)*(q*q)+1):(s*q*q), ((s-1)*(q*q)+1):(s*q*q), g]
    set.seed(123)
    results_g_s <- goric(est, VCOV = VCOV, hypotheses = list(H1)) 
    #summary(results_g_s)
    GORICAweights_g_s[, s, g] <- results_g_s$result[,5]
    GORICA_g_s[, s, g] <- results_g_s$result[,4]
    LL_g_s[, s, g] <- results_g_s$result[,2]
    PT_g_s[, s, g] <- results_g_s$result[,3]
    #
    # AIC: H0 vs Hunc
    set.seed(123)
    results_g_s_H0 <- goric(est, VCOV = VCOV, hypotheses = list(H0)) 
    #summary(results_g_s_H0)
    GORICAweights_g_s_H0[, s, g] <- results_g_s_H0$result[,5]
    GORICA_g_s_H0[, s, g] <- results_g_s_H0$result[,4]
    LL_g_s_H0[, s, g] <- results_g_s_H0$result[,2]
    PT_g_s_H0[, s, g] <- results_g_s_H0$result[,3]
  }
}
#
# Output: Table
table <- cbind(
  unique(TI)[order(unique(TI))],
  GORICAweights_g[1,order(unique(TI))], 
  apply(GORICAweights_g_s[1,,order(unique(TI))], 2, min),
  apply(GORICAweights_g_s[1,,order(unique(TI))], 2, max),
  apply(GORICAweights_g_s[1,,order(unique(TI))], 2, mean),
  apply(GORICAweights_g_s[1,,order(unique(TI))], 2, sd)
)
rownames(table) <- rep("", G)
table
#
# AIC: H0 vs Hunc
table_H0 <- cbind(
  unique(TI)[order(unique(TI))],
  GORICAweights_g_H0[2,order(unique(TI))], 
  apply(GORICAweights_g_s_H0[2,,order(unique(TI))], 2, min),
  apply(GORICAweights_g_s_H0[2,,order(unique(TI))], 2, max),
  apply(GORICAweights_g_s_H0[2,,order(unique(TI))], 2, mean),
  apply(GORICAweights_g_s_H0[2,,order(unique(TI))], 2, sd)
)
rownames(table_H0) <- rep("", G)
table_H0


#--- Save and Load (for new data)
save(list = ls(), file = "Example_GORICAonCTmeta_vsAIC.RData")


################

# Output: Plot
  op <- par(mfrow=c(1,1)) 
  Col <- 1
  title <- as.list(expression(paste("GORICA weights for ", H["1"], " for the targeted time-intervals ", Delta[t]^"*"))) 
  plot(y=GORICAweights_g[1,][order(unique(TI))], x=unique(TI)[order(unique(TI))], type="p", 
       ylim=
         c((min(GORICAweights_g[1,], GORICAweights_g_s[1,,]) - 0.05), 
           max(GORICAweights_g[1,], GORICAweights_g_s[1,,])), 
       ylab = expression(paste("GORICA weights")), xlab = expression(paste("Time-interval (", Delta[t]^"*", ")", sep="")), 
       col=Col, lwd=1, lty=1, pch = 15,
       main = mtext(do.call(expression, title), side=3)
  )
  #
  #
  #lines(y=GORICAweights_g[1,][order(unique(TI))], x=unique(TI)[order(unique(TI))], col=Col, lwd=0.5, lty=1, type = "l")
  #
  for(s in 1:S){
    # s <- 1
    lines(y=GORICAweights_g_s[1,s,], x=rep(unique(TI)[s], G), col=(1+s), lwd=1, lty=1, type = "p", pch = 1)
  }
  #
  #
  e1 <- "on CTmeta estimates"
  e2 <- "on study estimates"
  legend = c(e1,e2)
  pos <- "bottomright"
  legend(pos,
         legend = legend, cex=1,
         bty = "n",
         lty=rep(0,2), # gives the legend appropriate symbols (no lines)
         lwd=rep(0,2), 
         pch = c(15,1),
         col=c(1,"grey")
  )
  #
  #
  #dev.copy(pdf, paste0('Plot_GORICAonCTmeta_Example.pdf'))
  dev.copy(png, paste0('Plot_GORICAonCTmeta_Example.png'))
  dev.off()
  par(op)
  #
  #
  #
  # AIC: H0 vs Hunc
  op <- par(mfrow=c(1,1)) 
  Col <- 1
  title <- as.list(expression(paste("GORICA weights for ", H["u"], " for the targeted time-intervals ", Delta[t]^"*"))) 
  plot(y=GORICAweights_g_H0[2,][order(unique(TI))], x=unique(TI)[order(unique(TI))], type="p", 
       ylim=
         c((min(GORICAweights_g_H0[2,], GORICAweights_g_s_H0[2,,]) - 0.05), 
           max(GORICAweights_g_H0[2,], GORICAweights_g_s_H0[2,,])), 
       ylab = expression(paste("GORICA weights")), xlab = expression(paste("Time-interval (", Delta[t]^"*", ")", sep="")), 
       col=Col, lwd=1, lty=1, pch = 15,
       main = mtext(do.call(expression, title), side=3)
  )
  #
  #
  #lines(y=GORICAweights_g_H0[1,][order(unique(TI))], x=unique(TI)[order(unique(TI))], col=Col, lwd=0.5, lty=1, type = "l")
  #
  for(s in 1:S){
    # s <- 1
    lines(y=GORICAweights_g_s_H0[2,s,], x=rep(unique(TI)[s], G), col=(1+s), lwd=1, lty=1, type = "p", pch = 1)
  }
  #
  #
  e1 <- "on CTmeta estimates"
  e2 <- "on study estimates"
  legend = c(e1,e2)
  pos <- "bottomright"
  legend(pos,
         legend = legend, cex=1,
         bty = "n",
         lty=rep(0,2), # gives the legend appropriate symbols (no lines)
         lwd=rep(0,2), 
         pch = c(15,1),
         col=c(1,"grey")
  )
  #
  #
  #dev.copy(pdf, paste0('Plot_GORICAonCTmeta_Example_AIC.pdf'))
  dev.copy(png, paste0('Plot_GORICAonCTmeta_Example_AIC.png'))
  dev.off()
  par(op)

