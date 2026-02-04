## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## load workspace generated from running model in 'ModelPreppingReproduction.R' script

# for bob load this workspace: load("YOUR_FILEPATH/SEV_PJ_Demo/model_output_workspaces/reproduction_negbinom_woquad.RData")

## extract reproduction model params from fit model
repro.params <- 
  as.matrix(reprofit3) %>% as.data.frame()

## Convert alpha's and beta's from reproduction models into matrices
alpha1 <- repro.params %>% select(starts_with('alpha1[')) %>% as.matrix()
alpha2 <- repro.params %>% select(starts_with('alpha2[')) %>% as.matrix()
alpha3 <- repro.params %>% select(starts_with('alpha3[')) %>% as.matrix()

beta1 <- repro.params %>% select(starts_with('beta1[')) %>% as.matrix()
beta2 <- repro.params %>% select(starts_with('beta2[')) %>% as.matrix()
beta3 <- repro.params %>% select(starts_with('beta3[')) %>% as.matrix()

phi1 <- repro.params %>% select('phi1') %>% as.matrix()
phi2 <- repro.params %>% select('phi2') %>% as.matrix()
phi3 <- repro.params %>% select('phi3') %>% as.matrix()

# ------ generate preds -------
# vector of Size_tmins for generating preds
Sz.range <- seq(1,8,0.5)

# preds
lg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha1[,1])))
mg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha2[,1])))
hg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha3[,1])))

for (k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
  
  lg.cone.pred[,t,k] <- exp(alpha1[k,t] + beta1[k,t]*Sz.range)
  mg.cone.pred[,t,k] <- exp(alpha2[k,t] + beta2[k,t]*Sz.range)
  hg.cone.pred[,t,k] <- exp(alpha3[k,t] + beta3[k,t]*Sz.range) #  + beta3_2[k,t]*Sz.range^2


  }
  
}

# all trees, mean posterior estimate
matplot(apply(lg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")
matplot(apply(mg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")
matplot(apply(hg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")

# ----------- Posterior predictive check -------------

# simulated cone production from fit model using observed sizes

cp.yhat.lg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.lg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.yhat.mg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.mg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.yhat.hg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.hg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))

for (k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
    
    #nbinom
    cp.yhat.lg[,t,k] <- rnbinom(n=length(cp.obs[1,]), mu=exp(alpha1[k,t] + beta1[k,t]*Sz[t,,1]),size=phi1[k])
    cp.lambda.lg[,t,k] <- exp(alpha1[k,t] + beta1[k,t]*Sz[t,,1])
    
    cp.yhat.mg[,t,k] <- rnbinom(n=length(cp.obs[1,]), mu=exp(alpha2[k,t] + beta2[k,t]*Sz[t,,2]),size=phi2[k])
    cp.lambda.mg[,t,k] <- exp(alpha2[k,t] + beta2[k,t]*Sz[t,,2])
    
    cp.yhat.hg[,t,k] <- rnbinom(n=length(cp.obs[1,]), mu=exp(alpha3[k,t] + beta3[k,t]*Sz[t,,3]),size=phi3[k])
    cp.lambda.hg[,t,k] <- exp(alpha3[k,t] + beta3[k,t]*Sz[t,,3])
    
  }
  
}

## --- Calculate bayesian p value using DEVIANCE ---

# ** try runnning ppc separately for each year **
yy = 2 # index years here
#yy = 1:26 # for all years

# low growth
devsim.repro.lg <- rep(NA,length(alpha1[,1]))
devobs.repro.lg <- rep(NA,length(alpha1[,1]))
# avg growth
devsim.repro.mg <- rep(NA,length(alpha1[,1]))
devobs.repro.mg <- rep(NA,length(alpha1[,1]))
# high growth
devsim.repro.hg <- rep(NA,length(alpha1[,1]))
devobs.repro.hg <- rep(NA,length(alpha1[,1]))

for(k in 1:length(alpha1[,1])) {
  
  # nbinom
  # low growth
  devsim.repro.lg[k] <- -2*sum(dnbinom(x=cp.yhat.lg[,yy,k], mu=cp.lambda.lg[,yy,k], size=phi1[k], log = T), na.rm = T)
  devobs.repro.lg[k] <- -2*sum(dnbinom(x=cp.obs[yy,], mu=cp.lambda.lg[,yy,k], size=phi1[k], log = T), na.rm = T) 
  
  # avg growth
  devsim.repro.mg[k] <- -2*sum(dnbinom(x=cp.yhat.mg[,yy,k], mu=cp.lambda.mg[,yy,k], size=phi2[k], log = T), na.rm = T)
  devobs.repro.mg[k] <- -2*sum(dnbinom(x=cp.obs[yy,], mu=cp.lambda.mg[,yy,k], size=phi2[k], log = T), na.rm = T) 
  
  # high growth
  devsim.repro.hg[k] <- -2*sum(dnbinom(x=cp.yhat.hg[,yy,k], mu=cp.lambda.hg[,yy,k], size=phi3[k], log = T), na.rm = T)
  devobs.repro.hg[k] <- -2*sum(dnbinom(x=cp.obs[yy,], mu=cp.lambda.hg[,yy,k], size=phi3[k], log = T), na.rm = T) 
  
}

pval.lg = 0
pval.mg = 0
pval.hg = 0

for(k in 1:length(alpha1[,1])) {
  
  if(devsim.repro.lg[k]>devobs.repro.lg[k]) {pval.lg=pval.lg+1}
  if(devsim.repro.mg[k]>devobs.repro.mg[k]) {pval.mg=pval.mg+1}
  if(devsim.repro.hg[k]>devobs.repro.hg[k]) {pval.hg=pval.hg+1}
  
}
# between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
pval.lg/length(alpha1[,1])
pval.mg/length(alpha1[,1])
pval.hg/length(alpha1[,1])

hist(devobs.repro.lg, col=rgb(0,0,1,1/4), xlim = c(0,1000))  
hist(devsim.repro.lg, col=rgb(1,0,0,1/4), add = T)  

hist(devobs.repro.mg, col=rgb(0,0,1,1/4), xlim = c(0,1000))
hist(devsim.repro.mg, col=rgb(1,0,0,1/4), add = T)

hist(devobs.repro.hg, col=rgb(0,0,1,1/4), xlim = c(0,1000))
hist(devsim.repro.hg, col=rgb(1,0,0,1/4), add = T)

## --- Calculate bayesian p value using MEAN ---
# ** editting in progress here **
# # low growth
# musim.repro.lg <- apply(cp.yhat.lg, MARGIN = c(2,3), FUN = mean, na.rm = T)
# 
# # avg growth
# musim.repro.mg <- apply(cp.yhat.mg, MARGIN = c(2,3), FUN = mean, na.rm = T)
# 
# # high growth
# musim.repro.hg <- apply(cp.yhat.hg, MARGIN = c(2,3), FUN = mean, na.rm = T)
# 
# # mean obs
# muobs.repro <- rowMeans(cp.obs, na.rm = T)
# 
# # ** try runnning ppc separately for each year **
# yy = 2 # index years here
# 
# pval.lg = 0
# pval.mg = 0
# pval.hg = 0
# 
# for(k in 1:length(alpha1[,1])) {
#   
#   if(musim.repro.lg[yy,k]>muobs.repro[yy]) {pval.lg=pval.lg+1}
#   if(musim.repro.mg[yy,k]>muobs.repro[yy]) {pval.mg=pval.mg+1}
#   if(musim.repro.hg[yy,k]>muobs.repro[yy]) {pval.hg=pval.hg+1}
#   
# }
# # between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
# pval.lg/length(alpha1[,1])
# pval.mg/length(alpha1[,1])
# pval.hg/length(alpha1[,1])
# 
# hist(muobs.repro, col=rgb(0,0,1,1/4), xlim = c(0,1000))  
# hist(musim.repro.lg, col=rgb(1,0,0,1/4), add = T)  
# 
# hist(muobs.repro, col=rgb(0,0,1,1/4), xlim = c(0,1000))
# hist(musim.repro.mg, col=rgb(1,0,0,1/4), add = T)
# 
# hist(muobs.repro, col=rgb(0,0,1,1/4), xlim = c(0,1000))
# hist(musim.repro.hg, col=rgb(1,0,0,1/4), add = T)
