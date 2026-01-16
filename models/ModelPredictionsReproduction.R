## load packages
library(tidyverse)
library(rstan)
library(shinystan)
library(LaplacesDemon)

## load workspace generated from running model in 'ModelPreppingReproduction.R' script

## extract reproduction model params from fit model
repro.params <- 
  as.matrix(reprofit2) %>% as.data.frame()

## Convert alpha's and beta's from reproduction models into matrices
alpha1 <- repro.params %>% select(starts_with('alpha1[')) %>% as.matrix()
alpha2 <- repro.params %>% select(starts_with('alpha2[')) %>% as.matrix()
alpha3 <- repro.params %>% select(starts_with('alpha3[')) %>% as.matrix()

beta1 <- repro.params %>% select(starts_with('beta1[')) %>% as.matrix()
beta2 <- repro.params %>% select(starts_with('beta2[')) %>% as.matrix()
beta3 <- repro.params %>% select(starts_with('beta3[')) %>% as.matrix()


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
  hg.cone.pred[,t,k] <- exp(alpha3[k,t] + beta3[k,t]*Sz.range)
  
  }
  
}

matplot(lg.cone.pred[,,3], type = "l")
matplot(mg.cone.pred[,,3], type = "l")
matplot(hg.cone.pred[,,3], type = "l")

# ----------- PPC -------------

# **** editing here

y = dim(Surv.obs)[1]

# generate predicted cone prediction using observed sizes
cp.yhat <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))

for (k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
    
    cp.yhat[,t,k] <- rpois(length(cp.obs[1,]), exp(alpha1[k,t] + beta1[k,t]*Sz.obs[t,]))
    
  }
  
}

## Calculate bayesian p value using deviance
devsim.repro <- rep(NA,length(alpha1[,1]))
devobs.repro <- rep(NA,length(alpha1[,1]))


for(k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
  
  devsim.repro[k] <- -2*sum(dpois(cp.yhat[,t,k], Sz.obs[t,], log = T), na.rm = T)
  devobs.repro[k] <- -2*sum(dpois(cp.obs, Sz.obs[t,], log = T), na.rm = T) # *** this is not right
  
  }
  
}

pval = 0

for(k in 1:length(alpha1[,1])) {
  
  if(devsim.repro[k]>devobs.repro[k]) {pval=pval+1}
  
}
# between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
pval/length(alpha1[,1])

hist(devobs.repro, col=rgb(0,0,1,1/4))  
hist(devsim.repro, col=rgb(1,0,0,1/4))  



