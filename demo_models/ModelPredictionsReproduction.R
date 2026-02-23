## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## load workspace generated from running model in 'ModelPreppingReproduction.R' script

# for bob load this workspace: load("YOUR_FILEPATH/SEV_PJ_Demo/model_output_workspaces/reproduction_negbinom_woquad.RData")

## extract reproduction model params from fit model
repro.params <- 
  as.matrix(reprofit4) %>% as.data.frame()

saveRDS(repro.params, "demo_models/posterior_estimates/repro_params.rds")

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

## --- DEVIANCE - PPC ---
## separately for each year

# empty matrices
# low growth
devsim.repro.lg <- matrix(NA,y,length(alpha1[,1]))
devobs.repro.lg <- matrix(NA,y,length(alpha1[,1]))
# avg growth
devsim.repro.mg <- matrix(NA,y,length(alpha1[,1]))
devobs.repro.mg <- matrix(NA,y,length(alpha1[,1]))
# high growth
devsim.repro.hg <- matrix(NA,y,length(alpha1[,1]))
devobs.repro.hg <- matrix(NA,y,length(alpha1[,1]))

# calculate deviance and print bayesian p value per growth sceanrio, per year
for(t in 1:y) {

  for(k in 1:length(alpha1[,1])) {
    
    # nbinom
    # low growth
    devsim.repro.lg[t,k] <- -2*sum(dnbinom(x=cp.yhat.lg[,t,k], mu=cp.lambda.lg[,t,k], size=phi1[k], log = T), na.rm = T)
    devobs.repro.lg[t,k] <- -2*sum(dnbinom(x=cp.obs[t,], mu=cp.lambda.lg[,t,k], size=phi1[k], log = T), na.rm = T) 
    
    # avg growth
    devsim.repro.mg[t,k] <- -2*sum(dnbinom(x=cp.yhat.mg[,t,k], mu=cp.lambda.mg[,t,k], size=phi2[k], log = T), na.rm = T)
    devobs.repro.mg[t,k] <- -2*sum(dnbinom(x=cp.obs[t,], mu=cp.lambda.mg[,t,k], size=phi2[k], log = T), na.rm = T) 
    
    # high growth
    devsim.repro.hg[t,k] <- -2*sum(dnbinom(x=cp.yhat.hg[,t,k], mu=cp.lambda.hg[,t,k], size=phi3[k], log = T), na.rm = T)
    devobs.repro.hg[t,k] <- -2*sum(dnbinom(x=cp.obs[t,], mu=cp.lambda.hg[,t,k], size=phi3[k], log = T), na.rm = T) 
    
  }
  
  print(paste("Year:",as.character(t)))
  
  pval.lg = 0
  pval.mg = 0
  pval.hg = 0
  
  for(k in 1:length(alpha1[,1])) {
    
    if(devsim.repro.lg[t,k]>devobs.repro.lg[t,k]) {pval.lg=pval.lg+1}
    if(devsim.repro.mg[t,k]>devobs.repro.mg[t,k]) {pval.mg=pval.mg+1}
    if(devsim.repro.hg[t,k]>devobs.repro.hg[t,k]) {pval.hg=pval.hg+1}
    
  }
  # between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
  print(pval.lg/length(alpha1[,1]))
  print(pval.mg/length(alpha1[,1]))
  print(pval.hg/length(alpha1[,1]))
  
}

# index histogram by year
yy=1
  
  hist(devobs.repro.lg[yy,], col=rgb(0,0,1,1/4), xlim = c(0,1000))  
  hist(devsim.repro.lg[yy,], col=rgb(1,0,0,1/4), add = T)  
  
  hist(devobs.repro.mg[yy,], col=rgb(0,0,1,1/4), xlim = c(0,1000))
  hist(devsim.repro.mg[yy,], col=rgb(1,0,0,1/4), add = T)
  
  hist(devobs.repro.hg[yy,], col=rgb(0,0,1,1/4), xlim = c(0,1000))
  hist(devsim.repro.hg[yy,], col=rgb(1,0,0,1/4), add = T)

## --- Calculate bayesian p value using MEAN ---

# low growth
musim.repro.lg <- apply(cp.yhat.lg, MARGIN = c(2,3), FUN = mean, na.rm = T)

# avg growth
musim.repro.mg <- apply(cp.yhat.mg, MARGIN = c(2,3), FUN = mean, na.rm = T)

# high growth
musim.repro.hg <- apply(cp.yhat.hg, MARGIN = c(2,3), FUN = mean, na.rm = T)

# mean obs
muobs.repro <- rowMeans(cp.obs, na.rm = T)

# bayes p val for every year

for(t in 1:y) {
  print(t)
  
  pval.lg = 0
  pval.mg = 0
  pval.hg = 0
  
    for(k in 1:length(alpha1[,1])) {
      
      if(musim.repro.lg[y,k]>muobs.repro[y]) {pval.lg=pval.lg+1}
      if(musim.repro.mg[y,k]>muobs.repro[y]) {pval.mg=pval.mg+1}
      if(musim.repro.hg[y,k]>muobs.repro[y]) {pval.hg=pval.hg+1}
      
    }
    # between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
    print(pval.lg/length(alpha1[,1]))
    print(pval.mg/length(alpha1[,1]))
    print(pval.hg/length(alpha1[,1]))
    
}

for(t in 1:y) {
  
  hist(musim.repro.lg[y,], col=rgb(1,0,0,1/4))
  abline(v = quantile(musim.repro.lg[y,], probs = c(0.05, 0.95)),
         col = "red", lwd = 2)
  abline(v = muobs.repro[y,], col='blue', lwd = 2)  # blue
  
  hist(musim.repro.mg[y,], col=rgb(1,0,0,1/4))
  abline(v = quantile(musim.repro.mg[y,], probs = c(0.05, 0.95)),
         col = "red", lwd = 2)
  abline(v = muobs.repro[y,], col='blue', lwd = 2)  # blue
  
  hist(musim.repro.hg[y,], col=rgb(1,0,0,1/4))
  abline(v = quantile(musim.repro.hg[y,], probs = c(0.05, 0.95)),
         col = "red", lwd = 2)
  abline(v = muobs.repro[y,], col='blue', lwd = 2)  # blue
  
}
