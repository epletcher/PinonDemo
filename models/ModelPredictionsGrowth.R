## set working directory to 'PinonDemo'

## load model fitting workspace generated from 'ModelPreppingGrowth'
# contains the fit ss growth model 'PinonDemo/models/growth_years_statespace.stan'
# for Bob and Elise this is located here: 
# SEV_PJ_Demo/model_output_workspaces/growth_statespace_model.RData


## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## extract parameters from ss growth model
# (no year effect here)
growth.params.ss <- 
  as.matrix(growth_ss, pars = c("beta0","beta1","sigp","sigo")) %>% as.data.frame()

Szl <- rstan::extract(growth_ss, pars = 'Szl')[[1]] # latent sizes

## Re-assign NA's to any latent values outside of the census endpoints

Szl.filt <- array(NA, dim = dim(Szl))

for(i in 1:length(tcy[,1])) { # for every tree
  
  incens <- seq(tcy[i,1],tcy[i,2],1) # years within census
  
  Szl.filt[,incens,i] <- Szl[,incens,i] 
  
}

## extract median and upper and lower cis for the latent state (for plotting)

med.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = median)

lo.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = quantile, 0.05, na.rm = T)

hi.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = quantile, 0.95, na.rm = T)


#
# ------- Explore estimated size latent states ---------
#plot latent state annual growth time series overlayed on top of observed annual growth time series (possibly for a subset of trees)

# latent growth
matplot(exp(med.Szl), type = "l", lty = 1, col = rgb(red = 0, green = 0.5, blue = 0.6, alpha = 0.6))

# observed growth
matplot(Sz.obs, type = "l", lty = 1, col = rgb(red = 0, green = 0.5, blue = 0.6, alpha = 0.6))

# latent growth overlayed on observed growth (for a few trees)
plot(Sz.obs[,1], ylim = c(0,7), pch = 16)
lines(exp(med.Szl[,1]))
lines(exp(lo.Szl[,1]), lty = 2)
lines(exp(hi.Szl[,1]), lty = 2)
points(Sz.obs[,26], col = 'coral', pch = 16)
lines(exp(med.Szl[,26]), lty = 2, col = 'coral')
lines(exp(lo.Szl[,26]), lty = 2, col = 'coral')
lines(exp(hi.Szl[,26]), lty = 2, col = 'coral')
points(Sz.obs[,3], col = 'purple', pch = 16)
lines(exp(med.Szl[,3]), lty = 2, col = 'purple')
lines(exp(lo.Szl[,3]), lty = 2, col = 'purple')
lines(exp(hi.Szl[,3]), lty = 2, col = 'purple')

#
# ------- GENERATE PREDICITONS ---------

# Use latent states from fit model as size at t-1
# Generate predictions for year effect growth model
# (no year effect)

# empty array for predictions
Sz.pred <- array(NA,c(dim(Szl.filt)[1],dim(Szl.filt)[2],dim(Szl.filt)[3]))

# loop over iterations, and through years
# ** process error is not propagaed in this current form, latent size at t-1 is used each year to predict size the next year, not the new predictions
for(k in 1:dim(Szl.filt)[1]) {
  
  for(t in 2:dim(Szl.filt)[2]) { 
    
    # using latent size at t-1, and sigp to generate predictions here
    Sz.pred[k,t,] <- rnorm(dim(Szl.filt)[3], exp(growth.params.ss$beta0[k] + growth.params.ss$beta1[k]*Szl.filt[k,t-1,]), growth.params.ss$sigp[k]) # eponentiate
    
  }
  
}

# credible intervals and median predictions for plottting
med.Sz <- apply(Sz.pred, MARGIN = c(2,3), FUN = median)
lo.Sz <- apply(Sz.pred, MARGIN = c(2,3), FUN = quantile, 0.05, na.rm = T)
hi.Sz <- apply(Sz.pred, MARGIN = c(2,3), FUN = quantile, 0.95, na.rm = T)
 
#
# ----------- PLOT PREDICTIONS -----------

# plot predictions against latent states
plot(Sz.pred[50,,],exp(Szl.filt[50,,])) # picking a random iteration

# plot predictions on top of observed sizes for a handful of trees
plot(Sz.obs[,1], ylim = c(0,7), pch = 16)
lines(med.Sz[,1])
lines(lo.Sz[,1], lty = 2)
lines(hi.Sz[,1], lty = 2)
points(Sz.obs[,26], col = 'coral', pch = 16)
lines(med.Sz[,26], col = 'coral')
lines(lo.Sz[,26], lty = 2, col = 'coral')
lines(hi.Sz[,26], lty = 2, col = 'coral')
points(Sz.obs[,3], col = 'purple', pch = 16)
lines(med.Sz[,3], col = 'purple')
lines(lo.Sz[,3], lty = 2, col = 'purple')
lines(hi.Sz[,3], lty = 2, col = 'purple')

#
# --------- POSTERIOR PREDICTIVE CHECK -------------

## simulate fake data

# 'fake data' are simulated using estimated latent states and all iterations of fit model

Sz.sim <- array(NA,c(dim(Szl.filt)[1],dim(Szl.filt)[2],dim(Szl.filt)[3])) # empty array

# simulate data across all model iterations (and all years)

for(k in 1:dim(Szl.filt)[1]) {
  
  for(t in 1:dim(Szl.filt)[2]) {
    
    Sz.sim[k,t,] <- rnorm(dim(Sz.sim)[3],exp(Szl.filt[k,t,]),growth.params.ss$sigo[k]) # eponentiate
    
  }
    
}

## Calculate bayesian p value using deviance

# Exclude latent estimates and simulated data for where Observations are actually missing data
# do this for both Szl.filt and Sz.sim, assign NAs so they match where Sz.obs has nas

naspots<-which(is.na(Sz.obs)==T,arr.ind=T)

Sz.sim.ppc <- Sz.sim

for(k in 1:dim(Szl.filt)[1]){
  
  
  Sz.sim.ppc[k,napots[1,],napots[,2]]<-NA
  
  # Sz.iter <- Sz.sim.ppc[k,,]
  # Sz.iter[is.na(Sz.obs)] <- NA  
  # Sz.sim.ppc[k,,] <- Sz.iter
  # 
  # Szl.iter <- Sz.filt.ppc[k,,]
  # Szl.iter[is.na(Sz.obs)] <- NA  
  # Szl.filt.ppc[k,,] <- Szl.iter
  
}

devsim <- rep(NA,dim(Szl.filt)[3])
devobs <- rep(NA,dim(Szl.filt)[3])

for(k in 1:dim(Szl.filt)[3]) {
  
  # normal dist
  devsim[k] <- -2*sum(dnorm(Sz.sim.ppc[k,,], exp(Szl.filt[k,,]), growth.params.ss$sigo[k], log = T), na.rm = T)
  devobs[k] <- -2*sum(dnorm(Sz.obs, exp(Szl.filt[k,,]), growth.params.ss$sigo[k], log = T), na.rm = T)

}

# check na length
# need to figure out why the length of nas are wrong
sum(is.na(dnorm(Sz.sim.ppc[k,,], exp(Szl.filt[k,,]), growth.params.ss$sigo[k], log = T))==F)
sum(is.na(dnorm(Sz.obs, exp(Szl.filt[k,,]), growth.params.ss$sigo[k], log = T)==F))

# pvalue
pval = 0

for(k in 1:dim(Szl.filt)[3]) {
  
  if(devsim[k]>devobs[k]) {pval=pval+1}
  
}

pval/dim(Szl.filt)[3]

hist(devobs, col=rgb(0,0,1,1/4), ylim = c(0,100), xlim = c(-2200,-400), main = 'red = devsim, blue = devobs')  # blue
hist(devsim, col=rgb(1,0,0,1/4), ylim = c(0,100), xlim = c(-2200,-400), add=T)  # red

