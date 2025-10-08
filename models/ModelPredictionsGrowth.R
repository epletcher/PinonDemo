## set working directory to 'PinonDemo'

## load model fitting workspace generated from 'ModelPreppingGrowth'
# contains the fit 'growth_years_statespace.stan' growth model
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

Szl <- extract(growth_ss, pars = 'Szl')[[1]] # latent sizes

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

for(k in 1:dim(Szl.filt)[1]) {
  
  for(t in 2:dim(Szl.filt)[2]) { 
    
    # using latent size at t-1, and sigO to generate predictions here
    Sz.pred[k,t,] <- rnorm(dim(Szl.filt)[3], exp(growth.params.ss$beta0[k] + growth.params.ss$beta1[k]*Szl.filt[k,t-1,]), growth.params.ss$sigo[k]) # eponentiate
    
  }
  
}

# remove first year
Sz.pred<-Sz.pred[,2:11,]    
#
# --------------- PLOT PREDICTIONS AGAINST LATENT STATES -----------

plot(Sz.pred[50,,],exp(Szl.filt[50,2:11,])) # picking a random iteration

