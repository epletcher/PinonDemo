## set working directory to 'PinonDemo'

## load model fitting workspace generated from 'ModelPreppingGrowth'


## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## extract parameters

growth.params.ss <- 
  as.matrix(growth_ss, pars = c("beta0","beta1","sigp","sigo")) %>% as.data.frame()

Szl <- extract(growth_ss, pars = 'Szl')[[1]]

## assign NA's to any latent values outside of the census endpoints

Szl.filt <- array(NA, dim = dim(Szl))

for(i in 1:length(tcy[,1])) { # for every tree
  
  incens <- seq(tcy[i,1],tcy[i,2],1) # years within census
  
  Szl.filt[,incens,i] <- Szl[,incens,i] 
  
}

## extract median and upper and lower cis for the latent state

med.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = median)

lo.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = quantile, 0.05, na.rm = T)

hi.Szl <- apply(Szl.filt, MARGIN = c(2,3), FUN = quantile, 0.95, na.rm = T)



# ------- Explore estimated latent states ---------
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

# ------- GENERATE PREDICITONS ---------
# Questions:
# do i want to use the latent states here?

# vector of Size_tmins for generating preds
Sz.min.range <- seq(0.75,7,0.05)

## Generate predictions for year effect growth model
# # Generate yhats for no year effect
Sz.mean.pred.range <- matrix(NA,,)

  for(j in 1:dim(Sz)[2]) {
    
    # start year that the tree first shows up (enters the census), end the year the tree dies (leaves the census for good), fill blank matrix for first year's value
    for(t in (tcy[j,1]+1):tcy[j,2]) { 

    Szl[t,j] ~ normal(beta0 + beta1*Szl[t-1,j], sigp);
    
    Sz[t,j] ~ normal(exp(Szl[t,j]), sigo); # exponentiate here
  
}

