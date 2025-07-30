## set working directory to 'PinonDemo'

## load model fitting workspace generated from 'ModelPreppingGrowth'

## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## extract parameters
growth.params.ss <- 
  as.matrix(growth_st, pars = c("beta0","beta1","sigp","sigo")) %>% as.data.frame()



# ------- GENERATE PREDICITONS ---------
# Questions:
# do i want to use the latent states here?
# are the divergent divergent transitions after warm up just related to the NA's?


# vector of Size_tmins for generating preds
Sz.min.range <- seq(0.75,7,0.05)

## Generate predictions for year effect growth model
mean.pred.range <- array(NA,c(length(St.min.range),y,length(growth.params$`beta0[1]`)))

for (k in 1:length(growth.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
    mean.pred.range[,t,k] <- g.beta0[k,t] + g.beta1[k,t]*log(St.min.range)
    
  }
}

