library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

# ------- FIT REPRODUCTION MODEL IN STAN -----

# prep data

# specify model data
reprodata <- list()

#start <- list() # specify starting values, if needed

# fit reproduction model
reprofit1 <- stan(file='models/reproduction.stan', data=reprodata, chains=3, iter=3000, warmup=1500)

  