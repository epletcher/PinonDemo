library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)
library(abind) # for combining arrays

## set working directory to 'PinonDemo' folder/repository

## load model fitting workspace generated from 'ModelPreppingGrowth'
# contains the fit ss growth model 'PinonDemo/models/growth_years_statespace.stan'
# for Bob and Elise this is located here: 
# SEV_PJ_Demo/model_output_workspaces/growth_statespace_model.RData

## extract parameters from ss growth model
# (no year effect here)
gp <- as.matrix(growth_ss, pars = c("beta0","beta1","sigp","sigo")) %>% as.data.frame()

# ------- Load cone production and height of masting trees ---------

reprodat <- read.csv("cleaned_cone_prod_data.csv") %>% filter(!is.na(tree_height_2024))
  # remove trees that don't have height in 2024

# cp is cone production across trees and years [year,tree]
cp.obs<-cp <- reprodat %>% 
  select(c(Fruit_Count,Year,Field_ID)) %>% 
  filter(Year > 1999) %>% # remove 1997 and 1998, no cones produced by focal trees
  pivot_wider(names_from = Field_ID, values_from = Fruit_Count) %>%
  select(-Year) %>%
  as.matrix()

cp[is.na(cp)]<-999 # for running STAN, convert NAs to 999 value

# ----- estimate past size of mast trees using growth model


# make this a matrix of [year,tree]
Sz.obs <- reprodat %>% 
  rename(height = tree_height_2024) %>% 
  mutate(height = case_when(Year<2024 ~ NA, .default = height)) %>% # add NAs for previous that we need model/estimate height for
  select(c(Field_ID,height,Year)) %>%
  filter(Year > 1999) %>% # remove 1997 and 1998, no cones produced by focal trees
  pivot_wider(names_from = Field_ID, values_from = height) %>%
  select(-Year) %>%
  as.matrix()

colnames(Sz.obs) <- NULL
# Notes: some trees do not have height for 2024, so they will remain as NAs for heights back in time

## Sz.obs.p is array of [year,individuals, all param iterations]

# empty array to fill
Sz.obs.p <- replicate(length(gp$beta0), Sz.obs)

 for (i in 1:length(gp$beta0)) {

   for(t in dim(Sz.obs)[1]:2) { # back casting here, so descending order

     # trees are slowly shrinking?? check this equation
     Sz.obs.p[t-1,,i] <- exp(rnorm(dim(Sz.obs.p)[2], (log(Sz.obs.p[t,,i])-gp$beta0[i])/gp$beta1[i], gp$sigp[i])) # inlcude process error because we become less certain of tree size further into the past

   }

 }

# Visual check of back casted tree heights 
matplot(Sz.obs.p[,35,], type = "l") # plot for a particular tree

# thin Sz.obs.p to 500 iterations
thin.iter <- seq(9,dim(Sz.obs.p)[3], by = 9) # vector of every 10th iteration

# Sz <- log(Sz.obs.p[,,thin.iter]) # model SZ on the log scale
# Sz <- Sz.obs.p[,,thin.iter] # model SZ w/o log scale

# # Visual check of *thinned* back casted tree heights 
# matplot(Sz[,35,], type = "l") # plot for a particular tree

## TO SHARE: 

#a single tree example, all iterations, all years
matplot(Sz.obs.p[,35,], type = "l", lty = 1 , xlab = 'year', ylab = 'height (m)', col = rgb(red = 0, green = 0.5, blue = 0.6, alpha = 0.05)) 
# mean
lines(apply(Sz.obs.p, MARGIN = c(1,2), FUN = mean)[,35], lwd = 2.5)

#all trees mean
matplot(apply(Sz.obs.p, MARGIN = c(1,2), FUN = mean), type = "l", lty = 1 , xlab = 'year', ylab = 'height (m)')


## UPDATE FOR IMPROVE MODEL CONVERGENCE:
## Simplify Sz.obs.p so that it is an array of [year,individuals, mean, lo and hi CI's from growth model]

# low CI/ high growth scenario
Sz.obs.loci <- apply(Sz.obs.p, MARGIN = c(1,2), FUN = quantile, 0.05, na.rm = T)

# mean growth CI / average growth scenario
Sz.obs.avg <- apply(Sz.obs.p, MARGIN = c(1,2), FUN = mean, na.rm = T)

# hi growth CI / low growth scenario
Sz.obs.upci <- apply(Sz.obs.p, MARGIN = c(1,2), FUN = quantile, 0.95, na.rm = T)

# combine
Sz.obs.p3 <- abind(Sz.obs.upci, Sz.obs.avg, Sz.obs.loci, along = 3)

#a single tree example, low mean and hi growth, all years
matplot(Sz.obs.p3[,15,], type = "l", lty = 1 , xlab = 'year', ylab = 'height (m)', col = c("dodgerblue", "black", "coral")) 

#all trees mean
matplot(Sz.obs.p3[,,2], type = "l", lty = 1 , xlab = 'year', ylab = 'height (m)')

#
Sz <- Sz.obs.p3 # for scenarios based model, mode SZ w/o log scale for now

# ------- FIT REPRODUCTION MODEL IN STAN -----
## prep size data for STAN

# dimensions
i = dim(Sz)[2] # individual
y = dim(Sz)[1] # year
k = dim(Sz)[3] # growth model iterations OR growth scenarios (low, avg , high)

# # id for year effect
# yr <- unique(reprodat$Year)[3:27]

# specify model data
# reprodata <- list(i=i,k=k,y=y,yr=yr,Sz=Sz,cp=cp) # year fixed effect

reprodata <- list(i=i,k=k,y=y,Sz=Sz,cp=cp) # year random effect

# fit reproduction model

## state space fitting
# # set initial true size as the mean across param/process uncertainty values
# start <- list(list("tsz"=apply(Sz, MARGIN = c(1,2), FUN = mean)),
#               list("tsz"=apply(Sz, MARGIN = c(1,2), FUN = mean)),
#               list("tsz"=apply(Sz, MARGIN = c(1,2), FUN = mean)))

# options(mc.cores = parallel::detectCores())
# reprofit1 <- stan(file='models/reproduction.stan', data=reprodata, init = start, chains=3, iter=1000, warmup=500) # increase iterations later but need to debug
# 
# reprofit1 
# 
# launch_shinystan(reprofit1)

# ** issues, fitting latent 25 okay, but then bad for all other years, sigs trails off

## scenario based model fitting
# with quad term
# set initial true size as the mean across param/process uncertainty values
# start <- list(list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y)),
#               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y)),
#               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y)))
# 
# options(mc.cores = parallel::detectCores())
# reprofit2 <- stan(file='models/reproductionv2.stan', data=reprodata, chains=3, init=start, iter=3000, warmup=1500) # run for longer after i 
# reprofit2
# 
# launch_shinystan(reprofit2)

# save.image("G:/.shortcut-targets-by-id/1cGvc8VT3uIwM5NtkFk0RLP-xj4tptJAg/SEV_PJ_Demo/model_output_workspaces/reproduction_quad.RData")

# w/o quad term
# set initial true size as the mean across param/process uncertainty values
# start2 <- list(list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y)),
#               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y)),
#               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y)))
# 
# options(mc.cores = parallel::detectCores())
# reprofit3 <- stan(file='models/reproductionv2.stan', data=reprodata, chains=3, init=start2, iter=3000, warmup=1500) # run for longer after i 
# reprofit3
# 
# launch_shinystan(reprofit3)
# 
# save.image("G:/.shortcut-targets-by-id/1cGvc8VT3uIwM5NtkFk0RLP-xj4tptJAg/SEV_PJ_Demo/model_output_workspaces/reproduction_wo_quad.RData")

# w/ quad term, negative binomial
# set initial true size as the mean across param/process uncertainty values
start3 <- list(list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y), "phi1"=0.1, "phi2"=0.1, "phi3"=0.1),
               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y), "phi1"=0.1, "phi2"=0.1, "phi3"=0.1),
               list("alpha1"=rep(0.01,y),"alpha2"=rep(0.01,y),"alpha3"=rep(0.01,y),"beta1"=rep(1,y),"beta2"=rep(1,y),"beta3"=rep(1,y),"beta1_2"=rep(-.1,y),"beta2_2"=rep(-.1,y),"beta3_2"=rep(-.1,y), "phi1"=0.1, "phi2"=0.1, "phi3"=0.1))

options(mc.cores = parallel::detectCores())
reprofit3 <- stan(file='models/reproductionv2.stan', data=reprodata, chains=3, init=start3, iter=5000, warmup=2500) # run for longer after i 
reprofit3

launch_shinystan(reprofit3)

save.image("G:/.shortcut-targets-by-id/1cGvc8VT3uIwM5NtkFk0RLP-xj4tptJAg/SEV_PJ_Demo/model_output_workspaces/reproduction_negbinom.RData")
