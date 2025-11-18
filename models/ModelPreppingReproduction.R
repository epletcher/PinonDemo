library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

## load model fitting workspace generated from 'ModelPreppingGrowth'
# contains the fit ss growth model 'PinonDemo/models/growth_years_statespace.stan'
# for Bob and Elise this is located here: 
# SEV_PJ_Demo/model_output_workspaces/growth_statespace_model.RData

## extract parameters from ss growth model
# (no year effect here)
gp <- as.matrix(growth_ss, pars = c("beta0","beta1","sigp","sigo")) %>% as.data.frame()

# ------- Load cone production and height of masting trees ---------

reprodat <- read.csv("cleaned_cone_prod_data.csv") 

# cp is cone production across trees and years [year,tree]
cp.obs<-cp <- reprodat %>% select(c(Fruit_Count,Year,Field_ID)) %>% 
  pivot_wider(names_from = Field_ID, values_from = Fruit_Count) %>% 
  add_row(Year = 2024) %>% 
  select(-Year)


cp[is.na(cp)]<-999 # for running STAN, convert NAs to 999 value

# ----- estimate past size of mast trees using growth model


# make this a matrix of [year,tree]
Sz.obs <- reprodat %>% 
  rename(height = tree_height_2024) %>% 
  select(c(Field_ID,height,Year)) %>%
  pivot_wider(names_from = Field_ID, values_from = height) %>%
  add_row(Year = 2024)

# ** editing here **
# ** i forget, do we want to include process error (or obs error) here?

for (i in 1:length(gp$beta0)) {
  
  for(t in 1:length(Sz.obs[1]))
  
  log(Sz)-gp$beta0)/gp$beta1 
  
}

Sz<-Sz.obs

Sz[is.na(St)]<-999

# ------- FIT REPRODUCTION MODEL IN STAN -----

i = length(St)

# specify model data
reprodata <- list(i=i,St=St,cp=cp)

#start <- list() # specify starting values, if needed

# fit reproduction model
reprofit1 <- stan(file='models/reproduction.stan', data=reprodata, chains=3, iter=3000, warmup=1500)

reprofit1 

launch_shinystan(reprofit1)

# -------- Extract posterior estimates ------
# put parameter estimates in a dataframe

## repro params
repro.params <- 
  as.matrix(reprofit1, pars = c("beta0","beta1")) %>% 
  as.data.frame()

# ------ generate preds -------
# vector of Size_tmins for generating preds
St.range <- seq(1,6,0.05)

# preds
cone.mean.pred.range <- matrix(NA,length(St.range),length(repro.params$beta0))

for (k in 1:length(repro.params$beta0)) {
  
  cone.mean.pred.range[,k] <- exp(repro.params$beta0[k] + repro.params$beta1[k]*St.range)
  
}

# median & CI's
# extract median and 90% credible intervals of MEAN PREDICTED SIZE
med.cone <- apply(cone.mean.pred.range, MARGIN = c(1), FUN = median)
low.cone <- apply(cone.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.05) # low
up.cone <- apply(cone.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.95) # up


# plot
plot(x = St.range, y = med.cone, type = "l", lwd = 2, lty = 1, col = "aquamarine3")
lines(x = St.range, y = low.cone, lty = 2, col = "aquamarine3")
lines(x = St.range, y = up.cone, lty = 2, col = "aquamarine3")

# ------ ggplot ---------
  