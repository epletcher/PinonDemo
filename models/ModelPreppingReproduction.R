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
cp.obs<-cp <- reprodat %>% 
  select(c(Fruit_Count,Year,Field_ID)) %>% 
  filter(Year > 1999) %>% # remove 1997 and 1998, no cones produced by focal trees
  pivot_wider(names_from = Field_ID, values_from = Fruit_Count) %>%
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

# Notes: some trees do not have height for 2024, so they will remain as NAs for heights back in time

# empty array to fill
Sz.obs.p <- replicate(length(gp$beta0), Sz.obs)

for (i in 1:length(gp$beta0)) { 
  
  for(t in dim(Sz.obs)[1]:2) { # back casting here, so descending order
    
    # trees are slowly shrinking?? check this equation
    Sz.obs.p[t-1,,i] <- exp((log(Sz.obs.p[t,,i])-gp$beta0[i])/gp$beta1[i]) # not including any error
    
  }
  
}


# ------ Visual check of back casted tree heights ---------
matplot(Sz.obs.p[,10,], type = "l") # plot for a particular tree

# ------- FIT REPRODUCTION MODEL IN STAN -----
## prep size data for STAN

# **** editing here ***
Sz<-Sz.obs.p

Sz[is.na(St)]<-999


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
  