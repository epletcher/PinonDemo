library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

# ------- Load cone production and height of masting trees ---------

reprodat <- read.csv("cleaned_cone_prod_data.csv")

# ------- FIT REPRODUCTION MODEL IN STAN -----

## prep data
# filter reproduction data to only 2020 (most recent mast year)
reprod2020 <- reprodat %>% 
  filter(Year == 2020) 
  

# cp is cone production in 2020
cp <- reprod2020 %>% 
  pull(Fruit_Count)

cp[is.na(cp)]<-999

# St is size in 2024 (the only year we have height masting trees)
St <- reprod2020 %>% 
  pull(tree_height_2024)

St[is.na(St)]<-999

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
  