## set working directory to 'PinonDemo'

# load model fitting workspace generated from 'modelPrepping'

## load packages
library(tidyverse)
library(rstan)
library(shinystan)

## load functions
# inverse logit function
invlogit <- function(x) {
  
  exp(x)/(1 + exp(x))
  
}

# ------- GENERATE PREDICITONS ----------
# y = length years
# i = length/number of individuals
# St = size the current year, dimensions [y,i]
# Stmin = size the previous year, dimensions [y,i]
# Surv = survival in the current year, dimensions [y,i]

# # Convert beta0's and beta1's from growth and survival models into matrices
# g.beta0 <- growth.params %>% select(starts_with('beta0[')) %>% as.matrix()
# g.beta1 <- growth.params %>% select(starts_with('beta1[')) %>% as.matrix()

s.beta0 <- survival.params %>% select(starts_with('beta0[')) %>% as.matrix()
s.beta1 <- survival.params %>% select(starts_with('beta1[')) %>% as.matrix()

# establish size range to plot model predictions:
min(St[which(St!=999)])
max(St[which(St!=999)]) # ~0-7 (height)

# vector of Size_tmins for generating preds
log.St.min.range <- log(seq(0.05,7,0.05))

# ----- Growth --------
# # Empty matrix for mean predictions from growth model (no process error)
# St.mean.pred.range <- array(NA,c(length(log.St.min.range),y,length(growth.params$`beta0[1]`)))
# 
# for (k in 1:length(growth.params$`beta0[1]`)) {
#   
#   for(t in 1:y) {
#         
#         St.mean.pred.range[,t,k] <- g.beta0[k,t] + g.beta1[k,t]*log.St.min.range
#         
#         }
# }

St.mean.pred.range <- matrix(NA,length(log.St.min.range),length(growth.params$beta0))

for (k in 1:length(growth.params$beta0)) {

        St.mean.pred.range[,k] <- growth.params$beta0[k] + growth.params$beta1[k]*log.St.min.range

}

# # extract median and 90% credible intervals of MEAN PREDICTED SIZE
# med.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1,2), FUN = median)
# low.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1,2), FUN = quantile, 0.05) # low
# up.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1,2), FUN = quantile, 0.95) # up
# 
# # plot predictions by year
# # logged
# matplot(x = log.St.min.range, y = med.St.pred.range, type = "l", lwd = 2, lty = 1, xlab = "log(size_tmin1)", ylab = "log(size_t)")
# abline(0,1, col = "black", lwd = 1.5, lty=2)
# 
# # un logged values
# matplot(x = exp(log.St.min.range),y = exp(med.St.pred.range), type = "l", lwd = 2, lty = 1, xlab = "size_tmin1", ylab = "size_t")
# abline(0,1, col = "black", lwd = 1.5, lty=2)

# extract median and 90% credible intervals of MEAN PREDICTED SIZE
med.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = median)
low.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.05) # low
up.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.95) # up

# plot predictions by year
# logged
plot(x = log.St.min.range, y = med.St.pred.range, type = "l", lwd = 2, lty = 1, xlab = "log(size_t1)", ylab = "log(size_t2)", col = "aquamarine3")
lines(x = log.St.min.range, y = low.St.pred.range, lty = 2, col = "aquamarine3")
lines(x = log.St.min.range, y = up.St.pred.range, lty = 2, col = "aquamarine3")
abline(0,1, col = "black", lwd = 1.5, lty=3)

# un logged values
plot(x = exp(log.St.min.range), y = exp(med.St.pred.range), type = "l", lwd = 2, lty = 1, xlab = "size_t1", ylab = "size_t2", col = "aquamarine3")
lines(x = exp(log.St.min.range), y = exp(low.St.pred.range), lty = 2, col = "aquamarine3")
lines(x = exp(log.St.min.range), y = exp(up.St.pred.range), lty = 2, col = "aquamarine3")
abline(0,1, col = "black", lwd = 1.5, lty=3)

# ------ Survival --------

# generate probability of survival (across all years and iterations)
Surv.prob.range <- array(NA,c(length(log.St.min.range),y,length(survival.params$`beta0[1]`)))

for (k in 1:length(survival.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
    Surv.prob.range[,t,k] <- invlogit(s.beta0[k,t] + s.beta1[k,t]*log.St.min.range)
    
  }
  
}

## plot survival probability for each year, by log(size_tmin)
# extract median and 90% credible intervals of MEAN PREDICTED SIZE
med.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = median)
low.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = quantile, 0.05) # low
up.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = quantile, 0.95) # up


# logged
matplot(x = log.St.min.range, y = med.Surv.prob.range, type = "l", lty = 1, lwd = 2, xlab = "log(size_tmin)", ylab = "p(surv)")

# unlogged
matplot(x = exp(log.St.min.range), y = med.Surv.prob.range, type = "l", lty = 1, lwd = 2, xlab = "size_tmin", ylab = "p(surv)")
matplot(x = exp(log.St.min.range), y = up.Surv.prob.range, type = "l", lty = 2, lwd = 1, add = T)
matplot(x = exp(log.St.min.range), y = low.Surv.prob.range, type = "l", lty = 2, lwd = 1, add = T)

# ------------ Growth model posterior predictive check -------------

## prep data

# convert 999 in St back NAs
St.obs <-  demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
  filter(Year>2012&Year!=2021) %>%
  select(-Year) %>%
  as.matrix() 

# convert 999 in Stmin back to NAs
Stmin.obs <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
  filter(Year>2012&Year!=2021) %>%
  select(-Year) %>%
  as.matrix()

## Produce mean predictions from growth model
# Empty matrix
St.yhat <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))
St.mean.pred <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))

# loop over years and iterations
for (k in 1:length(growth.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
        St.yhat[t,,k] <- rnorm(length(Stmin.obs[1,]), g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]), growth.params$sigma[k]) # process error
        
        St.mean.pred[t,,k] <- g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]) # mean prediction
  }
  
}

## Calculate bayesian p value using deviance
devsim <- rep(NA,length(growth.params$`beta0[1]`))
devobs <- rep(NA,length(growth.params$`beta0[1]`))

for(k in 1:length(growth.params$`beta0[1]`)) {
  
  devsim[k] <- -2*sum(dnorm(St.yhat[,,k], St.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  devobs[k] <- -2*sum(dnorm(log(St.obs), St.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  
}

pval = 0

for(k in 1:length(growth.params$`beta0[1]`)) {
  
if(devsim[k]>devobs[k]) {pval=pval+1}

}

pval/length(growth.params$`beta0[1]`)

hist(devobs, col=rgb(0,0,1,1/4), xlim=c(-2200,-1500))  # first histogram
hist(devsim, col=rgb(1,0,0,1/4), xlim=c(-2200,-1500), add=T)  # second

# ------------ Growth 2 model (no year effect) PPC -------------

# convert 999 values back into NAs
St1.obs <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
  #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
  pull(`2013`)

## St2 is size at the last time step (year = 2018) ** best year to use for growth&data quality purposes
St2.obs <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
  #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
  pull(`2018`)

## Produce mean predictions from growth model
# Empty matrix
St2.yhat <- matrix(NA,length(St1.obs),length(growth.params$beta0))
St2.mean.pred <- matrix(NA,length(St1.obs),length(growth.params$beta0))

# loop over years and iterations
for (k in 1:length(growth.params$beta0)) {
  
    St2.yhat[,k] <- rnorm(length(St1.obs), growth.params$beta0[k] + growth.params$beta1[k]*log(St1.obs), growth.params$sigma[k]) # process error
    
    St2.mean.pred[,k] <- growth.params$beta0[k] + growth.params$beta1[k]*log(St1.obs) # mean prediction
  
}

## Calculate bayesian p value using deviance
devsim <- rep(NA,length(growth.params$beta0))
devobs <- rep(NA,length(growth.params$beta0))

for(k in 1:length(growth.params$beta0)) {
  
  devsim[k] <- -2*sum(dnorm(St2.yhat[,k], St2.mean.pred[,k], growth.params$sigma[k], log = T), na.rm = T)
  devobs[k] <- -2*sum(dnorm(log(St2.obs), St2.mean.pred[,k], growth.params$sigma[k], log = T), na.rm = T)
  
}

pval = 0

for(k in 1:length(growth.params$`beta0[1]`)) {
  
  if(devsim[k]>devobs[k]) {pval=pval+1}
  
}

pval/length(growth.params$beta0)

hist(devobs, col=rgb(0,0,1,1/4), xlim = c(-200,0), main = "", xlab = "deviance")  # first histogram
hist(devsim, col=rgb(1,0,0,1/4), xlim = c(-200,0), add=T)  # second

# ------------ Survival posterior predictive check -------------

## convert 999 vals back to NAs
Surv.obs <-  demo.data %>% 
  select(c(raw.data.TreeID, Year, Alive)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Alive) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
  filter(Year>2012&Year!=2021) %>%
  select(-Year) %>%
  as.matrix()

surv.yhat <- array(NA,c(length(Stmin.obs[1,]),y,length(survival.params$`beta0[1]`)))
surv.prob <- array(NA,c(length(Stmin.obs[1,]),y,length(survival.params$`beta0[1]`)))

for (k in 1:length(survival.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
    surv.yhat[,t,k] <- rbinom(length(Stmin.obs[1,]), size = 1, invlogit(s.beta0[k,t] + s.beta1[k,t]*log(Stmin.obs[t,])))
    
    surv.prob[,t,k] <- invlogit(s.beta0[k,t] + s.beta1[k,t]*log(Stmin.obs[t,]))
    
  }
  
}

## Calculate bayesian p value using deviance
devsim.surv <- rep(NA,length(survival.params$`beta0[1]`))
devobs.surv <- rep(NA,length(survival.params$`beta0[1]`))


for(k in 1:length(survival.params$`beta0[1]`)) {
  
  devsim.surv[k] <- -2*sum(dbinom(surv.yhat[,,k], size = 1, surv.prob[,,k], log = T), na.rm = T)
  devobs.surv[k] <- -2*sum(dbinom(Surv.obs, size = 1, surv.prob[,,k], log = T), na.rm = T)
  
}

pval = 0

for(k in 1:length(survival.params$`beta0[1]`)) {
  
  if(devsim.surv[k]>devobs.surv[k]) {pval=pval+1}
  
}
# between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
pval/length(growth.params$`beta0[1]`)

hist(devobs.surv, col=rgb(0,0,1,1/4), xlim=c(400,1000))  
hist(devsim.surv, col=rgb(1,0,0,1/4), xlim=c(400,1000), add=T)  
