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

# Convert beta0's and beta1's from growth and survival models into matrices
g.beta0 <- growth.params %>% select(starts_with('beta0[')) %>% as.matrix()
g.beta1 <- growth.params %>% select(starts_with('beta1[')) %>% as.matrix()

s.beta0 <- survival.params %>% select(starts_with('beta0[')) %>% as.matrix()
s.beta1 <- survival.params %>% select(starts_with('beta1[')) %>% as.matrix()

# establish size range to plot model predictions:
min(St[which(St!=999)])
max(St[which(St!=999)]) # ~0-7 (height)

# vector of Size_tmins for generating preds
St.min.range <- seq(0.5,7,0.05)

# ----- Growth - year effect --------

## Generate predictions for year effect growth model
# Empty matrix for mean predictions from growth model (no process error)
mean.pred.range <- array(NA,c(length(St.min.range),y,length(growth.params$`beta0[1]`)))

for (k in 1:length(growth.params$`beta0[1]`)) {

  for(t in 1:y) {

        mean.pred.range[,t,k] <- g.beta0[k,t] + g.beta1[k,t]*log(St.min.range)

        }
}

# extract median and 90% credible intervals of MEAN PREDICTED SIZE
med.G.pred.range <- apply(mean.pred.range, MARGIN = c(1,2), FUN = median)
low.G.pred.range <- apply(mean.pred.range, MARGIN = c(1,2), FUN = quantile, 0.05) # low
up.G.pred.range <- apply(mean.pred.range, MARGIN = c(1,2), FUN = quantile, 0.95) # up

# plot predictions by (growth modeled directly)
matplot(x = St.min.range, y = exp(med.G.pred.range), type = "l", lwd = 2, lty = 1, xlab = "size_tmin1", ylab = "growth by height in meters")

# plot predictions by year (size in current year modeled)
# # logged
# matplot(x = log.St.min.range, y = med.St.pred.range, type = "l", lwd = 2, lty = 1, xlab = "log(size_tmin1)", ylab = "log(size_t)")
# abline(0,1, col = "black", lwd = 1.5, lty=2)

# # un logged values
# matplot(x = exp(log.St.min.range),y = exp(med.St.pred.range), type = "l", lwd = 2, lty = 1, xlab = "size_tmin1", ylab = "size_t")
# abline(0,1, col = "black", lwd = 1.5, lty=2)
# ----- Growth - no year effect --------
## # Generate yhats for no year effect
# St.mean.pred.range <- matrix(NA,length(log.St.min.range),length(growth.params$beta0))
# 
# for (k in 1:length(growth.params$beta0)) {
# 
#         St.mean.pred.range[,k] <- growth.params$beta0[k] + growth.params$beta1[k]*log.St.min.range
# 
# }

# # extract median and 90% credible intervals of MEAN PREDICTED SIZE
# med.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = median)
# low.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.05) # low
# up.St.pred.range <- apply(St.mean.pred.range, MARGIN = c(1), FUN = quantile, 0.95) # up
# 
# # plot
# plot(x = exp(log.St.min.range), y = exp(med.St.pred.range), type = "l", lwd = 2, lty = 1, xlab = "log(size_t1)", ylab = "log(size_t2)", col = "aquamarine3")
# lines(x = exp(log.St.min.range), y = exp(low.St.pred.range), lty = 2, col = "aquamarine3")
# lines(x = exp(log.St.min.range), y = exp(up.St.pred.range), lty = 2, col = "aquamarine3")
# 

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
  # filter years to only 2013-2018 (when data is consistent and high quality for ht)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

# convert 999 in Stmin back to NAs
Stmin.obs <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2018 (when data is consistent and high quality for ht)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

# convert 999 in Stmin back to NAs
G.obs <- St.obs/Stmin.obs

## Produce mean predictions from growth model
# # Empty matrix
# St.yhat <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))
# St.mean.pred <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))

G.yhat <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))
G.mean.pred <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))

# loop over years and iterations
for (k in 1:length(growth.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    # ** calculating for direct growth model here **
        G.yhat[t,,k] <- rnorm(length(Stmin.obs[1,]), g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]), growth.params$sigma[k]) # process error
        
        G.mean.pred[t,,k] <- g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]) # mean prediction
  }
  
}

## Calculate bayesian p value using deviance
devsim <- rep(NA,length(growth.params$`beta0[1]`))
devobs <- rep(NA,length(growth.params$`beta0[1]`))

for(k in 1:length(growth.params$`beta0[1]`)) {
  
  devsim[k] <- -2*sum(dnorm(G.yhat[,,k], G.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  devobs[k] <- -2*sum(dnorm(G.obs, G.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  
}

pval = 0

for(k in 1:length(growth.params$`beta0[1]`)) {
  
if(devsim[k]>devobs[k]) {pval=pval+1}

}

pval/length(growth.params$`beta0[1]`)

hist(devobs, col=rgb(0,0,1,1/4), xlim = c(1000,4000))  # first histogram
hist(devsim, col=rgb(1,0,0,1/4), xlim = c(1000,4000), add=T)  # second

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

# ------------ Plotting Survival (ggplot) ------------
# named vector of years
year.names <- c('2013' = 'V1', '2014' = 'V2', '2015' = 'V3', '2016' = 'V4', '2017' = 'V5', '2018' = 'V6', '2019' = 'V7', '2022' = 'V8')

## reformat survival model predicitons

# median
med.surv.dat <- med.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = exp(log.St.min.range)) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'med.surv.prob') # pivot longer columns to years

# lower credible interval
low.surv.dat <- low.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = exp(log.St.min.range)) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'low.surv.prob') # pivot longer columns to years

# upper credible interval
up.surv.dat <- up.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = exp(log.St.min.range)) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'up.surv.prob') # pivot longer columns to years

# merge
surv.plot.dat <- left_join(med.surv.dat, low.surv.dat) %>% 
  left_join(., up.surv.dat) %>% 
  mutate(years = as.factor(years))

## plot
tiff("figures/survival_plotted.tif",width = 7,height=6,units="in", res=300)

# cols
cols <- c('2013'='#c7e9b9','2014'='#ADCC3C','2015'='#7fcdbb','2016'='#41b6c4','2017'='#1d91c0','2018'='#225ea8','2019'='#253494','2022'='black')


surv.plot.dat %>% 
  ggplot(aes(x = size.tmin, y = med.surv.prob)) +
  geom_ribbon(aes(ymin = low.surv.prob, ymax = up.surv.prob, group = years, fill = years), 
              alpha=0.3) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_line(aes(group = years, col = years), lwd = 1.25) +
  labs(x = "size in previous year (height in meters)", y = "probability of survival") +
  theme(
    text = element_text(size = 22),
    legend.key = element_rect(fill = "white"),
    panel.background = element_rect(linetype = "solid",fill = NA),
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.grid.major = element_line(colour = "lightgrey", linewidth = .4)
      )
  
dev.off()

# -------- Plotting growth (ggplot2) ------------------
tiff("figures/growth_plotted.tif",width = 5.25,height=6,units="in", res=300)

# organize growth predictions
growth.plot.dat <-
  data.frame(
  'size.tmin' = exp(log.St.min.range),
  'med.pred' = exp(med.St.pred.range), # median predicted size at t
  'low.pred' = exp(low.St.pred.range), # add the lower credible interval
  'up.pred' = exp(up.St.pred.range))# add the upper credible interval

# plot
growth.plot.dat %>% 
  ggplot(aes(x = size.tmin, y = med.pred)) +
  geom_abline(lty = 2, col = 'black') +
  geom_ribbon(aes(ymin = low.pred, ymax = up.pred), fill = '#225ea8', alpha=0.3) +
  geom_line(aes(), lwd = 1.25, col = '#225ea8') +
  labs(x = "size in 2013 (height in meters)", y = "size in 2018 (height in meters)") +
  theme(
    text = element_text(size = 22),
    legend.key = element_rect(fill = "white"),
    panel.background = element_rect(linetype = "solid",fill = NA),
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.grid.major = element_line(colour = "lightgrey", linewidth = .4)
  )

dev.off()
