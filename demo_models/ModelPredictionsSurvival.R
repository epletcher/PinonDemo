## set working directory to 'PinonDemo'

# load model fitting workspace generated from 'ModelPreppingSurvival'

## load packages
library(tidyverse)
library(rstan)
library(shinystan)
library(LaplacesDemon)

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

## Convert beta0's and beta1's from survival model into matrices
s.beta0 <- survival.params %>% select(starts_with('beta0[')) %>% as.matrix()
s.beta1 <- survival.params %>% select(starts_with('beta1[')) %>% as.matrix()

# establish size range to plot model predictions:
min(St[which(St!=999)])
max(St[which(St!=999)]) # ~0-7 (height)

# vector of Size_tmins for generating preds
RSz <- exp(seq(-1.39,2,by=.01))
NSz<-length(RSz)
St.min.range <- (RSz[-NSz]+RSz[-1])/2 # same as medpoint of classes used fr the ipm

# ------ Survival --------

# generate probability of survival (across all years and iterations)
y = dim(Surv)[1]

Surv.prob.range <- array(NA,c(length(St.min.range),y,length(survival.params$`beta0[1]`)))

for (k in 1:length(survival.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
    Surv.prob.range[,t,k] <- invlogit(s.beta0[k,t] + s.beta1[k,t]*log(St.min.range))
    
  }
  
}

## plot survival probability for each year, by log(size_tmin)
# extract median and 90% credible intervals of MEAN PREDICTED SIZE
med.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = median)
low.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = quantile, 0.05) # low
up.Surv.prob.range <- apply(Surv.prob.range, MARGIN = c(1,2), FUN = quantile, 0.95) # up


# logged
matplot(x = log(St.min.range), y = med.Surv.prob.range, type = "l", lty = 1, lwd = 2, xlab = "log(size_tmin)", ylab = "p(surv)")

# unlogged
matplot(x = St.min.range, y = med.Surv.prob.range, type = "l", lty = 1, lwd = 2, xlab = "size_tmin", ylab = "p(surv)")
matplot(x = St.min.range, y = up.Surv.prob.range, type = "l", lty = 2, lwd = 1, add = T)
matplot(x = St.min.range, y = low.Surv.prob.range, type = "l", lty = 2, lwd = 1, add = T)


# ------------ Survival posterior predictive check -------------

y = dim(Surv.obs)[1]

surv.yhat <- array(NA,c(length(Stmin2.obs[1,]),y,length(survival.params$`beta0[1]`)))
surv.prob <- array(NA,c(length(Stmin2.obs[1,]),y,length(survival.params$`beta0[1]`)))

for (k in 1:length(survival.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    
    surv.yhat[,t,k] <- rbinom(length(Stmin2.obs[1,]), size = 1, invlogit(s.beta0[k,t] + s.beta1[k,t]*log(Stmin2.obs[t,])))
    
    surv.prob[,t,k] <- invlogit(s.beta0[k,t] + s.beta1[k,t]*log(Stmin2.obs[t,]))
    
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
pval/length(survival.params$`beta0[1]`)

hist(devobs.surv, col=rgb(0,0,1,1/4), xlim=c(400,1000))  
hist(devsim.surv, col=rgb(1,0,0,1/4), xlim=c(400,1000), add=T)  

# ------------ Plotting Survival (publication quality) ------------
# named vector of years
year.names <- c('2013' = 'V1', '2014' = 'V2', '2015' = 'V3', '2016' = 'V4', '2017' = 'V5', '2018' = 'V6', '2019' = 'V7', '2022' = 'V8')

## reformat survival model predicitons

# median
med.surv.dat <- med.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = St.min.range) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'med.surv.prob') # pivot longer columns to years

# lower credible interval
low.surv.dat <- low.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = St.min.range) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'low.surv.prob') # pivot longer columns to years

# upper credible interval
up.surv.dat <- up.Surv.prob.range %>%
  as.data.frame() %>%
  rename(all_of(year.names)) %>% # rename columns to years
  add_column('size.tmin' = St.min.range) %>% # add the size.tmin column
  pivot_longer('2013':'2022', names_to = 'years', values_to = 'up.surv.prob') # pivot longer columns to years

# merge
surv.plot.dat <- left_join(med.surv.dat, low.surv.dat) %>% 
  left_join(., up.surv.dat) %>%
  mutate(years = case_when(
    years == "2013" ~ "2012-13",
    years == "2014" ~ "2013-14",
    years == "2015" ~ "2014-15",
    years == "2016" ~ "2015-16",
    years == "2017" ~ "2016-17",
    years == "2018" ~ "2017-18",
    years == "2019" ~ "2018-19",
    years == "2022" ~ "2021-22")) %>%
  mutate(years = as.factor(years))

## plot
svg("demo_models/figures/survival_plotted.svg",width = 7.5,height=6)

# cols
cols <- c('2012-13'='#c7e9b9','2013-14'='#ADCC3C','2014-15'='#7fcdbb','2015-16'='#41b6c4','2016-17'='#1d91c0','2017-18'='#225ea8','2018-19'='#253494','2021-22'='black')


surv.plot.dat %>% 
  ggplot(aes(x = size.tmin, y = med.surv.prob)) +
  geom_ribbon(aes(ymin = low.surv.prob, ymax = up.surv.prob, group = years, fill = years), 
              alpha=0.2) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_line(aes(group = years, col = years), lwd = 1.25) +
  labs(x = "size (m)", y = "probability of survival") +
  theme(
    text = element_text(size = 22),
    legend.key = element_rect(fill = "white"),
    panel.background = element_rect(linetype = "solid",fill = NA),
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.grid.major = element_line(colour = "#F2F0EF", linewidth = .4)
      )
  
 dev.off()

