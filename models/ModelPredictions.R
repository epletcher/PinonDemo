## set working directory to 'SEV_PJ_Demo'

# load model fitting workspace from 'modelPrepping'

## load packages
library(tidyverse)

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
max(St[which(St!=999)]) # ~0-25

# vector of Size_tmins for generating preds
log.St.min.range <- log(seq(0.1,25,0.1))

# ----- Growth --------
# Empty matrix for prediction from growth model
St.pred <- array(NA,c(length(log.St.min.range),y,length(growth.params$`beta0[1]`)))

for (k in 1:length(growth.params$`beta0[1]`)) {
  
  for(t in 1:y) {
        
        St.pred[,t,k] <- rnorm(length(log.St.min.range), (g.beta0[k,t] + g.beta1[k,t]*log.St.min.range), growth.params$sigma[k])
        
        }
}

# extract median and 90% credible intervals
med.St.pred <- apply(St.pred, MARGIN = c(1,2), FUN = median)
low.St.pred <- apply(St.pred, MARGIN = c(1,2), FUN = quantile, 0.05) # low
up.St.pred <- apply(St.pred, MARGIN = c(1,2), FUN = quantile, 0.95) # up

# plot predictions by year
# logged
matplot(x = log.St.min.range, y = med.St.pred, type = "l", lwd = 2, lty = 1, xlab = "log(size_tmin1)", ylab = "log(size_t)")

# un logged values
matplot(x = exp(log.St.min.range),y = exp(med.St.pred), type = "l", lwd = 2, lty = 1, xlab = "size_tmin1", ylab = "size_t")

# ------ Survival --------

# generate probability of survival (using mean parameter estimates here)
Surv.prob <- matrix(NA,length(log.St.min.range),y)

  for(t in 1:y) {
    # ** just taking the mean parameter estimates here for now
    Surv.prob[,t] <- invlogit(colMeans(s.beta0)[t] + colMeans(s.beta1)[t]*log.St.min.range)
    
  }

## plot survival probability for each year, by log(size_tmin)

# logged
matplot(x = log.St.min.range, y = Surv.prob, type = "l", lty = 1, lwd = 2, xlab = "log(size_tmin)", ylab = "p(surv)")

# unlogged
matplot(x = exp(log.St.min.range), y = Surv.prob, type = "l", lty = 1, lwd = 2, xlab = "size_tmin", ylab = "p(surv)")
