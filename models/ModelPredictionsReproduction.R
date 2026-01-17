## load packages
library(tidyverse)
library(rstan)
library(shinystan)
library(LaplacesDemon)

## load workspace generated from running model in 'ModelPreppingReproduction.R' script

## extract reproduction model params from fit model
repro.params <- 
  as.matrix(reprofit2) %>% as.data.frame()

## Convert alpha's and beta's from reproduction models into matrices
alpha1 <- repro.params %>% select(starts_with('alpha1[')) %>% as.matrix()
alpha2 <- repro.params %>% select(starts_with('alpha2[')) %>% as.matrix()
alpha3 <- repro.params %>% select(starts_with('alpha3[')) %>% as.matrix()

beta1 <- repro.params %>% select(starts_with('beta1[')) %>% as.matrix()
beta2 <- repro.params %>% select(starts_with('beta2[')) %>% as.matrix()
beta3 <- repro.params %>% select(starts_with('beta3[')) %>% as.matrix()

beta1_2 <- repro.params %>% select(starts_with('beta1_2[')) %>% as.matrix()
beta2_2 <- repro.params %>% select(starts_with('beta2_2[')) %>% as.matrix()
beta3_2 <- repro.params %>% select(starts_with('beta3_2[')) %>% as.matrix()


# ------ generate preds -------
# vector of Size_tmins for generating preds
Sz.range <- seq(1,8,0.5)

# preds
lg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha1[,1])))
mg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha2[,1])))
hg.cone.pred <- array(NA,c(length(Sz.range),y,length(alpha3[,1])))

for (k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
  
  lg.cone.pred[,t,k] <- exp(alpha1[k,t] + beta1[k,t]*Sz.range + beta1_2[k,t]*Sz.range^2)
  mg.cone.pred[,t,k] <- exp(alpha2[k,t] + beta2[k,t]*Sz.range + beta2_2[k,t]*Sz.range^2)
  hg.cone.pred[,t,k] <- exp(alpha3[k,t] + beta3[k,t]*Sz.range + beta3_2[k,t]*Sz.range^2)
  
  }
  
}

# all trees, mean posterior estimate
matplot(apply(lg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")
matplot(apply(mg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")
matplot(apply(hg.cone.pred, MARGIN = c(1,2), FUN = mean), type = "l")

# ----------- Posterior predictive check -------------
# **** editing here
# generate predicted cone production using observed sizes
cp.yhat.lg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.lg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.yhat.mg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.mg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.yhat.hg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))
cp.lambda.hg <- array(NA,c(length(cp.obs[1,]),y,length(alpha1[,1])))

for (k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
    
    cp.yhat.lg[,t,k] <- rpois(length(cp.obs[1,]), exp(alpha1[k,t] + beta1[k,t]*Sz[t,,1] + beta1_2[k,t]*Sz[t,,1]^2))
    cp.lambda.lg[,t,k] <- exp(alpha1[k,t] + beta1[k,t]*Sz[t,,1] + beta1_2[k,t]*Sz[t,,1]^2)
    
    cp.yhat.mg[,t,k] <- rpois(length(cp.obs[1,]), exp(alpha2[k,t] + beta2[k,t]*Sz[t,,2] + beta2_2[k,t]*Sz[t,,2]^2))
    cp.lambda.mg[,t,k] <- exp(alpha2[k,t] + beta2[k,t]*Sz[t,,2] + beta2_2[k,t]*Sz[t,,2]^2)
    
    cp.yhat.hg[,t,k] <- rpois(length(cp.obs[1,]), exp(alpha3[k,t] + beta3[k,t]*Sz[t,,3] + beta3_2[k,t]*Sz[t,,3]^2))
    cp.lambda.hg[,t,k] <- exp(alpha3[k,t] + beta3[k,t]*Sz[t,,3] + beta3_2[k,t]*Sz[t,,3]^2)
    
  }
  
}

## Calculate bayesian p value using deviance

# low growth
devsim.repro.lg <- rep(NA,length(alpha1[,1]))
devobs.repro.lg <- rep(NA,length(alpha1[,1]))
# avg growth
devsim.repro.mg <- rep(NA,length(alpha1[,1]))
devobs.repro.mg <- rep(NA,length(alpha1[,1]))
# high growth
devsim.repro.hg <- rep(NA,length(alpha1[,1]))
devobs.repro.hg <- rep(NA,length(alpha1[,1]))


for(k in 1:length(alpha1[,1])) {
  
  for(t in 1:y) {
  
  # low growth
  devsim.repro.lg[k] <- -2*sum(dpois(cp.yhat.lg[,t,k], cp.lambda.lg, log = T), na.rm = T)
  devobs.repro.lg[k] <- -2*sum(dpois(cp.obs, cp.lambda.lg, log = T), na.rm = T) 
  
  # avg growth
  devsim.repro.mg[k] <- -2*sum(dpois(cp.yhat.lg[,t,k], cp.lambda.mg, log = T), na.rm = T)
  devobs.repro.mg[k] <- -2*sum(dpois(cp.obs, cp.lambda.mg, log = T), na.rm = T) 
  
  # high growth
  devsim.repro.hg[k] <- -2*sum(dpois(cp.yhat.lg[,t,k], cp.lambda.hg, log = T), na.rm = T)
  devobs.repro.hg[k] <- -2*sum(dpois(cp.obs, cp.lambda.hg, log = T), na.rm = T) 
  
  }
  
}

pval.lg = 0
pval.mg = 0
pval.hg = 0

for(k in 1:length(alpha1[,1])) {
  
  if(devsim.repro.lg[k]>devobs.repro.lg[k]) {pval.lg=pval.lg+1}
  if(devsim.repro.mg[k]>devobs.repro.mg[k]) {pval.mg=pval.mg+1}
  if(devsim.repro.hg[k]>devobs.repro.hg[k]) {pval.hg=pval.hg+1}
  
}
# between 0.1 and 0.9 indicates our model does a good job of capturing the distribution of the observed data in our model
pval.lg/length(alpha1[,1])
pval.mg/length(alpha1[,1])
pval.hg/length(alpha1[,1])

hist(devobs.repro.lg, col=rgb(0,0,1,1/4))  
hist(devsim.repro.lg, col=rgb(1,0,0,1/4), add = T)  

hist(devobs.repro.mg, col=rgb(0,0,1,1/4))
hist(devsim.repro.lg, col=rgb(1,0,0,1/4), add = T)

hist(devobs.repro.hg, col=rgb(0,0,1,1/4))
hist(devsim.repro.lg, col=rgb(1,0,0,1/4), add = T)

####### ppc code from martin:

# Function to run PPC of the Deviance for the Poisson regression model
PPC_deviance_poisson_model <- function(n_obs, param_chains,
                                       predictor_matrix,
                                       pred_array, obs_data) {
  
  n_iter = nrow(param_chains)
  
  # Step 1: Create empty vectors to hold data
  
  # Deviance of the predicted data for each parameter iteration
  deviance_output_pred_data <- rep(NA, n_iter)
  
  # Deviance of the observed data for each parameter iteration
  deviance_output_obs_data <- rep(NA, n_iter)
  
  # Mean predicted mean of each observation
  mu = numeric(n_obs)
  
  # Log likelihood of the predicted data for each parameter iteration
  log_like_pred_data <- rep(NA, n_iter)
  
  # Log likelihood of the observed data for each parameter iteration
  log_like_obs_data <- rep(NA, n_iter)
  
  # Counts of how many times the deviance of the observed data is lower than the
  # deviance of the predicted data.
  p_value <- 0
  
  # Step 2: Calculate Deviance for the predicted and observed data for each
  # parameter iteration. Then count how many times the deviance of the observed
  # data is lower than the predicted data.
  for (i in 1:n_iter) {
    # Calculate predictive values from each parameter chain iteration
    for (j in 1:n_obs) {
      mu[j] <- exp(sum(predictor_matrix[j,] * param_chains[i,])) 
    }
    log_like_pred_data[i] <- sum(dpois(pred_array[,,i], lambda = mu, log = T))
    deviance_output_pred_data[i] <- (-2 * log_like_pred_data[i])
    
    
    log_like_obs_data[i] <- sum(dpois(obs_data, lambda = mu, log = T))
    deviance_output_obs_data[i] <- (-2 * log_like_obs_data[i])
    
    if(deviance_output_obs_data[i] < deviance_output_pred_data[i]) {
      p_value <- p_value + 1
    }
  }
  
  # Step 3: Prep data for output
  
  # Deviance and log likelihood values for each parameter iteration
  deviance_df <- data.frame(log_like_pred_data, deviance_output_pred_data,
                            log_like_obs_data, deviance_output_obs_data)
  
  # Total number of times observed deviance is lower than the predicted deviance
  # divided by the total iterations
  p_value = p_value/n_iter
  
  # Save data as a list
  output_list <- list(deviance_df = deviance_df,
                      p_value = p_value)
  
  return(output_list)
}


