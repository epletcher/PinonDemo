## this code originally started on line 88 of the 'ModelPrepping Survival.R' script, and ran the growth model before we switched to a state space growth model

# ------- plot growth --------

## Check for annual growth outliers

plot_grid(
  # annual growth
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = Ht.t.min.1, y = Ht-Ht.t.min.1)) +
    labs(y= "annual growth (m)", x = "height (previous year)") +
    geom_point() + 
    xlim(0, 7) +
    ylim(-1.25,2.25) +
    geom_abline(intercept = 1, slope = 0, col = "red", lty =2, lwd = 1.2) +
    geom_abline(intercept = 0.5, slope = 0, col = "red", alpha = 0.35, lty =2, lwd = 1) +
    theme_bw(),
  
  # growth 2013 to 2018
  demo.data %>% 
    select(c(raw.data.TreeID, Year, Ht)) %>% 
    pivot_wider(names_from = Year, values_from = Ht) %>%
    mutate("growth" = `2018`-`2013`) %>%
    ggplot(aes(x = `2013`, y = growth)) + 
    labs(y= "growth 2013-2018 (m)", x = "height 2013") +
    geom_point() + 
    xlim(0, 7) +
    ylim(-1.25,2.25) +
    geom_abline(intercept = 1, slope = 0, col = "red", lty =2, lwd = 1.2) +
    theme_bw()
)

## plot time-series of growth outliers (tree either grows or shrinks more than 0.5 from one year to the next)

# list of trees with growth >0.5 or <-0.5
growth.err <- demo.data %>% 
  filter(Year != 2012 & Year != 2021) %>%
  mutate(ann_grow = Ht-Ht.t.min.1) %>%
  filter(ann_grow > 0.5 | ann_grow < -0.5) %>% 
  pull(raw.data.TreeID) %>%
  unique()

# number of trees with annual >0.5 or <-0.5
length(growth.err)

# plot time series of annual hieght for trees with growth anomalies
demo.data %>% 
  filter(Year != 2012 & Year != 2021) %>%
  mutate(ann_grow = Ht-Ht.t.min.1) %>%
  filter(raw.data.TreeID %in% growth.err) %>%
  ggplot(aes(x = Year, y = Ht)) +
  labs(y= "Ht (m)", x = "year") +
  geom_point(aes(col = raw.data.TreeID)) +
  geom_line(aes(col = raw.data.TreeID)) + 
  scale_x_continuous(breaks = seq(2013,2022,1)) +
  scale_y_continuous(breaks = seq(0,7,0.5)) +
  theme_bw() +
  theme(legend.position="none")

# ----- plot the size variable by year -----

# size tmin vs. size (growth)
demo.data %>% 
  # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
  filter(Year != 2012 & Year != 2021) %>%
  ggplot(aes(x = Ht.t.min.1, y = Ht)) + 
  geom_abline(lty = 2) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(vars(Year)) +
  theme_bw()

# size early year vs. late year
# height 2013 to 2018
# 2019 and 2021 have some Ht measurements that seem like they must be measurement errors (more growth than biologically possible)
demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>%
  #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
  ggplot(aes(x = `2013`, y = `2018`)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(lty = 2) +
  theme_bw()

# ------- Prep data and model Growth -------


# building models where intercept and slope vary by year, 2013-2018 (highest quality data)
# no data for 2020 or 2021 b/c no data collection 2020.
# Stmin is size in previous year
# St is the current year's size

# ## Stmin.obs is size at the previous time step
# # For Stmin we will convert NA's to 999 below 
# Stmin <- Stmin.obs <- demo.data %>%
#   select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
#   # reorganize data so columns are individuals, rows are years
#   pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
#   # reorder rows so that years are in order
#   arrange(Year) %>%
#   # filter years to only 2013-2018 (when data is consistent for height, and before 2019, when there were errors in the data)
#   filter(Year>2012&Year<2019) %>%
#   select(-Year) %>%
#   as.matrix()
# 
# ## St is size at the current size step
# St <- St.obs <-  demo.data %>%
#   select(c(raw.data.TreeID, Year, Ht)) %>%
#   # reorganize data so columns are individuals, rows are years
#   pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
#   # reorder rows so that years are in order
#   arrange(Year) %>%
#   # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
#   filter(Year>2012&Year<2019) %>%
#   select(-Year) %>%
#   as.matrix()
# 
# # G is the growth ratio from Stmin to St
# G <- G.obs <- St.obs/Stmin.obs
# 
# # check that column names and years/rows match for St and Stmin
# colnames(St.obs)==colnames(Stmin.obs)
# 
# # reassign NAs as 999 for versions of data that will go into the stan model (Stand doesn't accept NA's)
# Stmin[is.na(Stmin)]<-999
# St[is.na(St)]<-999
# G[is.na(G)]<-999


# ## St1 is size at the first time step (year = 2013) ** best year to use for growth&data quality purposes
# St1 <- demo.data %>%
#   select(c(raw.data.TreeID, Year, Ht)) %>%
#   pivot_wider(names_from = Year, values_from = Ht) %>%
#   mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
#   #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
#   pull(`2013`)
# 
# ## St2 is size at the last time step (year = 2018) ** best year to use for growth&data quality purposes
# St2 <- demo.data %>%
#   select(c(raw.data.TreeID, Year, Ht)) %>%
#   pivot_wider(names_from = Year, values_from = Ht) %>%
#   mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
#   #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
#   pull(`2018`)
# 
# # G is the growth ratio from Stmin to St
# G1 <- St2/St1
# 
# # # reassign NAs as 999 (Stand doesn't accept NA's)
# St1[is.na(St1)]<-999
# St2[is.na(St2)]<-999
# G1[is.na(G1)]<-999

# # specify model data
# # i = length(St1) # index by individuals
# i = dim(St)[2] # index by individuals
# y = dim(St)[1] # index by year
# 
# # specify model data
# growthdata <- list(i = i, y = y, St = St, Stmin = Stmin, G=G)
# # growthdata <- list(i = i, St1 = St1, St2 = St2, G1 = G1)
# 
# #start <- list() # specify starting values, if needed
# 
# # fit growth model
# growth_st <- stan(file='models/growth_years.stan', data=growthdata, chains=3, iter=3000, warmup=1500) # student's T, you will need to update stan script to run model with right dist.
# 
# growth_norm <- stan(file='models/growth_years.stan', data=growthdata, chains=3, iter=3000, warmup=1500) # norm, you will need to update stan script to run model with right dist.
#

# ------- Inspect model outputs --------
# growth
growth_st 
growth_norm

launch_shinystan(growthfit1)

# -------- Extract posterior estimates ------
# put parameter estimates in a dataframe

## For gorwth by year model: growth params
growth.params.st <- 
  as.matrix(growth_st, pars = c("beta0[1]","beta0[2]","beta0[3]",
                                "beta0[4]","beta0[5]","beta0[6]",
                                "beta1[1]","beta1[2]","beta1[3]",
                                "beta1[4]","beta1[5]","beta1[6]",
                                "nu", # adding 'nu' here for student's t
                                "sigma")) %>% as.data.frame()

growth.params.norm <- 
  as.matrix(growth_norm, pars = c("beta0[1]","beta0[2]","beta0[3]",
                                  "beta0[4]","beta0[5]","beta0[6]",
                                  "beta1[1]","beta1[2]","beta1[3]",
                                  "beta1[4]","beta1[5]","beta1[6]",
                                  "sigma")) %>% as.data.frame()

# ## For single time transition model: growth params
# growth.params <- 
#   as.matrix(growthfit1, pars = c("beta0","beta1","sigma")) %>% 
#   as.data.frame()


# --------------------- This code was originally from 'ModelPredictionsSurvival.R' --------------------

# ------- GENERATE PREDICITONS ----------
# y = length years
# i = length/number of individuals
# St = size the current year, dimensions [y,i]
# Stmin = size the previous year, dimensions [y,i]
# Surv = survival in the current year, dimensions [y,i]

## Convert beta0's and beta1's from growth and survival models into matrices
g.beta0 <- growth.params.st %>% select(starts_with('beta0[')) %>% as.matrix() # student's t params
g.beta1 <- growth.params.st %>% select(starts_with('beta1[')) %>% as.matrix() # student's t params

g.beta0 <- growth.params.norm %>% select(starts_with('beta0[')) %>% as.matrix() # norm params
g.beta1 <- growth.params.norm %>% select(starts_with('beta1[')) %>% as.matrix() # norm params

# ----- Growth - year effect --------

## Generate predictions for year effect growth model
# Empty matrix for mean predictions from growth model (no process error)
y = dim(St)[1]

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
matplot(x = St.min.range, y = exp(med.G.pred.range), type = "l", lwd = 2, lty = 1, xlab = "size_tmin1", ylab = "growth ratio by height in meters")

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

# ------------ Growth model posterior predictive check -------------

## Produce mean predictions from growth model
# # Empty matrix
St.yhat <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))
St.mean.pred <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))

y = dim(Stmin.obs)[1]

# G.yhat <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))
# G.mean.pred <- array(NA,c(y,length(Stmin.obs[1,]),length(growth.params$`beta0[1]`)))

# loop over years and iterations
for (k in 1:length(growth.params$`beta0[1]`)) {
  
  for(t in 1:y) {
    # ** calculating for direct growth model here **
    # G.yhat[t,,k] <- rnorm(length(Stmin.obs[1,]), g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]), growth.params$sigma[k]) # process error = normal distribution
    #  
    St.yhat[t,,k] <- rst(n = length(Stmin.obs[1,]), mu = g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]), sigma = growth.params$sigma[k], nu = growth.params$nu[k]) # process error = student's t
    
    St.mean.pred[t,,k] <- g.beta0[k,t] + g.beta1[k,t]*log(Stmin.obs[t,]) # mean prediction
  }
  
}

## Calculate bayesian p value using deviance
devsim <- rep(NA,length(growth.params$`beta0[1]`))
devobs <- rep(NA,length(growth.params$`beta0[1]`))

for(k in 1:length(growth.params$`beta0[1]`)) {
  
  # # normal dist
  # devsim[k] <- -2*sum(dnorm(G.yhat[,,k], G.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  # devobs[k] <- -2*sum(dnorm(log(G.obs), G.mean.pred[,,k], growth.params$sigma[k], log = T), na.rm = T)
  
  # student's t
  devsim[k] <- -2*sum(dst(St.yhat[,,k], St.mean.pred[,,k], growth.params$sigma[k], nu = growth.params$nu[k], log = T), na.rm = T)
  devobs[k] <- -2*sum(dst(log(St.obs), St.mean.pred[,,k], growth.params$sigma[k], nu = growth.params$nu[k], log = T), na.rm = T)
  
}

pval = 0

for(k in 1:length(growth.params$`beta0[1]`)) {
  
  if(devsim[k]>devobs[k]) {pval=pval+1}
  
}

pval/length(growth.params$`beta0[1]`)

hist(devobs, col=rgb(0,0,1,1/4), xlim = c(-2600,-1100), main = 'red = devsim, blue = devobs')  # blue
hist(devsim, col=rgb(1,0,0,1/4), xlim = c(-2600,-1100), add=T)  # red


# ------------ Growth 2 model (no year effect) PPC -------------
# 
# # convert 999 values back into NAs
# St1.obs <- demo.data %>% 
#   select(c(raw.data.TreeID, Year, Ht)) %>% 
#   pivot_wider(names_from = Year, values_from = Ht) %>%
#   mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
#   #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
#   pull(`2013`)
# 
# ## St2 is size at the last time step (year = 2018) ** best year to use for growth&data quality purposes
# St2.obs <- demo.data %>% 
#   select(c(raw.data.TreeID, Year, Ht)) %>% 
#   pivot_wider(names_from = Year, values_from = Ht) %>%
#   mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
#   #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
#   pull(`2018`)
# 
# ## Produce mean predictions from growth model
# # Empty matrix
# St2.yhat <- matrix(NA,length(St1.obs),length(growth.params$beta0))
# St2.mean.pred <- matrix(NA,length(St1.obs),length(growth.params$beta0))
# 
# # loop over years and iterations
# for (k in 1:length(growth.params$beta0)) {
#   
#     St2.yhat[,k] <- rnorm(length(St1.obs), growth.params$beta0[k] + growth.params$beta1[k]*log(St1.obs), growth.params$sigma[k]) # process error
#     
#     St2.mean.pred[,k] <- growth.params$beta0[k] + growth.params$beta1[k]*log(St1.obs) # mean prediction
#   
# }
# 
# ## Calculate bayesian p value using deviance
# devsim <- rep(NA,length(growth.params$beta0))
# devobs <- rep(NA,length(growth.params$beta0))
# 
# for(k in 1:length(growth.params$beta0)) {
#   
#   devsim[k] <- -2*sum(dnorm(St2.yhat[,k], St2.mean.pred[,k], growth.params$sigma[k], log = T), na.rm = T)
#   devobs[k] <- -2*sum(dnorm(log(St2.obs), St2.mean.pred[,k], growth.params$sigma[k], log = T), na.rm = T)
#   
# }
# 
# pval = 0
# 
# for(k in 1:length(growth.params$`beta0[1]`)) {
#   
#   if(devsim[k]>devobs[k]) {pval=pval+1}
#   
# }
# 
# pval/length(growth.params$beta0)
# 
# hist(devobs, col=rgb(0,0,1,1/4), xlim = c(-200,0), main = "", xlab = "deviance")  # first histogram
# hist(devsim, col=rgb(1,0,0,1/4), xlim = c(-200,0), add=T)  # second

# -------- Plotting growth (ggplot2) ------------------
# tiff("figures/growth_plotted.tif",width = 5.25,height=6,units="in", res=300)
# 
# # organize growth predictions
# growth.plot.dat <-
#   data.frame(
#   'size.tmin' = St.min.range,
#   'med.pred' = St.pred.range, # median predicted size at t
#   'low.pred' = St.pred.range, # add the lower credible interval
#   'up.pred' = St.pred.range)# add the upper credible interval
# 
# # plot
# growth.plot.dat %>% 
#   ggplot(aes(x = size.tmin, y = med.pred)) +
#   geom_abline(lty = 2, col = 'black') +
#   geom_ribbon(aes(ymin = low.pred, ymax = up.pred), fill = '#225ea8', alpha=0.3) +
#   geom_line(aes(), lwd = 1.25, col = '#225ea8') +
#   labs(x = "size in 2013 (height in meters)", y = "size in 2018 (height in meters)") +
#   theme(
#     text = element_text(size = 22),
#     legend.key = element_rect(fill = "white"),
#     panel.background = element_rect(linetype = "solid",fill = NA),
#     panel.border = element_rect(linetype = "solid", fill = NA),
#     panel.grid.major = element_line(colour = "lightgrey", linewidth = .4)
#   )
# 
# dev.off()


