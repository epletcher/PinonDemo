library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

## LOAD DATA
## load demo data
demo.data <- read.csv("cleaned_demo_data.csv")

# remove outlier (just removing entire tree's row for now)
demo.data <- demo.data[-which(demo.data$CanDiam2==22.40),]

# ------ Add a few more columns ---------

demo.data <- demo.data %>% 
  
  # pull sampling stucture info back out of unique treeID
  separate(col = raw.data.TreeID, into = c("Site", "Transect", "Tree_Tag_Number", "Plot_distance"), sep = " . ", remove = FALSE) %>%
  
  # unique plotID 'plot_id' (transect + plot_distance)
  unite(col = "plot_id", c(Transect, Plot_distance), sep = ".", remove = FALSE) 
  # 
  # # add columns for size.sq
  # mutate("CanDiam1.tmin.1.sq" = CanDiam1.tmin.1^2) %>%
  # mutate("CanDiam2.tmin.1.sq" = CanDiam1.tmin.1^2) %>%
  # mutate("Ht.t.min.1.sq" = Ht.t.min.1^2) %>%
  # mutate("DBH.tmin.1.sq" = DBH.tmin.1^2)

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

## Stmin.obs is size at the previous time step
# For Stmin we will convert NA's to 999 below 
Stmin <- Stmin.obs <- demo.data %>%
  select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2018 (when data is consistent for height, and before 2019, when there were errors in the data)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

## St is size at the current size step
St <- St.obs <-  demo.data %>%
  select(c(raw.data.TreeID, Year, Ht)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

# G is the growth ratio from Stmin to St
G <- G.obs <- St.obs/Stmin.obs

# check that column names and years/rows match for St and Stmin
colnames(St.obs)==colnames(Stmin.obs)

# reassign NAs as 999 for versions of data that will go into the stan model (Stand doesn't accept NA's)
Stmin[is.na(Stmin)]<-999
St[is.na(St)]<-999
G[is.na(G)]<-999


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

# specify model data
# i = length(St1) # index by individuals
i = dim(St)[2] # index by individuals
y = dim(St)[1] # index by year

# specify model data
growthdata <- list(i = i, y = y, St = St, Stmin = Stmin, G=G)
# growthdata <- list(i = i, St1 = St1, St2 = St2, G1 = G1)

#start <- list() # specify starting values, if needed

# fit growth model
growth_st <- stan(file='models/growth_years.stan', data=growthdata, chains=3, iter=3000, warmup=1500) # student's T, you will need to update stan script to run model with right dist.

growth_norm <- stan(file='models/growth_years.stan', data=growthdata, chains=3, iter=3000, warmup=1500) # norm, you will need to update stan script to run model with right dist.
#
# ------ Prep data and model Survival -------
## Stmin is size at the previous time step
Stmin2.obs <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2019 and 2022 (when data is consistent for height)
  filter(Year>2012&Year!=2021) %>%
  select(-Year) %>%
  as.matrix()

# reassign NAs as 999 (Stand doesn't accept NA's)
Stmin2 <- Stmin2.obs
Stmin2[is.na(Stmin2)]<-999

## Surv is survival at the current size step
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

# reassign NAs as 999 (Stand doesn't accept NA's)
Surv <- Surv.obs
Surv[is.na(Surv)]<-999

i = dim(Stmin2)[2] # index by individuals 
y = dim(Stmin2)[1] # index by year
# specify model data
survdata <- list(i = i, y = y, Surv = Surv, Stmin2 = Stmin2)

#start <- list() # specify starting values, if needed

# fit growth model
survivalfit1 <- stan(file='models/survival.stan', data=survdata, chains=3, iter=3000, warmup=1500)


# ------- Inspect model outputs --------
# growth
growth_st 
growth_norm

launch_shinystan(growthfit1)

# survival
survivalfit1

launch_shinystan(survivalfit1)

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

## survival paramas
survival.params <- 
  as.matrix(survivalfit1, pars = c("beta0[1]","beta0[2]","beta0[3]",
                                   "beta0[4]","beta0[5]","beta0[6]",
                                   "beta0[7]","beta0[8]",
                                   "beta1[1]","beta1[2]","beta1[3]",
                                   "beta1[4]","beta1[5]","beta1[6]",
                                   "beta1[7]","beta1[8]",
                                   "beta0mu","beta1mu","tausq0",
                                   "tausq1")) %>% 
                                      as.data.frame()




## save model worksapce
#save.image(file = "your-file-path-here/growth_surv_model_outputs.RData")
