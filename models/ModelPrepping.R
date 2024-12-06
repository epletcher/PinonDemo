library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

## LOAD DATA
## load demo data
demo.data <- read.csv("cleaned_demo_data.csv")

# remove outlier (** just removing entire tree's row for now)
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

# -------plot growth --------
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
# **2019 and 2021 have some Ht measurements that seem like they must be measurement errors (more growth than biologically possible)
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

#** filter to see which individuals are outliers in 2013 to 2018 graph
demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>%
  select(c(raw.data.TreeID,growth)) %>%
  filter(growth >= 0.5)

# survival
demo.data %>% 
  # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
  filter(Year != 2012 & Year != 2021) %>%
  ggplot(aes(x = Ht.t.min.1, y = Alive)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(vars(Year)) +
  theme_bw()

# ------- Prep data and model Growth -------
# building models where intercept and slope vary by year, 2013-2022
# no data for 2020 or 2021 b/c no data collection 2020.
# tmin is size in previous year
# size is the current year's size

# # Check if there is there a duplicate tree id year combination
# # Removed in DataCleaning
# demo.data %>%
#   dplyr::group_by(Year, raw.data.TreeID) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 

# ## Stmin is size at the previous time step
# Stmin <- demo.data %>% 
#   select(c(raw.data.TreeID, Year, Ht.t.min.1)) %>%
#   # reorganize data so columns are individuals, rows are years
#   pivot_wider(names_from = raw.data.TreeID, values_from = Ht.t.min.1) %>%
#   # reorder rows so that years are in order
#   arrange(Year) %>%
#   # filter years to only 2013-2019 and 2022 (when data is consistent for height)
#   filter(Year>2012&Year!=2021) %>%
#   select(-Year) %>%
#   as.matrix()
# 
# ## St is size at the current size step
# St <-  demo.data %>% 
#   select(c(raw.data.TreeID, Year, Ht)) %>%
#   # reorganize data so columns are individuals, rows are years
#   pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
#   # reorder rows so that years are in order
#   arrange(Year) %>%
#   # filter years to only 2013-2019 and 2022 (when data is consistent for ht)
#   filter(Year>2012&Year!=2021) %>%
#   select(-Year) %>%
#   as.matrix() 

# # check that column names and years/rows match for St and Stmin
# colnames(St)==colnames(Stmin)
# St[,1]==Stmin[,1]
# 
# # reassign NAs as 999 (Stand doesn't accept NA's)
# Stmin[is.na(Stmin)]<-999
# St[is.na(St)]<-999


## St1 is size at the first time step (year = 2013) ** best year to use for growth&data quality purposes
St1 <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
  #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
  pull(`2013`)

## St2 is size at the last time step (year = 2018) ** best year to use for growth&data quality purposes
St2 <- demo.data %>% 
  select(c(raw.data.TreeID, Year, Ht)) %>% 
  pivot_wider(names_from = Year, values_from = Ht) %>%
  mutate("growth" = `2018`-`2013`) %>% # create a growth variable for years that we are modeling growth
  #filter(growth < 0.5 & growth > -0.5) %>% # filter out individuals where growth was greater or equal to have a meter
  pull(`2018`)

# # reassign NAs as 999 (Stand doesn't accept NA's)
St1[is.na(St1)]<-999
St2[is.na(St2)]<-999

# specify model data
i = length(St1) # index by individuals 
# y = dim(St)[1] # index by year

# specify model data
# growthdata <- list(i = i, y = y, St = St, Stmin = Stmin)
growthdata <- list(i = i, St1 = St1, St2 = St2)

#start <- list() # specify starting values, if needed

# fit growth model
growthfit1 <- stan(file='models/growth.stan', data=growthdata, chains=3, iter=3000, warmup=1500)

# ------ Prep data and model Survival -------
## Stmin is size at the previous time step
Stmin <- demo.data %>% 
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
Stmin[is.na(Stmin)]<-999
St[is.na(St)]<-999

## Surv is survival at the current size step
Surv <-  demo.data %>% 
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
Surv[is.na(Surv)]<-999

i = dim(St)[2] # index by individuals 
y = dim(St)[1] # index by year
# specify model data
survdata <- list(i = i, y = y, Surv = Surv, Stmin = Stmin)

#start <- list() # specify starting values, if needed

# fit growth model
survivalfit1 <- stan(file='models/survival.stan', data=survdata, chains=3, iter=3000, warmup=1500)


# ------- Inspect model outputs --------
# growth
growthfit1 

launch_shinystan(growthfit1)

# survival
survivalfit1

launch_shinystan(survivalfit1)

# -------- Extract posterior estimates ------
# put parameter estimates in a dataframe

# ## growth params
# growth.params <- 
#   as.matrix(growthfit1, pars = c("beta0[1]","beta0[2]","beta0[3]",
#                                  "beta0[4]","beta0[5]","beta0[6]",
#                                  "beta0[7]","beta0[8]",
#                                  "beta1[1]","beta1[2]","beta1[3]",
#                                  "beta1[4]","beta1[5]","beta1[6]",
#                                  "beta1[7]","beta1[8]","sigma",
#                                  "beta0mu","beta1mu","tausq0",
#                                  "tausq1")) %>% 
#                                     as.data.frame()

## growth params
growth.params <- 
  as.matrix(growthfit1, pars = c("beta0","beta1","sigma")) %>% 
  as.data.frame()

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
