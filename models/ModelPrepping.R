library(tidyverse)
library(rstan)
library(shinystan)

## set workspace to SEV_PJ_Demo

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
  unite(col = "plot_id", c(Transect, Plot_distance), sep = ".", remove = FALSE) %>%
  
  # add columns for size.sq
  mutate("CanDiam1.tmin.1.sq" = CanDiam1.tmin.1^2) %>%
  mutate("CanDiam2.tmin.1.sq" = CanDiam1.tmin.1^2) %>%
  mutate("Ht.t.min.1.sq" = Ht.t.min.1^2) %>%
  mutate("DBH.tmin.1.sq" = DBH.tmin.1^2)

# ------- Prep data and model Growth -------

# # Check if there is there a duplicate tree id year combination
# # Removed in DataCleaning
# demo.data %>%
#   dplyr::group_by(Year, raw.data.TreeID) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 

## Stmin is size at the previous time step
Stmin <- demo.data %>% 
  select(c(raw.data.TreeID, Year, DBH.tmin.1)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = DBH.tmin.1) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2018 (when data is consistent for dbh)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

## St is size at the current size step
St <-  demo.data %>% 
  select(c(raw.data.TreeID, Year, DBH)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = DBH) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2018 (when data is consistent for dbh)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix() 

# check that column names and years/rows match for St and Stmin
colnames(St)==colnames(Stmin)
St[,1]==Stmin[,1]

# reassign NAs as 999 (Stand doesn't accept NA's)
Stmin[is.na(Stmin)]<-999
St[is.na(St)]<-999

# specify model data
i = dim(St)[2] # index by individuals 
y = dim(St)[1] # index by year

# specify model data
growthdata <- list(i = i, y = y, St = St, Stmin = Stmin)

#start <- list() # specify starting values, if needed

# fit growth model
growthfit1 <- stan(file='models/growth.stan', data=growthdata, chains=3, iter=3000, warmup=1500)

# ------ Prep data and model Survival -------

## Surv is size at the current size step
Surv <-  demo.data %>% 
  select(c(raw.data.TreeID, Year, Alive)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Alive) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  # filter years to only 2013-2018 (when data is consistent for dbh)
  filter(Year>2012&Year<2019) %>%
  select(-Year) %>%
  as.matrix()

# reassign NAs as 999 (Stand doesn't accept NA's)
Surv[is.na(Surv)]<-999

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

## growth params
growth.params <- 
  as.matrix(growthfit1, pars = c("beta0[1]","beta0[2]","beta0[3]",
                                 "beta0[4]","beta0[5]","beta0[6]",
                                 "beta1[1]","beta1[2]","beta1[3]",
                                 "beta1[4]","beta1[5]","beta1[6]",
                                 "sigma","beta0mu","beta1mu","tausq0",
                                 "tausq1")) %>% 
                                    as.data.frame()


## survival paramas
survival.params <- 
  as.matrix(survivalfit1, pars = c("beta0[1]","beta0[2]","beta0[3]",
                                   "beta0[4]","beta0[5]","beta0[6]",
                                   "beta1[1]","beta1[2]","beta1[3]",
                                   "beta1[4]","beta1[5]","beta1[6]",
                                   "beta0mu","beta1mu","tausq0",
                                   "tausq1")) %>% 
                                      as.data.frame()




## save model worksapce
save.image(file = "models/growth_surv_model_outputs.RData")
