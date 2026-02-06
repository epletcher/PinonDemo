library(tidyverse)
library(rstan)
library(shinystan)
library(cowplot)

## set workspace to 'PinonDemo' folder/repository

## LOAD DATA
## load demo data
demo.data <- read.csv("cleaned_demo_data.csv")

## EXTRA DATA CLEAN UP

# remove tree that is not in the plot (size only measured for one year)
demo.data <- demo.data[-which(demo.data$raw.data.TreeID=="PJControl . Left . 72 . 35"),]

# ------ Add a few more columns ---------

demo.data <- demo.data %>% 
  
  # pull sampling stucture info back out of unique treeID
  separate(col = raw.data.TreeID, into = c("Site", "Transect", "Tree_Tag_Number", "Plot_distance"), sep = " . ", remove = FALSE) %>%
  
  # unique plotID 'plot_id' (transect + plot_distance)
  unite(col = "plot_id", c(Transect, Plot_distance), sep = ".", remove = FALSE) 
 

# ------ Prep data and model Survival -------
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
# survival
survivalfit1

launch_shinystan(survivalfit1)

# -------- Extract posterior estimates ------
# put parameter estimates in a dataframe

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
