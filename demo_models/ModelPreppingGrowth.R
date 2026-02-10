library(tidyverse)
library(rstan)
library(shinystan)
#
# test test
## set workspace to 'PinonDemo' folder/repository

## LOAD DATA

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

# ---------- check year availability for size data ---------

# individuals
length(unique(demo.data[which(demo.data$Ht>0),"raw.data.TreeID"]))
# years
sort(unique(demo.data[which(demo.data$Ht>0),]$Year)) # ** 2020 missing from the observed data

# ------- create empty individual x year dataframe -------

# add a 2020 placeholder so that all years are present)

# first column is each year repated for i number of times
treeyears <- data.frame(Year = vapply(2012:2022, rep, times = length(unique(demo.data$raw.data.TreeID)), numeric(length(unique(demo.data$raw.data.TreeID)))) %>% as.vector())


# first column is treid repeated for all years
treeyears$raw.data.TreeID = rep(unique(demo.data$raw.data.TreeID), 11)

# ------- Prep data for growth model -------


# building models where intercept and slope vary by year
# no data for 2020 or 2021 b/c no data collection 2020.
# Sz is dataframe of [year, individual]

## Sz is size at the current time step (NAs will be reassigned to -99)
## SZ.obs is size at the current time step (NAs = NAs)
Sz <- Sz.obs <- treeyears %>%
  mutate(Year = as.integer(Year)) %>%
  full_join(., demo.data) %>%
  select(c(raw.data.TreeID, Year, Ht)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  select(-Year) %>%
  as.matrix()

# remove trees that have no size measurements for the entire time period OR only 1 size measurement
Sz <- Szl.init <- Sz.obs <- Sz[,colSums(is.na(Sz))<(nrow(Sz)-1)]

# reassign NAs as 999 for versions of data that will go into the stan model (Stan doesn't accept NA's)
Sz[is.na(Sz)]<- 999
Szl.init[is.na(Szl.init)] <- mean(Sz.obs, na.rm = T)
# ------ Prep census endpoints data frame --------

## for every tree i the year it first entered the census and then last year in the census

# pull survival data
tree.surv <- treeyears %>%
  mutate(Year = as.integer(Year)) %>%
  full_join(., demo.data) %>%
  select(c(raw.data.TreeID, Year, Alive)) %>%
  pivot_wider(names_from = raw.data.TreeID, values_from = Alive) %>%
  # reorder rows so that years are in order
  arrange(Year)%>%
  as.matrix()

# column tree names should match Sz (remove trees that have no size measurements for the entire time period OR only 1 size) measurement
tree.surv <- tree.surv[,c('Year',colnames(Sz))]

# treeid, startyear, end year
tcy <- matrix(NA,dim(tree.surv)[2]-1,3)


for(i in 1:dim(tree.surv)[2]-1) {
  
  # look at survival for birth and death years
  ind.tree.surv <- tree.surv[,c(1,i+1)]
  
  # add tree name
  tcy[i,1] <- colnames(ind.tree.surv)[2]
  
  # first year of census
  tcy[i,2] <- ind.tree.surv[which(ind.tree.surv[,2]==1),1][1]
  
  # last year of census (because it died the following year)
  if(any(ind.tree.surv[,2]%in%0)) {
    
    tcy[i,3] <- ind.tree.surv[which(ind.tree.surv[,2]==0)-1,1]
    
  }
  
# last year of census (it lived the whole time)
  if(is.na(ind.tree.surv[11,2])==F) {

    tcy[i,3] <- 2022

  }
}

## checking indexing is right
colnames(Sz)==tcy[,1]

# remove treenames, convert to numeric
tcy <- tcy[,-1]
tcy <- apply(tcy, 2, as.integer)
# change years from calendar to relative years 
tcy <- tcy - 2011

# ------------ Run STAN model -------------

# specify model data
# i = length(St1) # index by individuals
i = dim(Sz)[2] # index by individuals
y = dim(Sz)[1] # index by year
c = dim(tcy)[2] # census endpoints

# specify model data
growthdata <- list(i = i, y = y, c = c, tcy = tcy, Sz = Sz)

# set initial latent values
start <- list(list("Szl"=log(Szl.init)),
              list("Szl"=log(Szl.init)),
              list("Szl"=log(Szl.init)))

# fit growth model
options(mc.cores = parallel::detectCores())
growth_ss <- stan(file='models/growth_years_statespace.stan', data=growthdata, init = start, chains=3, iter=3000, warmup=1500) 

growth_ss
launch_shinystan(growth_ss)

# save workspace to google drive 
# save.image(file = "LOCALFILEPATH/growth_statespace_model.RData")