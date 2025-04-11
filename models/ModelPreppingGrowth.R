library(tidyverse)
library(rstan)
library(shinystan)

## set workspace to 'PinonDemo' folder/repository

## LOAD DATA

demo.data <- read.csv("cleaned_demo_data.csv")

# remove outlier (just removing entire tree's row for now)
demo.data <- demo.data[-which(demo.data$CanDiam2==22.40),]

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
## SZ is size at the current time step (NAs = NAs)
Sz <- Sz.obs <-  treeyears %>%
  mutate(Year = as.integer(Year)) %>%
  full_join(., demo.data) %>%
  select(c(raw.data.TreeID, Year, Ht)) %>%
  # reorganize data so columns are individuals, rows are years
  pivot_wider(names_from = raw.data.TreeID, values_from = Ht) %>%
  # reorder rows so that years are in order
  arrange(Year) %>%
  select(-Year) %>%
  as.matrix()

# reassign NAs as -99 for versions of data that will go into the stan model (Stand doesn't accept NA's)
Sz[is.na(Sz)]<- -99

# ------ Prep census endpoints data frame --------

## for every tree i the year it first entered the census and then last year in the census

# treeid, startyear, end year
tcy <- matrix(NA,length(unique(demo.data$raw.data.TreeID)),3)

# pull survival data
tree.surv <- treeyears %>%
  mutate(Year = as.integer(Year)) %>%
  full_join(., demo.data) %>%
  select(c(raw.data.TreeID, Year, Alive)) %>%
  pivot_wider(names_from = raw.data.TreeID, values_from = Alive) %>%
  # reorder rows so that years are in order
  arrange(Year)%>%
  as.matrix()

for(i in 1:length(unique(demo.data$raw.data.TreeID))) {
  
  # look at survival for birth and death years
  ind.tree.surv <- tree.surv[,c(1,i+1)]
  
  # add tree name
  tcy[i,1] <- colnames(ind.tree.surv)[2]
  
  # first year of census
  tcy[i,2] <- ind.tree.surv[which(ind.tree.surv[,2]==1),1][1]
  
  # last year of census (because it died)
  if(any(ind.tree.surv[,2]%in%0)) {
    
    tcy[i,3] <- ind.tree.surv[which(ind.tree.surv[,2]==0),1]
    
  }
  
# last year of census (it lived the whole time)
  if(is.na(ind.tree.surv[11,2])==F) {

    tcy[i,3] <- 2022

  }
}

# ------- remove trees that are completely NA's the whole time?-------

length(which(is.na(tcy[,2])))
length(which(is.na(tcy[,3])))

# ------------ Run STAN model -------------

# specify model data
# i = length(St1) # index by individuals
i = dim(Sz)[2] # index by individuals
y = dim(Sz)[1] # index by year

# specify model data
growthdata <- list(i = i, y = y, Sz = Sz)

#start <- list() # specify starting values, if needed

# fit growth model
growth_ss <- stan(file='models/growth_years_statespace.stan', data=growthdata, chains=3, iter=3000, warmup=1500) 

