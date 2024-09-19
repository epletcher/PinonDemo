library(tidyverse)
library(cowplot)

## load masting data
mast.data <- read.csv("sev204_treemastproduction.csv")

## load dendrometer data 2006-2015
den.data <- read.csv("Dendrometer_trees_2006-15_with_mast_match.csv")

# Notes from MAST QAQC
# not all sites began during the same year, but NAs were added (historically) for tree Field_IDs for all years prior to initiation of the study at a site. These NAs should likely be removed before analysis.

# Notes from dendrometer meta data
# RZ_No - Tag number from another study (Roman Zlotin Mast production dataset)
# Cur DBH - Current Calculated DBH (cm)
# DRC..cm. - Initial Diameter at root crown (cm)

# ------ DATA CLEANING -----------

unique(mast.data$Species)
## clean mast data

# filter data to only pinon species
mast.pin <- mast.data %>% 
  filter(Species == "PIED") %>%
  # make age a factor
  mutate(Age = factor(Age, levels = c("J","Y","M","O","VO"))) %>%
  select(c(Year, Site, Plot, Subplot, Field_ID, Age, Species, Fruit_Count)) %>%
  filter(Year > 2005) %>%
  filter(Field_ID > 0) 

## clean dendro part data
  # only pinon
den.pin <- den.data %>%
  select(c(Date, Species, RZ_No, Cur.DBH)) %>%
  filter(Species == "PIED") %>%
  # only trees with rz tag number 
  filter(RZ_No > 0) %>%
  # separate out date month year
  separate(col = Date, into = c('Month', 'Day', 'Year')) %>%
  # several dbh measurements a year, take the mean
  group_by(RZ_No,Year) %>%
  summarize(avg.dbh = mean(Cur.DBH, na.rm = T)) %>%
  # rename RZ_No Field_ID
  rename(Field_ID = RZ_No) %>%
  mutate(Field_ID = as.character(Field_ID)) %>%
  mutate(Year = as.numeric(Year))

## merge dendro and mast data

prod.dat <- den.pin %>% left_join(mast.pin)

# ----- PLOTS CONE PROD. by DBH ----------

prod.dat %>% 
  filter(Fruit_Count > 0) %>% # only look at mast years
  ggplot(aes(x = avg.dbh, y = Fruit_Count)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# plot box plots of annual cone production by size class, over time
prod.dat %>% 
  ggplot(aes(x = avg.dbh, y = Fruit_Count)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~Year, scales = "free") +
  theme_classic()

# number of individuals in the dataset
length(unique(prod.dat$Field_ID))

# ----- PLOTS CONE PROD. by AGE CLASS ----------

# plot annual cone production by age class
mast.pin %>% ggplot(aes(x = Age, y = log(Fruit_Count))) + geom_boxplot()
  
# plot box plots of annual cone production by size class, over time
mast.pin %>% ggplot(aes(x = Age, y = log(Fruit_Count))) + 
  geom_boxplot() +
  facet_wrap(~Year)

# ------ PLOT AGE CLASS vs. DBH -----------
# plot annual cone production by age class
prod.dat %>% ggplot(aes(x = Age, y = avg.dbh)) + geom_boxplot() 
