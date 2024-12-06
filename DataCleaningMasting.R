library(tidyverse)
library(cowplot)

## load masting data
mast.data <- read.csv("sev204_treemastproduction.csv")

# Notes from MAST QAQC
# not all sites began during the same year, but NAs were added (historically) for tree Field_IDs for all years prior to initiation of the study at a site. These NAs should likely be removed before analysis.

## load height dat for mast trees collected by Dylan Taylor in 2024
height.data <- read.csv("Mast_PIED_height.csv")

# ------ DATA CLEANING -----------

unique(mast.data$Species)
## clean mast data

# filter data to only pinon species
mast.pin <- mast.data %>% 
  filter(Species == "PIED") %>%
  select(c(Year, Site, Plot, Subplot, Field_ID, Fruit_Count)) %>%
  unite("Plot_Subplot", c(Plot,Subplot), sep = ".") %>%
  mutate(Plot_Subplot = as.factor(Plot_Subplot)) %>%
  filter(Field_ID > 0) 

## filter to only pinon species
height.pin <- height.data %>%
  mutate(Plot_Subplot = as.factor(Plot_Subplot)) %>%
  rename(tree_height_2024 = tree_height) %>%
  mutate(alive = if_else(tree_height_2024=='d',0,1)) %>% # make a column for assigning alive/dead status; 0 = dead, 1 = alive or possibly missing for now
  mutate(tree_height_2024 = as.numeric(tree_height_2024)) # missing or 'd' will become NA's

## merge height and mast data
reprodat <- mast.pin %>% left_join(height.pin)

## how many trees with height and masting data do we have?
reprodat %>%
  filter(Year == 2020 & Fruit_Count > 0) %>%
  pull(Field_ID) %>%
  unique()

# ----- PLOTS CONE PROD. by Height ----------
# plot cone prod vs size in 2024
reprodat %>% 
  ggplot(aes(x = tree_height_2024, y = log(Fruit_Count))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  facet_wrap('Year')


# plot cone production in most recent 2 mast years against size in 2024
reprodat %>% 
  filter(Year == 2018|Year == 2020) %>% # only look at mast years
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x = tree_height_2024, y = Fruit_Count)) +
  geom_point(aes(col = Year)) +
  geom_smooth(aes(col = Year), method = "glm", method.args = list(family = "poisson"))

# plot cone production in only most recent mast year against size in 2024
reprodat %>% 
  filter(Year == 2020) %>% # only look at mast years
  ggplot(aes(x = tree_height_2024, y = Fruit_Count)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"))

# ------- FIT REPRODUCTION MODEL IN STAN -----

# re arrange data

# define 
  