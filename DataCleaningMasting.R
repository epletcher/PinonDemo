library(tidyverse)
library(cowplot)

## load masting data
mast.data <- read.csv("sev204_treemastproduction.csv")

# Notes from MAST QAQC
# not all sites began during the same year, but NAs were added (historically) for tree Field_IDs for all years prior to initiation of the study at a site. These NAs should likely be removed before analysis.

## load height dat for mast trees collected by Dylan Taylor in 2024
height.data <- read.csv("Mast_PIED_height.csv")

# ------ DATA CLEANING -----------

## clean mast data

mast.pin <- mast.data %>% 
  filter(Species == "PIED") %>% # filter data to only pinon species
  select(c(Year, Site, Plot, Subplot, Field_ID, Fruit_Count, Age)) %>%
  mutate(Age = factor(Age, levels = c("J","Y","M","O","VO"))) %>% # make age a factor
  unite("Plot_Subplot", c(Plot,Subplot), sep = ".") %>% # create 'plot_subplot' to match height data
  mutate(Plot_Subplot = as.factor(Plot_Subplot)) %>%
  filter(Field_ID > 0) 

## clean height data

height.pin <- height.data %>%
  mutate(Plot_Subplot = as.factor(Plot_Subplot)) %>%
  rename(tree_height_2024 = tree_height) %>%
  mutate(alive = if_else(tree_height_2024=='d',0, if_else(tree_height_2024 > 0, 1, NA))) %>% # make a column for assigning alive/dead status; 0 = dead, 1 = alive or possibly missing for now
  mutate(measured = if_else(tree_height_2024 > 0, 1, 0)) %>% # make a column for whether height was measured in 2024
  mutate(tree_height_2024 = as.numeric(tree_height_2024)) # missing or 'd' will become NA's

  # check missing height matches length of NAs and length of 'dead' in alive column 
  length(which(is.na(height.pin$tree_height_2024))) # all missing height
  length(which(is.na(height.pin$tree_height_2024&height.pin$alive))) # missing height because missing 
  length(which(is.na(height.pin$tree_height_2024)&height.pin$alive==0)) # missing height b/c dead
  
  
## merge height and mast data
reprodat <- mast.pin %>% left_join(height.pin)

## how many trees with height and masting data do we have?
length(which(reprodat$tree_height_2024>0&reprodat$Fruit_Count>=0&reprodat$Year==2020))

## list the Field_ID's that we have height and 2020 masting data for
tree_list <- reprodat %>%
  filter(Year == 2020 & Fruit_Count >= 0 & tree_height_2024 > 0) %>%
  pull(Field_ID) %>%
  unique()

# write.csv(tree_list, "masting_tree_list_2024.csv")

# ----- plot Number of HEIGHT counts by 2020 age/size class

reprodat %>% 
  filter(Year == 2020 & Fruit_Count >= 0 & tree_height_2024 > 0) %>%
  ggplot() +
  geom_bar(aes(x = Age))

reprodat %>% 
  filter(Year == 2020 & Fruit_Count >= 0) %>%
  ggplot() +
  geom_bar(aes(x = Age, fill = as.factor(measured)))

# ------ plot height 2024 based on age class 2023 -------

reprodat %>% 
  filter(Year == 2023 & Fruit_Count >= 0) %>%
  ggplot() +
  geom_point(aes(x = Age, y = tree_height_2024))

# ----- PLOT CONE PROD. VS HEIGHT ----------

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
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  theme_bw()
