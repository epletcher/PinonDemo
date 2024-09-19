library(tidyverse)
library(cowplot)

## load demo data
demo.data <- read.csv("cleaned_demo_data.csv")

# remove outlier (** just removing entire tree's row for now)
demo.data <- demo.data[-which(demo.data$CanDiam2==22.40),]

# ----- Exploratory plotting ---------

# plot size t and size t-1
par(mfrow=c(2,2))
plot(demo.data$CanDiam1.tmin.1, demo.data$CanDiam1, ylim = c(0,6), xlim = c(0,6))
abline(0, 1, lty = 2)
plot(demo.data$CanDiam2.tmin.1, demo.data$CanDiam2, ylim = c(0,6), xlim = c(0,6))
abline(0, 1, lty = 2)
plot(demo.data$Ht.t.min.1, demo.data$Ht, ylim = c(0,7), xlim = c(0,7))
abline(0, 1, lty = 2)
plot(demo.data$DBH.tmin.1, demo.data$DBH, ylim = c(0,22), xlim = c(0,22))
abline(0, 1, lty = 2)

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

# ------- model ----------

## Growth
summary(lm(CanDiam1-CanDiam1.tmin.1 ~ CanDiam1.tmin.1, data = demo.data))
summary(lm(CanDiam1-CanDiam1.tmin.1 ~  CanDiam1.tmin.1.sq + CanDiam1.tmin.1, data = demo.data))

plot_grid(
demo.data %>% ggplot(aes(x = CanDiam1.tmin.1, y = CanDiam1-CanDiam1.tmin.1)) + 
  geom_point() + geom_smooth(method = "lm"),
demo.data %>% ggplot(aes(x = CanDiam2.tmin.1, y = CanDiam2-CanDiam2.tmin.1)) + 
  geom_point() + geom_smooth(method = "lm"),
demo.data %>% ggplot(aes(x = Ht.t.min.1, y = Ht-Ht.t.min.1)) + 
  geom_point() + geom_smooth(method = "lm"),
demo.data %>% ggplot(aes(x = DBH.tmin.1, y = DBH-DBH.tmin.1)) + 
  geom_point() + geom_smooth(method = "lm")
)

## Survival

summary(glm(Alive ~ CanDiam1.tmin.1, family = binomial(link = logit), data = demo.data))
summary(glm(Alive ~ CanDiam1.tmin.1.sq + CanDiam1.tmin.1, family = binomial(link = logit), data = demo.data))

# survival as a function of size
plot_grid(
demo.data %>% ggplot(aes(x = CanDiam1.tmin.1, y = Alive)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial")),
demo.data %>% ggplot(aes(x = CanDiam2.tmin.1, y = Alive)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial")),
demo.data %>% ggplot(aes(x = Ht.t.min.1, y = Alive)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial")),
demo.data %>% ggplot(aes(x = DBH.tmin.1, y = Alive)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "binomial"))
)

# ------- Plot by year ---------------
## growth as a function of size, by year
plot_grid(
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = CanDiam1.tmin.1, y = CanDiam1-CanDiam1.tmin.1)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = CanDiam2.tmin.1, y = CanDiam2-CanDiam2.tmin.1)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = Ht.t.min.1, y = Ht-Ht.t.min.1)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = DBH.tmin.1, y = DBH-DBH.tmin.1)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  ncol = 4
)


## survival as a function of size, by year
plot_grid(
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = CanDiam1.tmin.1, y = Alive)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = CanDiam2.tmin.1, y = Alive)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),
  
  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = Ht.t.min.1, y = Alive)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),

  demo.data %>% 
    # filter to at least remove 2012 and 2021 for a lack of previous year data, but other years may have too little data to be useful too
    filter(Year != 2012 & Year != 2021) %>%
    ggplot(aes(x = DBH.tmin.1, y = Alive)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    facet_wrap(vars(Year), nrow = 8) +
    theme_bw(),

  ncol = 4

)
