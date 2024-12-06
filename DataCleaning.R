#libraries
library(tidyverse)

raw.data<-read.csv('CirclePlots.csv',header=T)

# ---------- prep 'raw.data' before extracting dmeographic data --------

# filter to pinon only
raw.data<-raw.data[which(raw.data$Species=='PIED'),]
# remove years w/ incomplete data
raw.data<-raw.data[-which(raw.data$Year==2009 | raw.data$Year==2008),]

# Create a unique ID column
raw.data$TreeID<-(paste(raw.data$Site,".",raw.data$Transect,".",raw.data$Tree_Tag_Number,".",raw.data$Plot_Distance))

# Create an RCD_sum column
raw.data <- raw.data %>% 
  mutate(RCD_sum = rowSums(across(starts_with("RCD")), na.rm = T))
  
# for trees with no RCD, we want values to be NA not zero
raw.data$RCD_sum[raw.data$RCD_sum==0] <- NA

# check that the length of NA's in RCD_1 and RCD_sum match
length(raw.data$RCD_sum[is.na(raw.data$RCD_sum)]) == length(raw.data$RCD_1[is.na(raw.data$RCD_1)])    
                              
# check for year-tree ID duplicates in the unique ID column
raw.data %>%
  dplyr::group_by(Year, TreeID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

# ** 3 duplicated entries found for tree ID in same year
# removing the three rows that are duplicated entries here
raw.data <- raw.data[-which(raw.data$TreeID=='PJControl . LAI_Right . 3355 . 50' & raw.data$Year==2017 & is.na(raw.data$RCD_2)),]
raw.data <- raw.data[-which(raw.data$TreeID=='PJControl . Main . 763 . 35' & raw.data$Year==2019 & is.na(raw.data$Height_to_Canopy)),]
raw.data <- raw.data[-which(raw.data$TreeID=='PJControl . Main . 754 . 35'  & raw.data$Year==2022 & is.na(raw.data$Status)),]

# Save tree IDs to vector
TreeIDs<-unique(paste(raw.data$Site,".",raw.data$Transect,".",raw.data$Tree_Tag_Number,".",raw.data$Plot_Distance))

ntrees<-length(TreeIDs) # number of individuals
years<-unique(raw.data$Year) # years 

# ------ Create new demo.data data frame ---------

demo.data<-as.data.frame(raw.data$TreeID)
demo.data$Year<-raw.data$Year
demo.data$Alive<-NA # empy column to fill survival data

## Add annual survival info 
for (i in 1:ntrees){
  
# subset to unique tree
s.data<-subset(raw.data,raw.data$TreeID==TreeIDs[i])

if(sum(which(is.na(s.data$Percent_Dead)))>0){s.data$Percent_Dead[which(is.na(s.data$Percent_Dead)==T)]=100} # In some cases NAs are used in place of 100s (reassign)

# create really dead var if a tree is dead more than once
reallydead<-diff(which(s.data$Percent_Dead==100)) # returns 1's if following yr was also dead
if(sum(reallydead>1)==0 ){ # for any years where a tree didn't 'come back alive'
  fdead<-min(which(s.data$Percent_Dead==100)) # first year dead
  
  s.data$Alive<-NA # add an Alive column to subset
  if(fdead>1 & fdead<100){
    s.data$Alive[1:(fdead-1)]<-1 # assign alive to all years prior to death
    s.data$Alive[fdead]<-0 # assign dead to first year dead
  }
  
  if(fdead>100){
    s.data$Alive<-1 # assign alive if it never died
  }
  # for each individual, each year assign survival as 1 or 0 (or NA)
  demo.data$Alive[demo.data[,1]==TreeIDs[i]]<-s.data$Alive
} 

#if(sum(reallydead>1)>0 ){ ##In these cases trees were said to be 100% dead but then not a following year. 
  #ldead<-max(which(s.data$Percent_Dead==100))
  
  #print (i)
  
  
#} 


}

## Add recruitment data
demo.data$newRecruits <- NA

for (i in 1:ntrees){
  
  ID <- TreeIDs[i] # set tree ID to first tree listed in TreeIDS
  
  # subset to unique tree id
  ind.data<-subset(demo.data, demo.data$`raw.data$TreeID`==ID)
  
# if the first year there is a record of the individual is greater than 2013
# Note: some plots seem to have maybe started in 2013, were there other plots that started later?
# Notes continued: need to check how confident we should be in assigning the status of new recruit to a tree when it first shows up in a census. Was it just missed in previous 
# years?
  if(min(ind.data$Year)>2013 & is.na(ind.data$Alive[which(ind.data$Year==min(ind.data$Year))])==F) { # if its not alive 1st year present its not a new recruit (some 'NA' individuals entered the census after 2013)
    
    rec.year <- min(ind.data$Year) # record the first the year the individual was present

    rID<-which(demo.data$`raw.data$TreeID`==ID & demo.data$Year==rec.year) # row id in demodata

    demo.data$newRecruits[rID] <- 1 # assign a 1 to newRecruits column
  }
}

# how many recruits?
length(which(demo.data$newRecruits==1))

# 3 trees are greater than 2 in height -- too tall for a new recruit?

## Explore RCD *** in progress

# how often are trees multi stemmed?
dim(raw.data[which(raw.data$Multiple_Stems=="Y"),])[1] # column seems to only apply for dbh time period

# how often do trees have atleast one RCD measurement?
dim(raw.data[which(raw.data$RCD_1>0),])[1]

# how many years do trees have an RCD measurement?
sort(unique(raw.data[which(raw.data$RCD_1>0),]$Year))

# how often do trees have atleast one DBH measurement?
dim(raw.data[which(raw.data$DBH_1>0),])[1]

# how many years do trees have a DBH measurement?
sort(unique(raw.data[which(raw.data$DBH_1>0),]$Year))

# for how many trees does the number of RCD measurements change depending on the year?
multistem.dat <- raw.data[which(raw.data$RCD_2>0),]
# need to filter to each tree and see if stems change by year
multistem.dat %>% select(c())

## Add annual growth data 

demo.data$Ht<-NA
demo.data$CanDiam1<-NA
demo.data$CanDiam2<-NA
demo.data$DBH<-NA
demo.data$RCDSum<-NA

for (i in 1:length(demo.data[,1])){
  
  ID<-demo.data$`raw.data$TreeID`[i]
  year<-demo.data$Year[i]
  rID<-which(raw.data$TreeID==ID & raw.data$Year==year) # row id
  
  # for all rows that are not dead or do not have NAs
  if(is.na(demo.data$Alive[i])==F & demo.data$Alive[i]!=0 ){
    # fill in size info 
    demo.data$Ht[i]<-raw.data$Height[rID[1]]
    demo.data$CanDiam1[i]<-raw.data$Greatest_Diameter[rID[1]]
    demo.data$CanDiam2[i]<-raw.data$Perpendicular_Diameter[rID[1]]
    demo.data$DBH[i]<-raw.data$DBH_1[rID[1]]
    demo.data$RCDSum[i]<-raw.data$RCD_sum[rID[1]]
    
  }
  
}


## Add previous years' size information (for growth)

demo.data$Ht.t.min.1 <- NA
demo.data$CanDiam1.tmin.1 <- NA
demo.data$CanDiam2.tmin.1 <- NA
demo.data$DBH.tmin.1 <- NA
demo.data$RCDSum.tmin.1 <- NA

for (i in 1:ntrees){
  
  ID <- TreeIDs[i] # set tree ID to first tree listed in TreeIDS
  
  #ID <- "PJControl . LAI_Left . 3519 . 50" # test on tree with several years of data
  # i =70 tree with 2017 duplicated
  
  # subset to unique tree id
  ind.data<-subset(demo.data, demo.data$`raw.data$TreeID`==ID)
  
  # list years of data present for individual, first to last
  years <- sort(unique(ind.data$Year))
  
  for(j in 1:length(years)) {
    
    year = years[j]
    prev.year = year-1
    
    rID<-which(demo.data$`raw.data$TreeID`==ID & demo.data$Year==year) # print row id equal to tree and current year
    
    if((prev.year) %in% years) { # if data from the previous calendar year exists for an individual, 
      
      # then assign the previous size info for the current year for all t minus 1 columns
      demo.data$Ht.t.min.1[rID] <- ind.data$Ht[which(ind.data$Year==prev.year)]
      demo.data$CanDiam1.tmin.1[rID]<-ind.data$CanDiam1[which(ind.data$Year==prev.year)]
      demo.data$CanDiam2.tmin.1[rID]<-ind.data$CanDiam2[which(ind.data$Year==prev.year)]
      demo.data$DBH.tmin.1[rID]<-ind.data$DBH[which(ind.data$Year==prev.year)]
      demo.data$RCDSum.tmin.1[rID]<-ind.data$RCDSum[which(ind.data$Year==prev.year)]
      
    }
    
  }
}

# ## Finally, add a column for CanDiamAvg and CanDiamAvg2
# # Create an RCD_sum column
# demo.data <- demo.data %>% 
#   mutate(CanDiamAvg = rowMeans(across(c(CanDiam1, CanDiam2)), na.rm = T)) %>% 
#   mutate(CanDiamAvg.tmin.1 = rowMeans(across(c(CanDiam1.tmin.1, CanDiam2.tmin.1)), na.rm = T))


# ** error is duplicate year for treeID==PJControl . LAI_Right . 3355 . 50 (2017 is duplicated), but all NAs so ignoring
subset(demo.data, demo.data$`raw.data$TreeID`=='PJControl . LAI_Right . 3355 . 50')

## save demo data to file
write.csv(demo.data, "cleaned_demo_data.csv", row.names = FALSE)


# ----- Exploratory plotting ---------

# plot size t and size t-1
par(mfrow=c(3,2))
plot(demo.data$CanDiam1.tmin.1, demo.data$CanDiam1, ylim = c(0,6), xlim = c(0,6))
plot(demo.data$CanDiam2.tmin.1, demo.data$CanDiam2, ylim = c(0,6), xlim = c(0,6))
plot(demo.data$Ht.t.min.1, demo.data$Ht, ylim = c(0,7), xlim = c(0,7))
plot(demo.data$DBH.tmin.1, demo.data$DBH, ylim = c(0,22), xlim = c(0,22))
plot(demo.data$RCDSum.tmin.1, demo.data$RCDSum, ylim = c(0,22), xlim = c(0,22))
