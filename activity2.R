install.packages(c("dplyr", "lubridate","tidyverse"))
library(dplyr)
library(lubridate)
library(tidyverse)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

#in-class
#subset data by creating new data frame
peace <- floods_fj %>%
  filter(siteID == "2295637")

example <- floods_fj %>%
  filter(gheight.ft >= 10)

plot(peace$dateF, peace$gheight.ft, type = "l") #p = point, l = line, b = both

max_ht <- floods_fj %>%
  group_by(names) %>%
  summarise(max_ht_ft = max(gheight.ft, na.rm = T))

#in-class prompt 1
#Follow the steps in the tutorial to join streamH and siteInfo into a data frame called Floods. 
#Check if the type of join makes a difference in the outcome.
floods_fj <- full_join(streamH, #left data frame
                    siteInfo, #right data frame
                    by="siteID") #shared data
floods_lj <- left_join(streamH, #left data frame
                    siteInfo, #right data frame
                    by="siteID") #shared data
floods_rj <- right_join(streamH, #left dataframe
                    siteInfo, #right dataframe
                    by="siteID") #shared data
#type of join does not affect the outcome with these data sets because there is full overlap
#between data sets. in data sets that are missing observations or data, the type of join will matter

#in-class prompt 2
#Parse the date for the Floods data frame.
#(using lubridate), parsed before joining data frame
streamH$dateF <- ymd_hm(streamH$datetime, tz = "America/New_York")

year(streamH$dateF)

#in-class prompt 3
#What was the earliest date that each river reached the flood stage?
flood_date <- floods_fj %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(flood_date = min(dateF, na.rm = T))

#Question 1
#Make a separate plot of the stream stage data for each river. 
floods_fj_fish <- filter(floods_fj, siteID == 2256500)
floods_fj_peace <- filter(floods_fj, siteID == 2295637)
floods_fj_with <- filter(floods_fj, siteID == 2312000)
floods_fj_santa <- filter(floods_fj, siteID == 2322500)

theme_set(theme_classic()) 

ggplot(floods_fj_fish, aes(x = dateF, y = gheight.ft)) +
  geom_line() +
  labs(x = "Date", y = "Fisheating Creek Stream Height (ft)")

ggplot(floods_fj_peace, aes(x = dateF, y = gheight.ft)) +
  geom_line() +
  labs(x = "Date", y = "Peace River Stream Height (ft)")

ggplot(floods_fj_with, aes(x = dateF, y = gheight.ft)) +
  geom_line() +
  labs(x = "Date", y = "Withlacoochee River Stream Height (ft)")

ggplot(floods_fj_santa, aes(x = dateF, y = gheight.ft)) +
  geom_line() +
  labs(x = "Date", y = "Santa Fe River Stream Height (ft)")

#Question 2
#What was the earliest date of occurrence for each flood category in each river? 
#How quickly did changes in flood category occur for each river? 
#Do you think there was enough time for advanced warning before a flood category changed?
action_date <- floods_fj %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise(action_date = min(dateF, na.rm = T))

moderate_date <- floods_fj %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(moderate_date = min(dateF, na.rm = T))

major_date <- floods_fj %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(major_date = min(dateF, na.rm = T))

#Question 3
#Which river had the highest stream stage above its listed height in the major flood category?
max_height_diff <- floods_fj %>%
  group_by(names) %>%
  summarise(max_height_diff = max(gheight.ft - major.ft, na.rm = T))
