install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

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
#In 3-4 sentences compare general patterns in the stream stage between sites around Hurricane Irma.

#Question 2
#What was the earliest date of occurrence for each flood category in each river? 
#How quickly did changes in flood category occur for each river? 
#Do you think there was enough time for advanced warning before a flood category changed?
  
#Question 3
#Which river had the highest stream stage above its listed height in the major flood category?
  
#Question 4
#Copy the url for your R script from GitHub and paste it here.
