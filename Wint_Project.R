library(car)
library(lubridate)
library(readr)
library(tidyverse)




#Get Data
dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE)
dc.data2022 <- read.csv("https://opendata.arcgis.com/datasets/f9cc541fc8c04106a05a1a4f1e7e813c_4.csv", stringsAsFactors = FALSE)
dc.data2021 <- read.csv("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.csv", stringsAsFactors = FALSE)
dc.data2020 <- read.csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv", stringsAsFactors = FALSE)
dc.data2019 <- read.csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv", stringsAsFactors = FALSE)
dc.data2018 <- read.csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv", stringsAsFactors = FALSE)
dc.data2017 <- read.csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv", stringsAsFactors = FALSE)
dc.data2016 <- read.csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv", stringsAsFactors = FALSE)
dc.data2015 <- read.csv("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.csv", stringsAsFactors = FALSE)
dc.data2014 <- read.csv("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.csv", stringsAsFactors = FALSE)
dc.data2013 <- read.csv("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.csv", stringsAsFactors = FALSE)
dc.data2012 <- read.csv("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.csv", stringsAsFactors = FALSE)
dc.data2011 <- read.csv("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.csv", stringsAsFactors = FALSE)
dc.data2010 <- read.csv("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.csv", stringsAsFactors = FALSE)
dc.data2009 <- read.csv("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.csv", stringsAsFactors = FALSE)
dc.data2008 <- read.csv("https://opendata.arcgis.com/datasets/180d56a1551c4e76ac2175e63dc0dce9_32.csv", stringsAsFactors = FALSE)


#Clean Data
dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022)
#There are different lengths in dc.data.temp year 2008-2022 & year 2023
dim(dc.data.temp) #498814 rows by 25 columns
dim(dc.data2023) #34668 rows by 29 columns
 #There is a difference in 4 columns



# Find Unique Variables -------------------------------------------------------------

unique_cols1 <- setdiff(names(dc.data.temp), names(dc.data2023))
print(unique_cols1) #X, Y, ObJECTID
unique_cols2 <- setdiff(names(dc.data2023), names(dc.data.temp))
print(unique_cols2) 



# Clean '08-'22 Data -----------------------------------------------

str(dc.data.temp$START_DATE) #needs to be Date not character

previous_years <- dc.data.temp %>%
  mutate(DATE = ymd_hms(START_DATE, tz = "America/New_York"))%>%
  select(DATE, SHIFT, LATITUDE, LONGITUDE, METHOD, OFFENSE)%>%
  view()
  
str(previous_years$DATE)#Now formatted as date

# Clean '23 Data------------------------------------------------

view(dc.data2023)
year_23 <- dc.data2023 %>%
  mutate(OFFENSE = case_when(
    OFFENSE == "theft f/auto" ~ "MOTOR VEHICLE THEFT",
    TRUE ~ OFFENSE
  )) %>%
  mutate_at(vars(SHIFT, METHOD, OFFENSE), toupper)%>%
  mutate(DATE = mdy_hms(START_DATE, tz = "America/New_York")) %>%
  select(-LATITUDE) %>%  # Remove 'LATITUDE' column to prevent duplicates
  separate(location, into = c("LATITUDE", "LONGITUDE"),sep=",") %>%
  select(DATE, SHIFT, LATITUDE, LONGITUDE, METHOD, OFFENSE)%>%
  view()



  

# Check Dimensions Again --------------------------------------------------

dim(previous_years) #498814 columns & 6 rows
dim(year_23) #34668 columns & 6 rows

dc_crime_data <- rbind(previous_years%>%slice(34668,1:6), year_23)%>%view()



