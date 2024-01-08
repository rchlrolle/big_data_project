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


#Date Conversion -----------------------------------------------------
dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022, dc.data2023)

dc.data.temp <- separate(dc.data.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data.temp$DATE <- as.Date(dc.data.temp$DATE, format = "%Y/%m/%d")
dc.data.temp$NEIGHBORHOOD_CLUSTER <- toupper(dc.data.temp$NEIGHBORHOOD_CLUSTER)
dc.data.temp$HOUR <- substr(dc.data.temp$TIME, 0, 2)



dc.data <- dc.data.temp %>%
  mutate(
    START = ymd_hms(START_DATE, tz = "America/New_York"),
    END = ymd_hms(END_DATE, tz = "America/New_York")
  ) 
  
 

# DATE: Y, M, D, DOW-------------------------------------------------------------

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)

# Clean '08-'23 Data -----------------------------------------------


dc.crime.data <- dc.data%>%
  select(OFFENSE, METHOD, SHIFT, DATE, TIME, START, END, YEAR, MONTH, DAY, DOW, LATITUDE, LONGITUDE)%>%
  mutate(RESPONSE_TIME = difftime(END, START, "mins"))


# Need to create our list of violent crimes per DC
viol_crime <- c("HOMICIDE", "SEX ABUSE", "ROBBERY", "ASSAULT W/DANGEROUS WEAPON")

# create our violent_crime column and reorder the columns so its first followed by offense
dc.crime <- dc.crime.data %>% 
  mutate(VIOLENT_CRIME = ifelse(OFFENSE %in% viol_crime, 1, 0)) %>% 
  select(VIOLENT_CRIME, OFFENSE, everything())


# Plotting Response Time on Graph -----------------------------------------

#Got negatives in Response Time; so will filter these out before starting analysis

response.data <-dc.crime %>%
  filter(RESPONSE_TIME >= 0 & RESPONSE_TIME < 200000000) %>%
  mutate(RESPONSE_TIME = as.numeric(RESPONSE_TIME, units = "mins"))

# Count of Violent Crime --------------------------------------------------


ggplot(response.data, aes(x = VIOLENT_CRIME, fill= factor(VIOLENT_CRIME))) +
  geom_histogram(bins=4) +
  labs( x = "Nonviolent vs Violent Crime")

# Count of Crime: DOW, Month, Year, TYPE, Non violent -----------------------------------------------------

library(plotly)


plot_ly(response.data) %>%
  add_histogram(x = ~DOW, name = "Day of Week", marker = list(color = '#FFA500')) %>%
  add_histogram(x = ~as.Date(MONTH, format="%B"), name = "Month", marker = list(color = '#00CED1')) %>%
  add_histogram(x = ~YEAR, name = "Year", marker = list(color = '#FF0000')) %>%
  add_histogram(x = ~OFFENSE, name = "Offense", marker = list(color = '#800080')) %>%
  add_histogram(x = ~SHIFT, name = "Shift", marker = list(color = '#008000')) %>%
  layout(title = "Crimes Based on the Respective Filters")
 

# Plotting GLM  -----------------------------------------------------------

ggplot(response.data, aes(x= RESPONSE_TIME, y= VIOLENT_CRIME))+ geom_point()+
  geom_smooth(method = "glm",
              method.args= list(family="binomial"),
              se=FALSE)

glm_model <- glm(VIOLENT_CRIME ~ RESPONSE_TIME, family = binomial, data= response.data)
summary(glm_model)



# MAP ---------------------------------------------------------------------
library(usmap)
station.locations <- read.csv("https://opendata.arcgis.com/api/v3/datasets/05d048a0aa4845c6a0912f3a9f216992_6/downloads/data?format=csv&spatialRefId=4326&where=1%3D1", stringsAsFactors = FALSE)

# Plotting the US map with Washington, D.C.
dc.map <-plot_usmap(include = "DC") +
  labs(title = "Map of the United States with Washington, D.C.")



crime.map <-response.data%>%select(OFFENSE,LATITUDE, LONGITUDE)%>%
  plot_ly() %>%
  add_trace(type = 'scattermapbox', mode = 'markers',
            lat = ~LATITUDE, lon = ~LONGITUDE, marker = list(size = 10, color = 'red'))

ems.map <- station.locations%>%select(TYPE, LATITUDE, LONGITUDE)%>%
  plot_ly()%>%
  add_trace(type = 'scattermapbox', mode = 'markers',
            lat = ~LATITUDE, lon = ~LONGITUDE, marker = list(size = 10, color = 'green'))
  

# Combine the base map with the plotly data points
subplot(dc.map, crime.map, ems.map, nrows = 1)
