library(car)
library(lubridate)
library(readr)
library(tidyverse)
library(tseries)
library(ggplot2)



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

  
# DATE: Y, M, D, DOW-------------------------------------------------------------

dc.data.temp$YEAR <- substr(dc.data.temp$DATE, 0, 4)
dc.data.temp$MONTH <- month(dc.data.temp$DATE)
dc.data.temp$DAY <- day(dc.data.temp$DATE)
dc.data.temp$DOW <- weekdays(dc.data.temp$DATE)



# Clean '08-'23 Data -----------------------------------------------


dc.crime.temp <- dc.data.temp%>%
  select(OFFENSE, METHOD, SHIFT, DATE, TIME, YEAR, MONTH, DAY, DOW, START_DATE, LATITUDE, LONGITUDE)


# Need to create our list of violent crimes per DC
viol_crime <- c("HOMICIDE", "SEX ABUSE", "ROBBERY", "ASSAULT W/DANGEROUS WEAPON")

# create our violent_crime column and reorder the columns so its first followed by offense
dc.crime <- dc.crime.temp %>% 
  mutate(VIOLENT_CRIME = ifelse(OFFENSE %in% viol_crime, 1, 0)) %>% 
  select(VIOLENT_CRIME, OFFENSE, everything())



# Count of Violent Crime --------------------------------------------------

ggplot(dc.crime, aes(x = VIOLENT_CRIME, fill= factor(VIOLENT_CRIME))) +
  geom_histogram(bins=4) +
  labs( x = "Nonviolent vs Violent Crime")


# Count of Offenses  ----------------------------


dc.crime.count <- dc.crime %>%
  group_by(OFFENSE, VIOLENT_CRIME) %>%
  summarise(count = n(), .groups = 'drop')

# Plotting a bar plot
ggplot(dc.crime.count, aes(x = count, y = OFFENSE, fill = OFFENSE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Counts of Offenses ", x = "Offense", y = "Count")+
  theme(legend.position = "none")+
  scale_color_colorbind()
# Count of Crime: DOW, Month, Year, TYPE, Non violent -----------------------------------------------------

library(plotly)


plot_ly(dc.crime) %>%
  add_histogram(x = ~DOW, name = "Day of Week", marker = list(color = '#FFA500')) %>%
  add_histogram(x = ~MONTH, name = "Month", marker = list(color = '#00CED1')) %>%
  add_histogram(x = ~YEAR, name = "Year", marker = list(color = '#FF0000')) %>%
  add_histogram(x = ~OFFENSE, name = "Offense", marker = list(color = '#800080')) %>%
  add_histogram(x = ~SHIFT, name = "Shift", marker = list(color = '#008000')) %>%
  layout(title = "Crimes Based on: Day of Week, Month, Year, Offense, Shift")



# Exploring Graphs & LM, ANOVA, GLM ------------------------------------------------------------
viol_crime <- c("HOMICIDE", "SEX ABUSE", "ROBBERY", "ASSAULT W/DANGEROUS WEAPON")


# START-REPORT
report_data <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022, dc.data2023)%>%
               mutate(
                      VIOLENT_CRIME = ifelse(OFFENSE %in% viol_crime, 1, 0),
                      REPORT = ymd_hms(REPORT_DAT, tz = "America/New_York"),
                      START = ymd_hms(START_DATE, tz = "America/New_York")
               )%>%
               mutate(report_start_diff = as.numeric(difftime(REPORT, START, units = "hours")),
                      ) %>%
               filter(report_start_diff >= 0 & report_start_diff < 500000) #filter outliers

#Report vs Start Data
ggplot(
  report_data, 
  aes(x = REPORT, y= START)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Report vs Start Date",
       x = "Offense",
       y = "Diff in Report vs Start Date (hours)")

length(report_data$report_start_diff)



#GLM Plot
report_data %>%
  ggplot(aes(x = report_start_diff, y = VIOLENT_CRIME)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(
    title = "Nonviolent vs Violent Report Difference from Start Date",
    xlab = "",
    ylab = ""
  )


lm_model <- lm(data= report_data, report_start_diff ~ VIOLENT_CRIME+ SHIFT+ METHOD)
summary(lm_model)

anova_model <- aov(lm_model)
summary(anova_model)

glm_model <- glm(VIOLENT_CRIME ~ report_start_diff, data= report_data)
summary(glm_model)



# P-Test (ADF) -- to be used on a smaller dataset later -------------------------
  adf.test(as.matrix(dc.data)) 

# Temporal topology ------------------------------------------------------------------


# MAP ---------------------------------------------------------------------
library(usmap)
library(plotly)
library(sf)
station.locations <- read.csv("https://opendata.arcgis.com/api/v3/datasets/05d048a0aa4845c6a0912f3a9f216992_6/downloads/data?format=csv&spatialRefId=4326&where=1%3D1", stringsAsFactors = FALSE)
dc_wards <- read.csv("", stringsAsFactors = FALSE)

# US map 
dc.map <- plot_usmap(include = "DC", color = "blue") +
  labs(title = "Washington, DC", subtitle = "Crime near EMS Locations")

#Violent Crime
viol_map <-ggplot(
  report_data, 
  aes(x = X, y= Y, color= VIOLENT_CRIME)) +
  geom_sf(data = dc_wards) +
  geom_point() +
  labs(
    title = "Report vs Start Date",
    x = "Latitude",
    y = "Longitude")

#EMS
ems.map <- station.locations %>% 
  plot_ly(
    type = 'scattermapbox',
    mode = 'markers',
    lat = ~LATITUDE, 
    lon = ~LONGITUDE,
    text = ~WARD,  
    colors = 'green', 
    name = "Crime", 
    marker = list(size = 10)
  )

#Combine MAPs
ggplotly(viol_map)%>%
  add_trace(dc.map)%>%
  add_trace(ems.map)

  







crime.year.day <- dc.data %>%
  group_by(YEAR, DOW, WARD) %>%
  summarise(COUNT = n())
crime.year.day <- subset(crime.year.day, !is.na(crime.year.day$WARD))

ggplot(crime.year.day, aes(WARD, DOW, fill = COUNT)) +
  geom_tile()+
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday", "Sunday")) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  facet_wrap(~YEAR, nrow =4)





dc.crime.count.day <- dc.crime %>%
  group_by(OFFENSE, VIOLENT_CRIME,WARD) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(
  data = dc.crime.count.wards,
  mapping = aes(x = WARD, y = count)
) + geom_point(aes(color = factor(VIOLENT_CRIME), shape = factor(VIOLENT_CRIME))) +
  scale_shape_manual(values = c(16,17))+
  labs(
    title = "Crime Counts by Ward in DC",
    subtitle = "Crime in DC from 2008-2023",
    x = "Ward", y = "Crime Count",
    color = "Nonviolent (0) or Nonviolent (1)", shape = "Nonviolent (0) or Nonviolent (1)"
  ) + 
  scale_color_colorblind()


