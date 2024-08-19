setwd("~/Desktop/Portfolio/LA Crime Project")

library("tidyverse")
library(zoo)
data = read_csv("refinedv1.csv")
data2 = data %>% 
  mutate(month_occured = month(date_occured), 
         year_occured = as.factor(year(date_occured)),
         wday_occured = wday(date_occured, label = T))
#number of crimes 
nrow(data)

#average victim age
mean(data$victim_age,na.rm = T)

#most common crime type
data %>% 
  group_by(time_occured,victim_age,victim_sex,victim_race,crime_group) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#common years
data2 %>% 
  group_by(year_occured) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

data2 %>% 
  group_by(year_occured,crime_group) %>% 
  summarise(count = n()) %>% 
  arrange(year_occured,desc(count)) %>% 
  print(n = 35)

data2 %>% 
  filter(year_occured == "2024") %>% 
  arrange(desc(date_reported))
#average and median reporting time 
data %>% 
  mutate(date_diff = date_reported - date_occured) %>% 
  group_by(crime_group) %>% 
  summarise(avg = mean(date_diff), median = median(date_diff))

#longest reporting times 
data %>% 
  mutate(date_diff = date_reported - date_occured) %>% 
  arrange(desc(date_diff)) %>% 
  select(-c(DR_NO, patrol_division,sub_area,crime_premis,weapon_description,status,lat,lon))

#monthly number of crimes pre and post covid restrictions 
#covid: 2020-03-01 - 2021-06-01
c_start = as_date("2020-03-01")
c_end = as_date("2021-06-01")
covid_monthly = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ) %>% 
  summarise(count = n()) %>% 
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  mutate(covid = ifelse(month_year_occ>=c_start & month_year_occ<c_end, "covid","normal")) %>% 
  group_by(covid) %>% 
  summarise(average_monthly = mean(count))

#percentage increase 
(covid_monthly$average_monthly[2] - covid_monthly$average_monthly[1])/covid_monthly$average_monthly[1]

#fraud and forgery number of crimes in group 
data %>% 
  filter(crime_group == "Fraud and forgery offences") %>% 
  group_by(crime) %>% 
  summarise(count = n())

#looking at time frequency
data2 %>% 
  group_by(time_occured,wday_occured) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(time_occured) %>% 
  summarise(average = mean(count)) %>% 
  arrange(desc(average))