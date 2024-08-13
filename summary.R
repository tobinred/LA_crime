setwd("~/Desktop/Portfolio/LA Crime Project")

library("tidyverse")
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
  