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