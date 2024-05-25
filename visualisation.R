setwd("~/Desktop/Portfolio/LA Crime Project")

library("tidyverse")
library("shiny")
library(viridis)
library(zoo)

data = read_csv("refinedv1.csv")

#returning variables to factors
data = data %>% 
  mutate(patrol_division = as.factor(patrol_division), 
         sub_area = as.factor(sub_area), 
         felony = as.factor(felony),
         crime = as.factor(crime), 
         victim_sex = as.factor(victim_sex),
         victim_race = as.factor(victim_race),
         weapon_description = as.factor(weapon_description),
         status = as.factor(status),
         crime_premis = as.factor(crime_premis)
  )

#visualisations to do: 
#bar graph of difference in date occured date reported, split by crime group 
#bar graph incidence (total), split by crime group (x axis) and felony (color, stacked)

#line graph, x= time (month increments), y = number of crimes recorded (in that month), color = crime group
crime_overtime_crimegroup = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ,crime_group) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count, col = crime_group))+
  geom_line()+
  geom_point()+
  theme_bw()+
  #changes color to color-blind friendly scheme 
  scale_color_viridis(discrete=TRUE)+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.position="bottom",legend.title=element_blank(),
        axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("")+
  ylab("Monthly crimes recorded")+
  #changing frequency of labels to monthly and labels just being month and year
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.1))

#line graph, x= time (month increments), y = number of crimes recorded (in that month), color = felony
crime_overtime_felony = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ,felony) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count, col = felony))+
  geom_line()+
  geom_point()+
  theme(legend.position = "bottom")+
  theme_bw()+
  #changes color to color-blind friendly scheme, sets colors on scale to use, sets legend labels
  scale_color_viridis(discrete=TRUE,begin = 0.3, end = 0.95,labels = c("Felony", "Less serious crime"))+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.position="bottom",legend.title=element_blank(),
        axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("")+
  ylab("Monthly crimes recorded")+
  #changing frequency of labels to monthly and labels just being month and year
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.1))

#line graph, x= time (month increments), y = number of crimes recorded (in that month), color = felony
#In shiny needs to have options to select the patrol division, too many and can't merge
crime_overtime_patrol = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ,patrol_division) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count, col = patrol_division))+
  geom_line()+
  geom_point()+
  theme_bw()+
  #changes color to color-blind friendly scheme 
  scale_color_viridis(discrete=TRUE)+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.position="bottom",legend.title=element_blank(),
        axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("")+
  ylab("Monthly crimes recorded")+
  #changing frequency of labels to monthly and labels just being month and year
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.1))

data %>% 
  mutate(date_diff = date_reported - date_occured) %>% 
  group_by(crime_group,date_diff) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = date_diff, y = count, col = crime_group))+
  geom_line()+
  facet_wrap(~crime_group, ncol =3)
  geom_point()

