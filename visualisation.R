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
         crime_premis = as.factor(crime_premis),
         crime_group = as.factor(crime_group)
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
crime_overtime_crimegroup
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

crime_overtime_felony
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

#getting number of offences, in preperation for calculating percentages
n_burglary = sum(data$crime_group == "Burglary and theft")
n_violent = sum(data$crime_group == "Violent offences")
n_other = sum(data$crime_group == "Other")
n_tresspass = sum(data$crime_group == "Criminal damage, trespassing, and related offences")
n_fraud = sum(data$crime_group == "Fraud and forgery offences")
n_sex = sum(data$crime_group == "Sex offences")
n_threat = sum(data$crime_group == "Extortion and threatening offences")

#faceted bar graph (by crime), showing percentage of crimes reported within time period 
date_diff_by_crime = data %>% 
  #calculating time difference 
  mutate(date_diff = date_reported - date_occured) %>% 
  #grouping time difference 
  mutate(date_diff_group = case_when(date_diff == 0 ~ "Same day",
                                     date_diff>=1 & date_diff <=3 ~ "1 - 3 days",
                                     date_diff>=4 & date_diff <=7 ~ "4 - 7 days",
                                     date_diff>=8 & date_diff <=14 ~ "8 - 14 days",
                                     date_diff>=15 & date_diff <=29 ~ "15 - 29 days",
                                     date_diff>=30 & date_diff <=89 ~ "30 - 89 days",
                                     date_diff>=90 & date_diff <=179 ~ "90 - 179 days",
                                     date_diff>=180 ~ "More than 180 days")) %>%
  #adding column with total number of crimes 
  mutate(denom = case_when(crime_group == "Burglary and theft"~ n_burglary,
                           crime_group == "Violent offences"~ n_violent,
                           crime_group == "Other"~ n_other,
                           crime_group == "Criminal damage, trespassing, and related offences"~ n_tresspass,
                           crime_group == "Fraud and forgery offences"~ n_fraud,
                           crime_group == "Sex offences"~ n_sex,
                           crime_group == "Extortion and threatening offences"~ n_threat)) %>% 
  #changing to factor so that logical order is presented in graph
  mutate(date_diff_group = factor(date_diff_group, levels = c("Same day","1 - 3 days","4 - 7 days",
                                                              "8 - 14 days","15 - 29 days","30 - 89 days",
                                                              "90 - 179 days","180 - 364 days","More than 180 days"))) %>% 
  group_by(crime_group,date_diff_group,denom) %>% 
  summarise(count = n()) %>% 
  #calculating percentage
  mutate(percent = 100*(count/denom)) %>% 
  ggplot(mapping = aes(x = date_diff_group, y = percent, fill = date_diff_group))+
  geom_col()+
  facet_wrap(~crime_group, ncol =3)+
  ylim(0,100)+
  scale_fill_viridis(discrete=TRUE)+
  theme_bw()+
  ylab("Percentage of crimes reported")+
  xlab("Time taken to report crime")+
  theme(legend.position="none",
        axis.text.x=element_text(angle=60,hjust = 1))

date_levels = c("Same day","3 days","7 days",
                "14 days","30 days","90 days",
                "180 days","More than 180 days")

#within timeframe graph
cumulative_reported = data %>% 
  #calculating time taken to report crime (in days)
  mutate(date_diff = date_reported - date_occured) %>% 
  group_by(date_diff,crime_group) %>% 
  #calculating count
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(crime_group) %>% 
  #calculating cumulative count
  mutate(sum = cumsum(count)) %>% 
  #adding total number of crimes in prep for percentage calculation
  mutate(denom = case_when(crime_group == "Burglary and theft"~ n_burglary,
                           crime_group == "Violent offences"~ n_violent,
                           crime_group == "Other"~ n_other,
                           crime_group == "Criminal damage, trespassing, and related offences"~ n_tresspass,
                           crime_group == "Fraud and forgery offences"~ n_fraud,
                           crime_group == "Sex offences"~ n_sex,
                           crime_group == "Extortion and threatening offences"~ n_threat)) %>% 
  #calculating cumulative percent 
  mutate(percent = 100*(sum/denom)) %>% 
  ggplot(mapping = aes(x = date_diff, y = percent,col = crime_group))+
  geom_line()+
  theme_bw()+
  ylim(0,100)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="bottom")+
  xlab("Time post offence")+
  ylab("Cumulative percentage of crimes reported")+
  labs(col = "Crime group")

#zoomed in version of graph above, to interesting bit
cumulative_reported_zoomed = cumulative_reported+
  ylim(75,100)+
  xlim(0,1000)

crime_group_split_felony = data %>% 
  group_by(crime_group,felony) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = crime_group, y = count, fill = felony)) + 
  geom_col()+
  scale_fill_viridis(discrete=TRUE,begin = 0.25, end = 0.99,labels = c("Felony", "Less serious crime"))+
  theme_bw()+
  #changes color to color-blind friendly scheme 
  scale_color_viridis(discrete=TRUE)+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.title=element_blank())+
  xlab("")+
  ylab("Number of crimes reported")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))



  


