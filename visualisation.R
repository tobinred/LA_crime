setwd("~/Desktop/Portfolio/LA Crime Project")

library("tidyverse")
library("shiny")
library(viridis)
library(zoo)
library(ggExtra)
library(ggmap)
library(ggridges)
library(scales)
data = read_csv("refinedv1.csv")

group_count = data %>% group_by(crime_group) %>% summarise(count = n()) %>% arrange(desc(count))
crime_levels = c(group_count$crime_group)
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
         crime_group = factor(crime_group, levels = crime_levels)
  )

#setting line width for some line graphs
l_width = 1.5

#line graph, x= time (month increments), y = number of crimes recorded (in that month), color = crime group
crime_overtime_crimegroup = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ,crime_group) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count, col = crime_group))+
  geom_line(linewidth = l_width)+
  geom_point()+
  theme_bw()+
  #changes color to color-blind friendly scheme 
  scale_color_viridis(discrete=TRUE, name = "Crime type")+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("Month crimes occured")+
  ylab("Number of crimes recorded")+
  #changing frequency of labels to monthly and labels just being month and year
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.1))
crime_overtime_crimegroup
ggsave("crime_overtimegrp_highres.png",dpi = "retina", width = 30, height = 15, units = "cm")

#line graph, x= time (month increments), y = number of crimes recorded (in that month)
crime_overtime = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count))+
  #adding shaded areas to show when stay at home orders were in place
  annotate("rect",xmin = as_date("2020-03-01"),xmax = as_date("2020-05-01"),ymin = -Inf,ymax = Inf,alpha = 0.2,fill = "red")+
  annotate("rect",xmin = as_date("2020-12-01"),xmax = as_date("2021-02-01"),ymin = -Inf,ymax = Inf,alpha = 0.2,fill = "red")+
  #line to show when most restrictions were lifted
  geom_vline(xintercept = as_date("2021-06-01"),color = "red",alpha = 0.9)+
  geom_line(linewidth = l_width)+
  geom_point()+
  theme_bw()+
  #angling x label text and moving down 
  theme(axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("Month crimes occured")+
  ylab("Number of crimes recorded")+
  #changing frequency of labels to monthly and labels just being month and year
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.1))+
  ylim(1000,21000)
crime_overtime
ggsave("crime_overtime_highres.png",dpi = "retina", width = 30, height = 15, units = "cm")

#line graph, x= time (month increments), y = number of crimes recorded (in that month), color = felony
crime_overtime_felony = data %>% 
  #creating new variable which is only year and month, creating table to create graph
  mutate(month_year_occ = as.yearmon(date_occured)) %>% 
  group_by(month_year_occ,felony) %>% 
  summarise(count = n()) %>% 
  #turning date back into date data type so it works with scale_x_date later
  mutate(month_year_occ = as.Date(month_year_occ)) %>% 
  ggplot(mapping = aes(x = month_year_occ, y = count, col = felony))+
  geom_line(linewidth = l_width)+
  geom_point()+
  theme(legend.position = "bottom")+
  theme_bw()+
  #changes color to color-blind friendly scheme, sets colors on scale to use, sets legend labels
  scale_color_viridis(discrete=TRUE,begin = 0.3, end = 0.95,labels = c("Felony", "Less serious crime"), name = "Crime classification")+
  #moving legend to the bottom, removing legend title, angling x label text and moving down 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=60,hjust = 1))+
  xlab("Month crimes occured")+
  ylab("Number of crimes recorded")+
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
crime_overtime_patrol
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
  xlab("Days post offence")+
  ylab("Cumulative percentage of crimes reported")+
  labs(col = "Crime group")

cumulative_reported

#zoomed in version of graph above, to interesting bit
cumulative_reported_zoomed = cumulative_reported+
  ylim(75,100)+
  xlim(0,1000)

cumulative_reported_zoomed

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
  xlab("Crime group")+
  ylab("Number of crimes reported")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))
crime_group_split_felony

#vector of months for axis labels 
months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#creating data2, which has specific columns which allow for more indepth time series analysis
data2 = data %>% 
  mutate(month_occured = month(date_occured), 
         year_occured = as.factor(year(date_occured)),
        wday_occured = wday(date_occured, label = T))



monthly_crime_pattern = data2 %>% 
  group_by(month_occured,year_occured) %>% 
  summarise(count = n()) %>% 
  #2024 removed as not complete data 
  filter(year_occured != "2024") %>% 
  ggplot(mapping = aes(x = month_occured, y = count, color = year_occured)) + 
  geom_line(linewidth = l_width)+
  scale_x_continuous(name = "Month crime occured",breaks = 1:12, labels = months)+
  ylab("Number of crimes reported")+
  labs(col = "Year crime occured")+
  scale_color_viridis(discrete=TRUE)+
  theme_bw()
monthly_crime_pattern

#calculating the average monthly number of crimes reported, by crime group 
monthly_averages = data2 %>% 
  group_by(month_occured,year_occured,crime_group) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(crime_group) %>% 
  summarise(average = mean(count))

monthly_deviation_crime_group = data2 %>% 
  group_by(month_occured,year_occured,crime_group) %>% 
  summarise(count = n()) %>% 
  #adding monthly averages 
  left_join(monthly_averages) %>% 
  #2024 removed as not complete data 
  filter(year_occured != "2024") %>%
  #inc is percentage increase compared to crime average
  mutate(inc = (((count-average)/average)*100)) %>% 
  ggplot(mapping = aes(x = month_occured, y = inc, color = year_occured)) + 
  geom_line(linewidth = 0.55)+
  geom_hline(yintercept=0, color = 'red')+
  #splitting graph by crime group 
  facet_wrap(~crime_group,nrow = 3)+
  #changing x axis labels to be more informative 
  scale_x_continuous(name = "Month crime occured",breaks = 1:12, labels = months)+
  scale_y_continuous(n.breaks = 16)+
  ylab("Percentage change")+
  labs(col = "Year crime occured")+
  scale_color_viridis(discrete=TRUE)+
  theme_bw()
monthly_deviation_crime_group
ggsave("deviation_highres.png",width = 28, height = 28, units = "cm",dpi = "retina")


day_time_heatmap = data2 %>% 
  #creating table with hour occurred and weekday occurred
  mutate(hour_occured = hour(time_occured)) %>% 
  group_by(hour_occured,wday_occured) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = wday_occured, y = hour_occured, fill = count)) +
  geom_tile(color= "white",linewidth=0.2)+
  #reversing scale, correct number of breaks
  scale_y_continuous(trans = "reverse", breaks = 0:23,expand = c(0,0))+
  #removing padding 
  scale_x_discrete(expand = c(0,0))+
  scale_fill_viridis(option = "magma", name = "Number of crimes reported", direction = -1)+
  theme_bw()+
  theme(legend.position = "bottom",axis.ticks=element_blank())+
  #removing background grid for a neater look
  removeGrid()+
  labs(x = "Weekday occured",y = "Hour occured")

day_time_heatmap 
ggsave("heatmap_highres.png",width = 20, height = 20, units = "cm",dpi = "retina")

#WIP another date graph
# data2 %>% 
#   group_by(time_occured,wday_occured) %>% 
#   summarise(count = n()) %>% 
#   ungroup() %>% 
#   group_by(time_occured) %>% 
#   summarise(average = mean(count)) %>% 
#   ggplot(mapping = aes(x = time_occured, y = average)) + 
#   geom_line()+
#   theme_bw()+
#   labs(x = "Time occured", y = "Average number of crimes reported")+
#   scale_x_continuous(breaks = breaks_width("1 hours"))


#in progress: geographical analysis 
# #getting map of LA from stadia. bbox is map boundaries, zoom is level of detail, maptype is coloring etc
# la_map = get_stadiamap( bbox = c(left = -118.8, bottom = 33.7, right = -118.0, top = 34.4), zoom = 11, maptype = "alidade_smooth_dark")
# 
# #number of crimes reported by crime type, mapped onto LA map
# ggmap(la_map)+
#   geom_hex(data,mapping = aes(x = lon, y = lat),bins = 200)+
#   scale_fill_viridis(option = "magma", name = "Number of crimes reported", 
#                      direction = -1, begin = 0.1, end = 1)+
#   xlab("Longitude")+
#   ylab("Latitude")+
#   facet_wrap(~crime_group,nrow= 2)
# 
# base_map = ggmap(la_map)+
#   geom_hex(data,mapping = aes(x = lon, y = lat),bins = 300)+
#   scale_fill_viridis(option = "magma", name = "Number of crimes reported", 
#                      direction = -1, begin = 0.1, end = 1)+
#   xlab("Longitude")+
#   ylab("Latitude")
# 
# la_map_zoomed = get_stadiamap( bbox = c(left = -118.4, bottom = 34.2, right = -118.2, top = 34.0), zoom = 11, maptype = "alidade_smooth_dark")
# 
# ggmap(la_map_zoomed)

#crime between years 
years_stacked = data2 %>% 
  group_by(year_occured,crime_group) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = year_occured, y = count, fill = crime_group)) +
  geom_col()+
  scale_fill_viridis(discrete = T, name = "Crime group")+
  xlab("Year occured")+
  ylab("Count")+
  theme_bw()
ggsave("yearsstacked_highres.png",width = 25, height = 20, units = "cm",dpi = "retina")



time_diff_ridgeplot = data %>% 
  mutate(date_diff = date_reported - date_occured) %>% 
  ggplot(mapping = aes(y = crime_group, x = date_diff, fill = crime_group))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = T)+
  xlim(0,30)
time_diff_ridgeplot
  

  
  
