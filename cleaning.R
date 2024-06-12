setwd("~/Desktop/Portfolio/LA Crime Project")

library("tidyverse")
library("shiny")
library(hms)

data = read_csv("crime_data.csv")
head(data)


refined = data %>% 
  #selecting relevant columns
  select(c(-9,-5,-11,-15,-17,-19,-21,-22,-23,-24,-25,-26)) %>% 
  #renaming columns to be more usable (remove spaces) and understandable. information about columns from kaggle 
  rename(date_time_reported = `Date Rptd`, date_time_occured = `DATE OCC`, time_occured = `TIME OCC`, 
         patrol_division = `AREA NAME`,sub_area = `Rpt Dist No`, felony = `Part 1-2`,
         crime = `Crm Cd Desc`,victim_age =`Vict Age`, victim_sex = `Vict Sex`,
         victim_race = `Vict Descent`,crime_premis = `Premis Desc`, weapon_description = `Weapon Desc`, 
         status =`Status Desc`, lat = LAT, lon = LON) %>% 
  #creating columns with dates
  mutate(date_reported = date(mdy_hms(date_time_reported)),date_occured = date(mdy_hms(date_time_occured))) %>% 
  #removing original date columns 
  select(c(-2,-3)) %>% 
  #adding colon to prepare time for parsing
  mutate(time_occured = `str_sub<-`(time_occured,3,1,value = ":")) %>% 
  #turning into date format and then changing to just hours and minutes
  mutate(time_occured = as_hms(parse_date_time(time_occured,orders = "HM"))) %>% 
  #converting factor variables to factors
  mutate(patrol_division = as.factor(patrol_division), 
         sub_area = as.factor(sub_area), 
         felony = as.factor(felony),
         crime = as.factor(crime), 
         victim_sex = as.factor(victim_sex),
         victim_race = as.factor(victim_race),
         weapon_description = as.factor(weapon_description),
         status = as.factor(status),
         crime_premis = as.factor(crime_premis)
         ) %>% 
  #modifiying victim age so if a negative number is age, it is replaced with NA otherwise left as is
  mutate(victim_age = case_when(victim_age < 0 ~ NA, victim_age >= 0 ~ victim_age))


head(refined)
#checking what crimes are committed against those aged 0 
age_checker = refined %>% 
  select(victim_age,crime) %>% 
  filter(victim_age == 0)

levels(age_checker$crime)

#list of crimes that were listed as being committed against those age 0 and could plausibly occur 
possible_zero = c("CHILD ABANDONMENT","CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT","CHILD NEGLECT (SEE 300 W.I.C.)","CHILD STEALING","CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT",
             "CHILD ANNOYING (17YRS & UNDER)","CHILD PORNOGRAPHY","CRIMINAL HOMICIDE","CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)",
             "INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)","INDECENT EXPOSURE","KIDNAPPING - GRAND ATTEMPT","KIDNAPPING","LEWD/LASCIVIOUS ACTS WITH CHILD","MANSLAUGHTER, NEGLIGENT",
             "ORAL COPULATION","MANSLAUGHTER, NEGLIGENT","OTHER MISCELLANEOUS CRIME","RAPE, FORCIBLE","RAPE, ATTEMPTED","SEX OFFENDER REGISTRANT OUT OF COMPLIANCE",
             "SEXUAL PENETRATION W/FOREIGN OBJECT")

refined %>% 
  filter(victim_age == 0, crime %in% possible_zero == F)

#function for if victim's age is 0 and crime is not in possible zero vector, replace victim with NA, otherwise leave as is (0)
zero_function = function(x){
  ifelse(is.na(refined$victim_age[x])==F,ifelse(refined$victim_age[x] == 0 & refined$crime[x] %in% possible_zero == FALSE,return(NA),return(refined$victim_age[x])),return(NA))
}
#applying function to all rows
zero = map_dbl(1:nrow(refined),zero_function)

#replacing original ages with new age list (which checks if crime could be committed against zero age,replaces with NA if not)
refined$victim_age = zero

#vectors of words which indicate different types of crime
sex_ind = c("SEX","SEXUAL","PORNOGRAPHY","LEWD","RAPE","ORAL COPULATION","PEEPING TOM","INDECENT EXPOSURE","PANDERING","PIMPING")
theft_ind = c("THEFT","BURGLARY","STOLEN","BUNCO","SNATCHING","SHOPLIFTING","TILL TAP","PICKPOCKET","STOLEN")
violent_ind = c("ASSAULT","BATTERY","ARSON","LYNCHING","ROBBERY","HOMICIDE","MANSLAUGHTER")
#driving_ind = c("DRIVING","FAILURE TO YIELD")
damage_ind = c("VANDALISM","TELEPHONE PROPERTY - DAMAGE","TRAIN WRECKING","TRESPASSING")
fraud_ind = c("COUNTERFEIT","FRAUD","FORGERY","THEFT OF IDENTITY")
#firearms_ind = c("SHOTS FIRED","FIREARMS")
#court_ind = c("COURT","VIOLATION OF RESTRAINING ORDER","VIOLATION OF TEMPORARY RESTRAINING ORDER")
threat_ind = c("CRIMINAL THREATS","BRANDISH WEAPON","EXTORTION","THREATENING PHONE CALLS/LETTERS")

#returns indexes of entries which fit each group:
sex_indx = grep(paste(sex_ind,collapse = "|"),refined$crime)
theft_indx = grep(paste(theft_ind,collapse = "|"),refined$crime)
violent_indx = grep(paste(violent_ind,collapse = "|"),refined$crime)
#driving_indx = grep(paste(driving_ind,collapse = "|"),refined$crime)
damage_indx = grep(paste(damage_ind,collapse = "|"),refined$crime)
fraud_indx = grep(paste(fraud_ind,collapse = "|"),refined$crime)
#firearms_indx = grep(paste(firearms_ind,collapse = "|"),refined$crime)
#court_indx = grep(paste(court_ind,collapse = "|"),refined$crime)
threat_indx = grep(paste(threat_ind,collapse = "|"),refined$crime)

#assigning crime group
refined$crime_group = rep("Other",nrow(refined))
refined$crime_group[sex_indx] = "Sex offences"
refined$crime_group[theft_indx] = "Burglary and theft"
refined$crime_group[violent_indx] = "Violent offences"
#refined$crime_group[driving_indx] = "Driving offences"
refined$crime_group[damage_indx] = "Criminal damage, trespassing, and related offences"
refined$crime_group[fraud_indx] = "Fraud and forgery offences"
#refined$crime_group[firearms_indx] = "Firearms offences"
#refined$crime_group[court_indx] = "Court and restraining order offences"
refined$crime_group[threat_indx] = "Extortion and threatening offences"

#checking unclassified crimes - making sure they don't find into another category or a new category doesn't need to be created to capture common themes
refined %>% 
  filter(crime_group == "Other") %>% 
  group_by(crime) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

refined %>% 
  group_by(crime_group) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#if lat or lon is 0, replacing with NA 
refined = refined %>% 
  mutate(lat = case_when(lat == 0 ~ NA,
                         .default = lat),
         lon = case_when(lon == 0 ~ NA,
                         .default = lon)) 

write_csv(refined,"refinedv1.csv")