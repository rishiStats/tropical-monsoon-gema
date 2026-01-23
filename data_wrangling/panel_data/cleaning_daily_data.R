
#loading necessary packages 
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# add form column - indicates which of 2 forms have been used
daily_data_1 = read_csv("data_wrangling/georef_daily_dataa.csv")
daily_data_1$form = as.factor(ifelse(is.na(daily_data_1$q1), 
                                   "b","a"))
daily_data_1 <- daily_data_1 %>%
  rename_with(~ str_replace_all(., "_", "."), contains("q"))

#looping over both versions of questions to ensure unified appearance
for (i in 1:11){
  a = paste0("q",i)
  b = paste0("q",i, "b")
  daily_data_1[[a]] = as.factor(ifelse(is.na(daily_data_1[[a]]), 
                                     daily_data_1[[b]], 
                                     daily_data_1[[a]]))
}
for (i in seq(from =2.1, to =2.9, by=0.1)){
  a = paste0("q",i)
  b = paste0("q",i, "b")
  daily_data_1[[a]] = as.factor(ifelse(is.na(daily_data_1[[a]]), 
                                     daily_data_1[[b]], 
                                     daily_data_1[[a]]))
}
for (i in seq(from =12.1, to =12.3, by=0.1)){
  a = paste0("q",i)
  b = paste0("q",i, "b")
  daily_data_1[[a]] = as.factor(ifelse(is.na(daily_data_1[[a]]), 
                                     daily_data_1[[b]], 
                                     daily_data_1[[a]]))
}

#removing columns not relevant for analysis 
daily_data_1 = daily_data_1 %>%
  mutate(district_id = id_1, district=name) %>%
  select(-(1:3), -(6:14),-(21), -(45:78), -(82:83), -(86:96)) %>%
  filter(!is.na(district_id))
  

# removing duplicate observations (~ only one per day-person, if more than one exist, the earliest value is taken )
daily_data_1 = daily_data_1 %>%
  group_by(ID, Day) %>%
  filter(end == min(end)) %>%
  ungroup()

#converting all numerical variables to numeric format
daily_data_1 = daily_data_1 %>%
  mutate(across(c(9, 20:29, 31), as.numeric))

#creating total scores, standardized total scores and negative and positive events variables 
daily_data_1 =  daily_data_1 %>%
  mutate(psychopath = q3 + q4 + q5 + q6 + q7, 
         well_being = q8 + q9 + q10 + q11, 
         overall = 20- (psychopath) +well_being,
         psychopath_std = round(psychopath/5), 
         well_being_std = round(well_being/4), 
         overall_std = round(overall/9),
         q1 = q1-5,
         q12.3 = q12.3 - 5,
         positive_event = ifelse(q12.3 > 0, 1, NA), 
         negative_event = ifelse(q12.3 < 0, 1, NA))


daily_data_1 = daily_data_1 %>%
  #extracting date of response
  mutate(date_entry = as_date(end),
         #extracting time of time of response
    time_entry = format(end, format = "%H:%M:%S"), 
    #extracting time taken to finish the form 
    time_taken = format(end - start), 
    date = as.Date("2025-11-29") + (Day - 1) ) %>%
  #excluding variables outside 7:20 pm to 4:00 am time period 
  filter(time_entry >= "19:20:00" | time_entry <= "04:00:00" ) %>% 
  #removing start and end column 
  select( - c(start, end))


#removing participants with less than 15 (50%) observations 
daily_data_1 = daily_data_1 %>%
  group_by(ID) %>%
  mutate(total = n()) %>%
  filter( total >= 15) %>%
  ungroup()

#saving the final output 
write_csv(daily_data_1, "~/tropical-monsoon-gema/data_wrangling/panel_data/daily_data_cleaned.csv")
