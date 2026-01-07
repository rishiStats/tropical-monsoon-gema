library(dplyr)
library(lubridate)

daily_data_1$form = as.factor(ifelse(is.na(daily_data_1[[a]]), 
                                   "b","a"))
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

daily_data_1 = daily_data_1 %>%
  select(-(42:66), -77)


daily_data_1 = daily_data_1 %>%
  group_by(ID, Day) %>%
  filter(end == min(end)) %>%
  ungroup()

daily_data_1 = daily_data_1 %>%
  mutate(across(c(19, 30:39, 41), as.numeric))
  
daily_data_1 =  daily_data_1 %>%
  mutate(psychopath = q3 + q4 + q5 + q6 + q7, 
         well_being = q8 + q9 + q10 + q11, 
         overall = 20- (psychopath) +well_being,
         psychopath_std = round(psychopath/5), 
         well_being_std = round(well_being/5), 
         overall_std = round(overall/5),
         positive_event = ifelse(q12.3 < 0, "yes", "no"), 
         negative_event = ifelse(q12.3 > 0, "yes", "no"))

daily_data_fin =  daily_data_1 %>%
  select(- c(3:11, 18, 42:50 ))

write_csv(daily_data_fin, "~/tropical-monsoon-gema/data_wrangling/daily_data.csv")
