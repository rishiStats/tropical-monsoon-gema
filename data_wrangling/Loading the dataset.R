library(readxl)
library(dplyr)


for (i in 1:30) {
  if (i <= 10) {
    file_path = "/Users/rishik/Desktop/Day 1-10.xlsx"
  } else if (i <= 20) {
    file_path = "/Users/rishik/Desktop/Day 11-20.xlsx"
  } else {
    file_path = "/Users/rishik/Desktop/Day 21 -30.xlsx"
  }
  
  day = paste0("Day ", i)
  
  if (i == 1) {
    daily_data =  read_excel(file_path, sheet = day, col_names = TRUE)[, 1:77]
    daily_data = daily_data %>%
      mutate(Day = day)
  } else{
    new_day = read_excel(file_path, sheet = day, col_names = TRUE)[, 1:77]
    new_day = new_day %>%
      mutate(Day = day)
    daily_data = rbind(daily_data, new_day)
  }
}



daily_data  = daily_data %>%
  rename_with(~ c("location", "latitude", "longitude","altitude", 
                  "precision", "location_desc","minutes", "name", "number", 
                  "q1", "q2", paste0("q2.", 1:9),  
                  paste0("q", 3:11),  paste0("q12.", 1:3), 
                  "q1b", "q2b", paste0("q2.", 1:9, "b"),  
                  paste0("q", 3:11, "b"),  paste0("q12.", 1:3, "b")
                  ), 12:66)

library(readr)
deid_list <- read_csv("~/Downloads/deidentify - Sheet1.csv")

daily_data$number = as.numeric(daily_data$number)
deid_list$Number = as.numeric(deid_list$Number)

daily_data = daily_data %>%
  left_join(deid_list, by = c("number" = "Number"), relationship = "many-to-many") %>%
  select(-name, -number, -Name)

write_csv(daily_data, "~/tropical-monsoon-gema/data_wrangling/daily_data.csv")