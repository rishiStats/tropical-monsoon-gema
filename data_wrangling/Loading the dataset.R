library(readxl)
library(dplyr)


for (i in 1:30) {
  if (i <= 10) {
    file_path <- "/Users/rishik/Desktop/Day 1-10.xlsx"
  } else if (i <= 20) {
    file_path <- "/Users/rishik/Desktop/Day 11-20.xlsx"
  } else {
    file_path <- "/Users/rishik/Desktop/Day 21 -30.xlsx"
  }
  
  day <- paste0("Day ", i)
  
  if (i == 1) {
    daily_data =  read_excel(file_path, sheet = day, col_names = TRUE)[, 1:77]
    daily_data = daily_data %>%
      mutate(Day = day)
  } else{
    new_day <- read_excel(file_path, sheet = day, col_names = TRUE)[, 1:77]
    new_day = new_day %>%
      mutate(Day = day)
    daily_data = rbind(daily_data, new_day)
  }
}
