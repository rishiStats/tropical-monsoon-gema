library(readr)
library(tidyverse)
demographic = read_csv("tropical-monsoon-gema/data_wrangling/demographic/demographic_cleaned.csv")
baseline = read_csv("tropical-monsoon-gema/data_wrangling/baseline/baseline_final.csv")
daily_data = read_csv("tropical-monsoon-gema/data_wrangling/panel_data/daily_data_cleaned.csv")

daily_data %>%
  group_by(ID) %>%
  summarize(total = n(), .groups = "drop") %>%  
  summarize(
    n_students = n(),
    expected   = n() * 30, 
    n_obs = sum(total),
    min        = min(total),
    max        = max(total),
    mean       = mean(total),
    sd         = sd(total),
    median     = median(total),
    iqr        = IQR(total),
    percent    = (mean / 30) * 100,
    .groups    = "drop"
  )

daily_data %>%
  group_by(College, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    n_students = n(),
    expected   = n() * 30, 
    n_obs = sum(total),
    min        = min(total),
    max        = max(total),
    mean       = mean(total),
    sd         = sd(total),
    median     = median(total),
    iqr        = IQR(total),
    percent    = (mean / 30) * 100,
    .groups    = "drop"
  )

daily_data %>%
  group_by(district, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    n_obs = sum(total),
    min        = min(total),
    max        = max(total),
    mean       = mean(total),
    sd         = sd(total),
    median     = median(total),
    iqr        = IQR(total),
    .groups    = "drop"
  )

