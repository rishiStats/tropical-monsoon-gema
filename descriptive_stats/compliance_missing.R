library(readr)
library(tidyverse)
demographic = read_csv("tropical-monsoon-gema/data_wrangling/demographic/demographic_cleaned.csv")
baseline = read_csv("tropical-monsoon-gema/data_wrangling/baseline/baseline_final.csv")
daily_data = read_csv("tropical-monsoon-gema/data_wrangling/panel_data/daily_data_cleaned.csv")

data = daily_data %>%
  left_join(demographic, by = "ID") %>%
  left_join(baseline, by = "ID")

data %>%
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

data %>%
  group_by(College.x, ID) %>%
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

data %>%
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

data %>%
  group_by(sex, ID) %>%
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

data %>%
  group_by(sex, ID) %>%
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

data %>%
  group_by(ses_class, ID) %>%
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

data %>%
  group_by(ses_class, ID) %>%
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

data %>%
  group_by(week, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    n_students = n(),
    days_in_week = if_else(first(week) == 5, 2, 7),
    expected = n_students * days_in_week,
    n_obs    = sum(total),
    min      = min(total),
    max      = max(total),
    mean     = mean(total),
    sd       = sd(total),
    median   = median(total),
    iqr      = IQR(total),
    percent  = (mean / days_in_week) * 100,
    .groups  = "drop"
  )

decay = data %>%
  group_by(Day, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    expected   = 99, 
    n_obs = sum(total),
    percent    = (n_obs / expected) * 100,
    .groups    = "drop"
  ) 

decay_model <- lm(percent ~ Day, data = decay)
summary(decay_model)

data %>%
  group_by(day_type, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    n_students = n(),
    days_per_type = if_else(first(day_type) == "Weekday", 20, 10), 
    expected = n_students * days_per_type,
    n_obs    = sum(total),
    min      = min(total),
    max      = max(total),
    mean     = mean(total),
    sd       = sd(total),
    median   = median(total),
    iqr      = IQR(total),
    percent  = (n_obs / expected) * 100,
    .groups  = "drop"
  )

data %>%
  group_by(day_week, day_type, ID) %>%
  summarize(total = n(), .groups = "drop_last") %>% 
  summarize(
    n_students = n(),
    days_per_type = if_else(first(day_type) == "Weekend", 5, 4),
    expected = n_students * days_per_type,
    n_obs    = sum(total),
    min      = min(total),
    max      = max(total),
    mean     = mean(total),
    sd       = sd(total),
    median   = median(total),
    iqr      = IQR(total),
    percent  = (n_obs / expected) * 100,
    .groups  = "drop"
  ) %>%
  arrange(factor(day_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))