library(tidyverse)


vars <- c("psychopath", "well_being", "overall")  

compute_measures <- function(var) {
  min = min((data[[var]]))
  max = max((data[[var]]))
  overall_mean <- mean(data[[var]])
  overall_sd <- sd(data[[var]])
  
  within_person_sd <- data %>%
    group_by(ID) %>%
    summarise(person_sd = sd(.data[[var]]), .groups = 'drop') %>%
    summarise(mean(person_sd)) %>%
    pull()
  
  between_person_sd <- data %>%
    group_by(ID) %>%
    summarise(person_mean = mean(.data[[var]]), .groups = 'drop') %>%
    summarise(sd(person_mean)) %>%
    pull()
  
  within_center_sd <- data %>%
    group_by(College.x) %>%
    summarise(center_sd = sd(.data[[var]]), .groups = 'drop') %>%
    summarise(mean(center_sd)) %>%
    pull()
  
  between_center_sd <- data %>%
    group_by(College.x) %>%
    summarise(center_mean = mean(.data[[var]]), .groups = 'drop') %>%
    summarise(sd(center_mean)) %>%
    pull()
  
  within_district_sd <- data %>%
    group_by(district) %>%
    summarise(district_sd = sd(.data[[var]]), .groups = 'drop') %>%
    summarise(mean(district_sd)) %>%
    pull()
  
  between_district_sd <- data %>%
    group_by(district) %>%
    summarise(district_mean = mean(.data[[var]]), .groups = 'drop') %>%
    summarise(sd(district_mean)) %>%
    pull()
  
  return(c(min, max, overall_mean, overall_sd, 
           within_person_sd, within_center_sd, within_district_sd,
           between_person_sd, between_center_sd,between_district_sd))
}

results_list <- map(vars, ~compute_measures(.x))
results_matrix <- do.call(rbind, results_list)

# summary
final_results <- data.frame(
  Variable = c("Psychopathology", "Well-Being", "Overall"),
  results_matrix,
  row.names = NULL
)
colnames(final_results)[-1] <- c("Min", "Max", "Mean", "sd (Overall)", 
                                 "Within-Person sd", "Within_Center_sd","Within_District_sd",
                                 "Between_Person_sd", "Between_Center_sd","Between_District_sd")

print(final_results)
