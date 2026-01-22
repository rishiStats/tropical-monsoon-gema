library(readr)

baseline_raw = read_csv("data_wrangling/baseline/baseline_raw.csv")

baseline_scoring = baseline_raw %>%
  mutate(
    depression = dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21,
    anxiety = dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20, 
    stress = dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18, 
    problem_cope = rowSums(pick(paste0("cope_", c(2,7,10,12,14,17,23,25)))),
    emotion_cope = rowSums(pick(paste0("cope_", c(5, 9, 13, 15, 18, 20, 21, 22, 24, 26, 27, 28)))),
    avoidant_cope = rowSums(pick(paste0("cope_", c(1, 3, 4, 6, 8, 11, 16, 19)))),
    swls = rowSums(pick(paste0("swls_", 1:5))),
    social = rowSums(pick(paste0("social_", 1:8))),
    loneliness = rowSums(pick(paste0("ucla_", 1:20))),
    resilience = 18 - rowSums(pick(paste0("brs_", 1:6))), 
    sleep_dur = ifelse(psq_4 >= 7, 0, 
                       ifelse(psq_4 >= 6, 1,
                              ifelse(psq_4 >= 5, 2, 3))),
    sleep_dist = rowSums(pick(paste0("psq_5.", 2:9))),
    sleep_dist = ifelse(sleep_dist == 0, 0, 
                        ifelse(sleep_dist <= 8, 1,
                               ifelse(sleep_dist <= 16, 2, 3))),
    sleep_lat = ifelse(psq_2 <= 15, 0, 
                       ifelse(psq_2 <= 30, 1,
                              ifelse(psq_2 <= 60, 2, 3))),
    sleep_lat = ifelse((sleep_lat +psq_5.1) <= 15, 0, 
                       ifelse((sleep_lat +psq_5.1)<= 30, 1,
                              ifelse((sleep_lat +psq_5.1) <= 60, 2, 3)))
  )

