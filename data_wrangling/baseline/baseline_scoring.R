library(readr)
library(dplyr)
library(lubridate)

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
    sleep_dist_fin = ifelse(sleep_dist == 0, 0, 
                        ifelse(sleep_dist <= 8, 1,
                               ifelse(sleep_dist <= 16, 2, 3))),
    sleep_lat = ifelse(psq_2 <= 15, 0, 
                       ifelse(psq_2 <= 30, 1,
                              ifelse(psq_2 <= 60, 2, 3))),
    sleep_lat_fin = ifelse((sleep_lat +psq_5.1) <= 15, 0, 
                       ifelse((sleep_lat +psq_5.1)<= 30, 1,
                              ifelse((sleep_lat +psq_5.1) <= 60, 2, 3))),
    day_dysf = ifelse((psq_8 + psq_9) ==0, 0, 
                      ifelse((psq_8 + psq_9) <= 2, 1,
                             ifelse((psq_8 + psq_9)  <= 4, 2, 3))),
    diff_hour = as.numeric(lubridate::hms(psq_3) - 
                            lubridate::hms(psq_1), "hours") %% 24,
    tmphse = (psq_4/diff_hour)*100,
    sleep_eff = ifelse(tmphse >85, 0, 
                       ifelse(tmphse > 75, 1,
                              ifelse(tmphse  >65, 2, 3))),
    sleep_qual = psq_6, 
    sleep_meds = psq_7, 
    sleep_total = sleep_dur + sleep_dist_fin + sleep_lat_fin + 
      day_dysf + sleep_eff +  sleep_qual + sleep_meds,
    extravert = 18 - rowSums(pick(paste0("bfi_", c(1,6,11,16,21,26)))), 
    agreeable =18 - rowSums(pick(paste0("bfi_", c(2,7,12,17,22,27)))),
    conscience = 18 - rowSums(pick(paste0("bfi_", c(3,8,13,18,23,28)))),
    neurotic = 18 - rowSums(pick(paste0("bfi_", c(4,9,14,19,24,29)))),
    openness = 18 - rowSums(pick(paste0("bfi_", c(5,10,15,20,25,30))))
  ) %>%
  select(ID, depression, anxiety,stress, problem_cope,  emotion_cope, 
         avoidant_cope,swls, social, loneliness, sleep_dur,sleep_dist_fin, 
         sleep_lat_fin,day_dysf, sleep_qual, sleep_meds, sleep_total, 
         extravert, agreeable,conscience, neurotic, openness  )

baseline_final = baseline_scoring %>%
  mutate(
    depress_cat = ifelse(depression <=9, "normal", 
                         ifelse(depression <=13, "mild",
                                ifelse(depression <= 20, "moderate",
                                       ifelse(depression <= 27, "severe", "extreme")))), 
    anxiety_cat= ifelse(depression <=7, "normal", 
                        ifelse(depression <=9, "mild",
                               ifelse(depression <= 14, "moderate",
                                      ifelse(depression <= 19, "severe", "extreme")))),
    stress_cat= ifelse(depression <=14, "normal", 
                       ifelse(depression <=18, "mild",
                              ifelse(depression <= 25, "moderate",
                                     ifelse(depression <= 33, "severe", "extreme")))),
    swls_cat = ifelse(swls <=9, "extremely dissatisfied",
                      ifelse(swls <= 14, "dissatisfied", 
                             ifelse(swls <= 19, "slightly dissatisfied", 
                                    ifelse (swls == 20, "neutral",
                                            ifelse(swls <= 25, "slightly satisfied", 
                                                   ifelse (swls<= 30, "satisfied", "extremely satisfied")))))),
                               
    psqi = ifelse(sleep_total <=5, "good", "bad" )
  )

write_csv(baseline_final,"~/tropical-monsoon-gema/data_wrangling/baseline/baseline_final.csv" )