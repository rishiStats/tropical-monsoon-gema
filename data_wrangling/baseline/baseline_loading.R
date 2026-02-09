library(readr)
library(stringr)
library(hms)
library(dplyr)

#loading dataset
file_1 = read_csv("~/Desktop/GEMA/Data/Questionnaires -1  (Responses) - Form responses 1.csv")
file_2 = read_csv("~/Desktop/GEMA/Data/Questionnaire 2 (Responses) - Form responses 1.csv")
deidentify = read_csv("~/Desktop/GEMA/Data/deidentify - Sheet1.csv")

#deidentifying dataset
deidentify$Number = as.numeric(deidentify$Number)
file_1$`Phone Number  / தொலைபேசி எண்` = as.numeric(file_1$`Phone Number  / தொலைபேசி எண்`)
file_2$`Phone Number / தொலைபேசி எண்` = as.numeric(file_2$`Phone Number / தொலைபேசி எண்`)


baseline_raw = deidentify %>%
  inner_join(file_1, by = c("Number" = "Phone Number  / தொலைபேசி எண்") ,multiple = "first")   %>%
  inner_join(file_2, by = c("Number" = "Phone Number / தொலைபேசி எண்" )  ,multiple = "first" ) %>%
  select(-c("Name", "Number", "College", "Timestamp.x" ,  
            "Timestamp.x" , "Email address.x" ,  "Name / பெயர்.x", 
            "Timestamp.y" ,"Email address.y","Name / பெயர்.y"     ))

#removing tamil characters 
baseline_raw = baseline_raw %>%
  mutate(across(where(is.character), ~str_remove_all(., "[\u0B80-\u0BFF]")))

names = c("ID",
          paste0("dass_", 1:21),
          paste0("cope_", 1:28),
          paste0("swls_", 1:5),
          paste0("social_", 1:8),
          paste0("bfi_", 1:30),
          paste0("ucla_", 1:20),
          paste0("brs_", 1:6),
          paste0("psq_", 1:4),
          paste0("psq_", seq(5.1, 5.9, by =0.1)),
          paste0("psq_", 6:10))

baseline_raw = baseline_raw %>%
  rename_with(~names)

baseline_raw <- baseline_raw %>%
  mutate(across(num_range("ucla_", 1:20), ~ 5 - .x)) %>%
  mutate(across(num_range("social_", 1:9), ~ case_match(
    .x,
    "I can’t rely on anyone at all     " ~ 1,
    "Rely on people a little bit     "   ~ 2,
    "Rely on people somewhat     "       ~ 3,
    "Rely on people quite a bit     "    ~ 4,
    "Rely on people completely     "     ~ 5,))) 

baseline_raw <- baseline_raw %>%
  mutate(across(num_range("psq_", seq(5.1, 5.9, by =0.1)), ~ case_match(
      .x,
      "Not during the past month /   " ~ 0,
      "Less than once a week /    "   ~ 1,
      "Once or twice a week /     "       ~ 2,
      "Three or more time a week /      "    ~ 3 ))) %>%
  mutate(psq_6 = case_match(
    psq_6,                   
    "Very good /  "   ~ 0,
    "Fairly good /  " ~ 1,
    "Fairly bad /  "  ~ 2,
    "Very bad /  "    ~ 3)) %>%
  mutate(across(num_range("psq_", c(7,8)), ~ case_match(
    .x,                   
    "Not during the past month /   "   ~ 0,
    "Less than once a week /    " ~ 1,
    "Once or twice a week /     "  ~ 2,
    "Three or more times a week /      "    ~ 3))) %>%
  mutate(psq_9 = case_match(
    psq_9, 
    "No problem at all /   "  ~ 0,         
    "Somewhat of a problem /  "  ~ 1,      
    "Only a very slight problem /    "  ~ 2,
    "A very big problem /    "  ~ 3)) %>%
  mutate(psq_10 = case_match(
    psq_10, 
    "No bed partner or room mate       "  ~ 0,            
    "Partner in same bed    "     ~ 1,                      
    "Partner in same room, but not same bed    ,    "  ~ 2,
    "Partner/room mate in other room /    " ~ 3))
  

baseline_raw <- baseline_raw %>%
  mutate(
    psq_1 = as.numeric(psq_1),
    psq_1 = case_when(psq_1 > 3000 ~ as.numeric(substr(as.character(psq_1), 1, 4)), TRUE ~ psq_1),
    psq_1 = case_when(
      psq_1 >= 100 ~ as_hms((ifelse(floor(psq_1 / 100) %in% 1:7, floor(psq_1 / 100), 
                                    ifelse(floor(psq_1 / 100) == 12, 0, 
                                           ifelse(floor(psq_1 / 100) %in% 8:11, floor(psq_1 / 100) + 12, floor(psq_1 / 100)))) * 3600) + 
                              ((psq_1 %% 100) * 60)),
      TRUE ~ as_hms((case_when(floor(psq_1) %in% 1:6 ~ floor(psq_1),
                               floor(psq_1) %in% 7:11 ~ floor(psq_1) + 12,
                               floor(psq_1) == 12 ~ 0,
                               TRUE ~ floor(psq_1)) * 3600) + 
                      (round((psq_1 - floor(psq_1)) * 100) * 60))
    ),
    psq_3 = as.numeric(psq_3),
    psq_3 = case_when(psq_3 == 67 ~ 630, psq_3 == 89 ~ 830, TRUE ~ psq_3),
    psq_3 = case_when(
      psq_3 >= 100 ~ as_hms((floor(psq_3 / 100) * 3600) + ((psq_3 %% 100) * 60)),
      TRUE ~ as_hms((floor(psq_3) * 3600) + (round((psq_3 - floor(psq_3)) * 100) * 60))
    )
  )


library(dplyr)

baseline_raw <- baseline_raw %>%
  mutate(
    psq_2 = as.numeric(psq_2),
    psq_2 = case_when(
      psq_2 == 0.2 ~ 20,
      psq_2 == 150 ~ 2.5,
      psq_2 == 360 ~ 6,
      psq_2 == 540 ~ 9,
      psq_2 == 180 ~ 3,
      TRUE ~ psq_2
    ),
    psq_2 = round(psq_2),
    psq_4 = as.numeric(psq_4),
    psq_4 = case_when(
      psq_4 == 59 ~ 7,
      psq_4 == 89 ~ 8,
      psq_4 == 6.3 ~ 6.5,
      psq_4 == 1 ~ 7,
      TRUE ~ psq_4
    )
  )

write_csv(baseline_raw, "~/tropical-monsoon-gema/data_wrangling/baseline/baseline_raw.csv")