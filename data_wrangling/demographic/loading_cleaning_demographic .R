library(readr)
library(dplyr)
library(stringr)
library(lubridate)

demographic = read_csv("~/Desktop/GEMA/Data/Socio-Demographic Details (Responses) - Form responses 1.csv")
deidentify = read_csv("~/Desktop/GEMA/Data/deidentify - Sheet1.csv")

demographic$`Phone Number/ தொலைபேசி எண்` = as.numeric(demographic$`Phone Number/ தொலைபேசி எண்`)
demographic = deidentify %>% 
  left_join(demographic, by = c("Number" = "Phone Number/ தொலைபேசி எண்"  ))%>% 
  select(- c(Name, Number, Timestamp,  "Email address", 
             "Do you consent to be part of the study?" ,"Name / பெயர்" ))

demographic = demographic %>% 
  rename( dob = "Date of Birth / பிறந்த தேதி",
          sex = "Sex / பால்",
          gender = "Gender Identity / பாலினம்",
          residence = "Place of Residence\nவசிப்பிடம்" ,
          dur_residence = "How long have you lived here ?\nநீங்கள் இங்கு எவ்வளவு காலமாக வசிக்கிறீர்கள்?",
          migrant = "Are you an in-migrant?\nநீங்கள் உள்-குடியேறியவரா?" ,
          edu = "What is your highest level of education?\nஉங்கள் மிக உயர்ந்த கல்வித் தகுதி என்ன?" ,
          current_edu = "What are you currently pursuing?\nநீங்கள் தற்போது என்ன படித்துக் கொண்டிருக்கிறீர்கள்?" ,
          occupation = "Occupational Status\nதொழில் நிலை" , 
          living_sit = "Current Living Situation \nதற்போதைய வசிப்பு நிலை" , 
          hoh_relation = "Who is the Head of your Household?\nஉங்கள் குடும்பத்தின் தலைவர் யார்?"    ,
          hoh_edu = "What is the highest qualification of the Head of the Household?\nகுடும்பத் தலைவரின் மிக உயர்ந்த கல்வித் தகுதி என்ன?" ,
          hoh_occu = "What is the current occupation of the head of the household?\nகுடும்பத் தலைவரின் தற்போதைய தொழில் என்ன?",
          fam_income = "What is current family income ?\nதற்போதைய குடும்ப வருமானம் என்ன?" ,
          religon =  "Religon \nசமயம்/மதம்"    , 
          ethnicity = "Ethnicity\nஇனம்/இனக்குழு", 
          mother_tongue = "Mother Tongue\nதாய்மொழி"  , 
          upbringing = "Describe the place you where born and brought up in \nநீங்கள் பிறந்து வளர்ந்த இடத்தைப் பற்றி விவரிக்கவும்?" , 
          fam_structure = "How would you describe your family structure?\nஉங்கள் குடும்ப அமைப்பை எவ்வாறு விவரிப்பீர்கள்?" , 
          household_members = "How many members are there your household ?\nஉங்கள் வீட்டில் எத்தனை பேர் வசிக்கிறார்கள்?", 
          birth_order = "What is your birth order ?\nஉங்கள் பிறப்பு வரிசை என்ன?" , 
          ho_mental_illness = "Do you have any history of mental illness or psychiatric institutionalization ?\nஉங்களுக்கு மனநோய் அல்லது மனநல மருத்துவமனையில் சேர்க்கப்பட்ட வரலாறு ஏதேனும் உண்டா?" ,
          smoking = "Do you smoke?\nநீங்கள் புகை பிடிக்கிறீர்களா?" , 
          alcohol = "Do you consume alcohol?\nநீங்கள் மது அருந்துகிறீர்களா?", 
          substance_use = "Do you any history of using psychoactive substances other than alcohol and smoking?\nமது மற்றும் புகைப்பழக்கத்தைத் தவிர வேறு மனதை பாதிக்கும் பொருட்களைப் (psychoactive substances) பயன்படுத்திய வரலாறு உங்களுக்கு உண்டா?",
          physical_disability = "Do you have a history of any physical disabilities ?\nஉங்களுக்கு ஏதேனும் உடல் குறைபாடுகள் (physical disabilities) இருந்த வரலாறு உண்டா?"   , 
          chronic_illness = "Do you have any chronic illnesses or chronic pain?\nஉங்களுக்கு ஏதேனும் நாள்பட்ட நோய்கள் (chronic illnesses) அல்லது நாள்பட்ட வலி (chronic pain) உள்ளதா?"   ,
          relationship = "What is current relationship status?\nஉங்கள் தற்போதைய உறவு நிலை என்ன?"  ,
          orientation = "Sexual Orientation \nபாலியல் நாட்டம்" , 
          field_edu = "Field of Current Study\nதற்போதைய படிப்புத் துறை"  
  ) %>% 
  select(-c( edu, "Name of your University/College\nஉங்கள் பல்கலைக்கழகம்/கல்லூரியின் பெயர்"))


demographic = demographic %>%
  mutate(across(where(is.character), ~str_remove_all(., "[\u0B80-\u0BFF]")))


demographic <- demographic %>%
  mutate(
    temp_col = str_squish(str_replace_all(str_to_lower(field_edu), "\\.", "")),
    field_edu = case_when(
      str_detect(temp_col, "computer|\\bcs\\b|data science") ~ "Computer Science",
      str_detect(temp_col, "statistics|biostatistics|bcom|b com") ~ "Statistics",
      str_detect(temp_col, "public health|mph|research") ~ "Public Health",
      str_detect(temp_col, "baslp|aslp|audiology|allied health|health science|science") ~ "Allied Health",
      str_detect(temp_col, "counselling|councelling|counseling") ~ "Counselling Psychology",
      str_detect(temp_col, "psychology|pychology|\\bbsc\\b") ~ "Psychology",
      TRUE ~ str_to_title(field_edu)
    )
  ) %>%
  select(-temp_col)

demographic <- demographic %>%
  mutate(
    age = floor(interval(as.Date(dob, format = "%d/%m/%Y"), as.Date("2025-11-29")) / years(1)),
    age = case_when(
      age == 0  ~ as.numeric(names(which.max(table(age[age > 0])))), 
      age == 17 ~ 18,
      TRUE      ~ age
    )
  )
    
demographic <- demographic %>%
  mutate ( orientation = case_when(orientation == "..." ~ names(which.max(table(orientation))),
                                   TRUE ~ orientation))

library(dplyr)

demographic <- demographic %>%
  mutate(
    hoh_edu = case_when(
      grepl("Profession or honours", hoh_edu) ~ 7,
      grepl("Graduate", hoh_edu) ~ 6,
      grepl("Intermediate", hoh_edu) ~ 5,
      grepl("High school", hoh_edu) ~ 4,
      grepl("Middle school", hoh_edu) ~ 3,
      grepl("Primary school", hoh_edu) ~ 2,
    ),

    hoh_occu = case_when(
      grepl("Legislators", hoh_occu) ~ 10, 
      grepl("Professionals", hoh_occu)  ~ 9,
      grepl("Technicians", hoh_occu) ~ 8,
      grepl("Clerks", hoh_occu)  ~ 7,
      grepl("Skilled workers", hoh_occu) ~ 6,
      grepl("Skilled agricultural", hoh_occu)  ~ 5,
      grepl("Craft", hoh_occu)  ~ 4,
      grepl("Plant and machine", hoh_occu) ~ 3,
      grepl("Elementary", hoh_occu)~ 2,
      grepl("Unemployed|Retired", hoh_occu)   ~ 1, 
    ),

    fam_income = case_when(
      grepl("More than 2 lakhs", fam_income)        ~ 12,
      grepl("1 lakh to 2 lakhs", fam_income)       ~ 10,
      grepl("80 thousand to 1 lakh", fam_income)   ~ 6,
      grepl("50 thousand to 80 thousand", fam_income) ~ 4,
      grepl("30 thousand to 50 thousand", fam_income) ~ 3,
      grepl("10 thousand to 30 thousand", fam_income) ~ 2,
      grepl("Less than 10 thousand", fam_income)      ~ 1,
      TRUE ~ 1
    ),
    ses_total = hoh_edu + hoh_occu + fam_income,
    ses_class = case_when(
      ses_total >= 26 ~ "Upper (I)",
      ses_total >= 16 ~ "Upper Middle (II)",
      ses_total >= 11 ~ "Lower Middle (III)",
      ses_total >= 5  ~ "Upper Lower (IV)",
      TRUE            ~ "Lower (V)"
    )
  )


write_csv(demographic, "~/tropical-monsoon-gema/data_wrangling/demographic/demographic_cleaned.csv")
