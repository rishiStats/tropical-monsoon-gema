# procedure 

1. [Loading the  daily dataset file](loading_daily_data.R) includes code for compiling datasets across the days. [Output File](data_wrangling/daily_data.csv)
2. [Spatially referencing file](spatial_join.py) includes python code to add district and state name to the dataset. [Output File](data_wrangling/georef_daily_dataa.csv)
3. [Cleaning the daily data file](cleaning_daily_data.R) includes R code to clean the dataset. [Output File](data_wrangling/daily_data_cleaned.csv)



# required packages in R for data wrangling 

## for daily_data loading 

dplyr 

readr

## for daily_data cleaning 

lubridate 

dplyr 
