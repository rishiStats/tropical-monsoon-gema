# procedure 

1. [Loading the  daily dataset file](data_wrangling/loading_daily_data.R) includes code for compiling datasets across the days. [Output File](data_wrangling/daily_data.csv)
2. [Spatially referencing file](data_wrangling/spatial_join.py) includes python code to add district and state name to the dataset. [Output File](data_wrangling/georef_daily_dataa.csv)
3. [Cleaning the daily data file](data_wrangling/cleaning_daily_data.R) includes R code to clean the dataset. [Output File](data_wrangling/daily_data_cleaned.csv)



# required packages in R for data wrangling 

## for daily_data loading 

dplyr 

readr

## for daily_data cleaning 

lubridate 

dplyr 
