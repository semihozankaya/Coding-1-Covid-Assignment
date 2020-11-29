# Clear the memory
rm(list=ls())

# Define the path and the date
my_path <- "/home/ozzy/Documents/CEU/ECBS-5208-Coding-1-Business-Analytics/Ozan/Homework/Data/"
date <- '07-10-2020'

# Preamble
library(WDI)
library(tidyverse)

# Download COVID cross-sectional data
date <- '07-10-2020'
covid_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
            date,'.csv')
covid_raw <- read.csv(covid_url)

# Download population data for 2019
pop_raw <- WDI(indicator=c('SP.POP.TOTL'), 
                country="all", start=2019, end=2019)

# Save covid data
write_csv(covid_raw, paste0(my_path,"Raw/covid_", date,"_raw.csv"))
# Save population data
write_csv(pop_raw, paste0(my_path,"Raw/pop_WDI_2019.csv"))


