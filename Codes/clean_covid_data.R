# Clear the memory
rm(list=ls())

# Define the path and the date
my_path <- "/home/ozzy/Documents/CEU/ECBS-5208-Coding-1-Business-Analytics/Ozan/Homework/Data/"
date <- '07-10-2020' 

# Preamble
library(tidyverse)

# covid data
cv <- read_csv(paste0(my_path,"Raw/covid_", date,"_raw.csv"))
# population data
pop <- read_csv(paste0(my_path,'Raw/pop_WDI_2019.csv'))

####
# COVID DATA CLEANING
#
# Check covid data
glimpse( cv )

# Drop not needed variables
cv <- cv %>% select( -c( FIPS,Admin2,Last_Update,Lat,Long_,Combined_Key,Incidence_Rate,Case.Fatality_Ratio))

# One observation to be one country
length(levels(as.factor(cv$Country_Region)))
# Check e.g. China:
cv %>% filter( Country_Region == 'China')

# Create new data table now only contains the countries
cv2 <- cv %>% 
  group_by( Country_Region ) %>% 
  summarise_if(is.numeric,lst( sum ) )

# Finak check for one observation to be one country per row 
nrow(cv2) == length(levels(as.factor(cv$Country_Region)))

# Rename variables
cv2 <- cv2 %>% rename( Country   = Country_Region ,
                       Confirmed = Confirmed_sum,
                       Death     = Deaths_sum,
                       Recovered = Recovered_sum,
                       Active    = Active_sum )

glimpse(cv2)
####
# Clean population data
#

## Check the observations:
glimpse(pop)
# 1) Filter out grouping observations based on using digits
pop <- pop %>% filter( !grepl("[[:digit:]]", pop$iso2c) )

levels(as.factor(pop$country))
# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe
pop %>% filter(iso2c == "OE")
pop <- pop %>% filter(country != "OECD members")
pop %>% filter(iso2c == "HK")
pop %>% filter(iso2c == "CN")
cv2 %>% filter(Country == "China")
pop[which(pop$iso2c == "CN"), 3] <- (pop %>% filter(iso2c == "CN"))[3] +  (pop %>% filter(iso2c == "HK"))[3]
pop <- pop %>% filter(country != "Hong Kong SAR, China")
to_be_discarded <- pop %>% filter(str_detect(iso2c, "X")) %>% filter(!(country %in% c("Kosovo", "Mexico",
                                                                                      "Sint Maarten (Dutch part)")))
pop <- pop %>% filter(!(country %in% to_be_discarded$country))

to_be_discarded <- pop %>% filter(str_detect(iso2c, "Z")) %>% filter(!(country %in% c("Zimbabwe", "Zambia",
                                                                                      "Uzbekistan", "Tanzania", 
                                                                                      "South Africa", "New Zealand",
                                                                                      "Mozambique", "Kazakhstan", 
                                                                                      "Eswatini", "Czech Republic",
                                                                                      "Belize", "Azerbaijan",
                                                                                      "Algeria")))
pop <- pop %>% filter(!(country %in% to_be_discarded$country))

country_check_covid <- data.frame(country = cv2$Country)
country_check_WDI <- data.frame(country = pop$country)
country_check <- merge(country_check_WDI, country_check_covid, by = "country", all = FALSE)
cv2 %>% filter(!(Country %in% country_check$country))
pop %>% filter(!(country %in% country_check$country))

# Some manual checks after this point
pop <- pop %>% filter(country != "European Union")
pop[which(pop$country == "Bahamas, The"), 2] <- "Bahamas"
pop[which(pop$country == "Brunei Darussalam"), 2] <- "Brunei"
pop[which(pop$country == "Myanmar"), 2] <- "Burma"
pop[which(pop$country == "Congo, Rep."), 2] <- "Congo (Brazzaville)"
pop[which(pop$country == "Congo, Dem. Rep."), 2] <- "Congo (Kinshasa)"
pop[which(pop$country == "Czech Republic"), 2] <- "Czechia"
pop[which(pop$country == "Egypt, Arab Rep."), 2] <- "Egypt"
pop[which(pop$country == "Gambia, The"), 2] <- "Gambia"
pop[which(pop$country == "Iran, Islamic Rep."), 2] <- "Iran"
pop[which(pop$country == "Korea, Rep."), 2] <- "Korea, South"
pop[which(pop$country == "Kyrgyz Republic"), 2] <- "Kyrgyzstan"
pop[which(pop$country == "Lao PDR"), 2] <- "Laos"
pop[which(pop$country == "Russian Federation"), 2] <- "Russia"
pop[which(pop$country == "St. Kitts and Nevis"), 2] <- "Saint Kitts and Nevis"
pop[which(pop$country == "St. Lucia"), 2] <- "Saint Lucia"
pop[which(pop$country == "St. Vincent and the Grenadines"), 2] <- "Saint Vincent and the Grenadines"
pop[which(pop$country == "Slovak Republic"), 2] <- "Slovakia"
pop[which(pop$country == "Syrian Arab Republic"), 2] <- "Syria"
pop[which(pop$country == "United States"), 2] <- "US"
pop[which(pop$country == "Venezuela, RB"), 2] <- "Venezuela"
pop[which(pop$country == "Yemen, Rep."), 2] <- "Yemen"

country_check_covid <- data.frame(country = cv2$Country)
country_check_WDI <- data.frame(country = pop$country)
country_check <- merge(country_check_WDI, country_check_covid, by = "country", all = FALSE)
cv2 %>% filter(!(Country %in% country_check$country))
pop %>% filter(!(country %in% country_check$country))


# Retain and rename variables which are going to be used later
pop <-pop %>% transmute( Country = country,
                         Population=SP.POP.TOTL )


################
# MERGE the two data table
##


df <- full_join(cv2,pop)

# Write a for-loop to find those which are partial or complete matches!
# 1) auxillary table for countries without any population value
aux <- df %>% filter( is.na(Population) )
# 2) Get the name of the countries
countries_nm <- aux$Country
# 3) Iterate through all potential partial matches
for ( i in seq_along( countries_nm ) ){
  # Select those observations where partial match exists
  log_select <- str_detect( df$Country , countries_nm[ i ] )
  # Get the population values for partial matches
  c_partial <- df$Population[ log_select ]
  # If there is a match: only two countries are selected and one is missing the other has population:
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # Replace the missing value with the match
    df$Population[ log_select & is.na(df$Population)] = c_partial[ !is.na( c_partial ) ]
    # Remove the replaced variable
    df <- df %>% filter( !(log_select & is.na( df$Confirmed ) ) )
  }
}

# 4) Check the results:
df %>% filter( is.na(Population) )
# These are:
#   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by world bank 
#       (Western Sahara, Taiwan or Kosovo)
#   c) we have no population data on them (Ertirea, Holy See (Vatican))

#####
# Handle missing values:
df %>% filter( !complete.cases(df) ) 
# Drop if population, confirmed cases or death is missing
df <- df %>% filter( !( is.na( Population ) | is.na( Confirmed ) | is.na( Death ) ))


#####
# Save clean data
# COVID data
write_csv( df , paste0(my_path,"Clean/covid_pop_", date, "_clean.csv"))




