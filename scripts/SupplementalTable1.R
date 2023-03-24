# GOAL: I downloaded all the date/location data from high quality, human SARS-CoV-2 samples on GISAID from the early pandemic. 
# This script will load them all together, bind them, then summarise by country / region 

#load libraries
library(tidyverse)

#load list of all the GISAID TSV files
date_location_files <- list.files(path = "./data/EarlyPandemic_Date_Location_GISAID_20230224")

#create empty data frame for date/location information
date_location_df <- data.frame("Accession_ID" = as.character(), "Collection_date" = as.character(), "Submission_date" = as.character(), "Location" = as.character())

for (file in date_location_files){
  #read in each file
  df_temp <- read_tsv(paste0("data/EarlyPandemic_Date_Location_GISAID_20230224/", file))
  
  #rename columns so there are no spaces
  df_temp <- df_temp %>% 
    rename("Accession_ID" = "Accession ID") %>% 
    rename("Collection_date" = "Collection date") %>% 
    rename("Submission_date" = "Submission date")
  
  #add dataframe to growing dataframe of all data
  date_location_df <- rbind(date_location_df, df_temp)
  
  #remove temp dataframe
  rm(df_temp)
}

#caribbean countries
caribbean_countries_list <- c("Anguilla", "Antigua and Barbuda", "Barbados", "Bermuda", "Cayman Islands", "Cuba", "Dominican Republic", "Guadeloupe", "Haiti",
                              "Jamaica", "Martinique", "Puerto Rico", "Saint Barthelemy", "Saint Lucia" , "Saint Martin", "Saint Vincent and the Grenadines" , 
                              "Sint Eustatius", "Sint Maarten", "The Bahamas", "U.S. Virgin Islands", "Trinidad and Tobago", "Aruba", "Curacao", "Bonaire")


#update location information
date_location_df <- date_location_df %>% 
  separate(col = Location, into = c("region", "country", "division", "other"), sep ="/") %>% 
  #remove spaces from country and region names
  mutate(country = str_trim(country)) %>% 
  mutate(region = str_trim(region)) %>% 
  mutate(region = ifelse(country %in% caribbean_countries_list, "Caribbean", region))

#group_by_country in Caribbean
supp_table_1_a <- date_location_df %>% 
  filter(region == "Caribbean") %>% 
  group_by(country) %>% 
  summarise("number of sequences in GISAID" = n()) %>% 
  ungroup()


#number of high quality caribbean samples 
nrow(date_location_df %>% filter(region == "Caribbean"))
  #[1] 173
