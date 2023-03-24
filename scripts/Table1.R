# goal to make table 1 from patient data

# load libraries
library(tidyverse)

# load patient data
data <- read.csv("data/Haiti_Data_with_Lineages_cleaned.csv")
GISAID_ID <- read.csv("data/Samplename_GisaidID.csv")

# join gisaid ID with sample data
table_1 <- left_join(data %>% select(sample_name, Pangolin_Lineage, Date_of_specimen_collection),
                     GISAID_ID %>% rename(sample_name = Sample.ID)) %>% 
  rename(sample_collection_date = Date_of_specimen_collection)

table_1$sample_collection_date <- as.Date(table_1$sample_collection_date, format = "%d/%m/%Y")

