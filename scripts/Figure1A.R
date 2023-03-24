# this figures makes a table of the count of Haiti samples by pangolin lineage and nextclade clade

#load libraries
library(tidyverse)

Haiti_Pango_classifications_raw <- read.csv("data/Haiti_Data_with_Lineages_cleaned.csv")
Haiti_Nextclade_classifications_raw <- read_delim("data/SGSSamples_nextclade_classification.csv", delim=";")

#rename columns between pangolin lineage data and nextclade data for joining 
Haiti_Nextclade_classifications <- Haiti_Nextclade_classifications_raw %>% 
  select(seqName, clade) %>% 
  rename("strain" = "seqName") %>% 
  rename("Nextstrain_Clade" = "clade")

Haiti_Pango_classifications <- Haiti_Pango_classifications_raw %>% 
  select(sample_name, Pangolin_Lineage) %>% 
  rename("strain" = "sample_name") %>% 
  rename("Pango_Clade" = "Pangolin_Lineage")

#join pangolin lineage and nextclade data
classification_comparison <- left_join(Haiti_Pango_classifications, Haiti_Nextclade_classifications)

#manually add nextclade lineage to unidentified samples
B1157_nextclade <- "20A"
B1111_nextclade <- "20A"
B1243_nextclade <- "20A"

#count number of samples by lineage
final_table <- classification_comparison %>% 
  #count number of samples by pangolin clade
  group_by(Pango_Clade) %>% 
  summarise(number_of_sequences = n()) %>% 
  ungroup() %>% 
  # add nextclade lineage for each pangolin lineage present
  mutate(Nextstrain_Clade = case_when(
    Pango_Clade == "A" ~ "19B",
    Pango_Clade == "B.1" ~ "20A and 20C", 
    Pango_Clade == "B.1.111" ~ "20A", 
    Pango_Clade == "B.1.143" ~ "20A", 
    Pango_Clade == "B.1.157" ~ "20A", 
    Pango_Clade == "B.1.161" ~ "20A", 
    Pango_Clade == "B.1.179" ~ "20A", 
    Pango_Clade == "B.1.220" ~ "20A", 
    Pango_Clade == "B.1.243" ~ "20A", 
    Pango_Clade == "B.1.413" ~ "20A", 
    Pango_Clade == "B.1.473" ~ "20A", 
    Pango_Clade == "B.1.478" ~ "20A", 
    Pango_Clade == "B.1.520" ~ "20A", 
    TRUE ~ as.character(Pango_Clade)),
    .before = "Pango_Clade"
  ) %>% 
  #rename columns 
  rename("Nextstrain Clade" = "Nextstrain_Clade") %>% 
  rename("Pango Clade" = "Pango_Clade") %>% 
  rename("Number of Sequences" = "number_of_sequences")

