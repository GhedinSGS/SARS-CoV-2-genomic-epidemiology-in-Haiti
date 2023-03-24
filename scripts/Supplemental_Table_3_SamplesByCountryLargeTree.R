# goal is to summarise number of samples included in analysis per country after ncov QC was completed 

#load libraries
library(tidyverse)

# raw metadata is from the results of the ncov workflow 
metadata_raw <- read_tsv("data/Maximum_Likelihood_Phylogenetic_Tree/metadata_with_nextclade_qc.tsv")

# group by country and count samples
metadata_by_country <- metadata_raw %>% 
  group_by(country) %>% 
  summarise("number of samples" = n()) %>% 
  ungroup()