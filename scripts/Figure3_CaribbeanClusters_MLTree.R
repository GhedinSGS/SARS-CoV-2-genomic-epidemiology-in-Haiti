# this figure make visualizations of the small clusters for figure 3

#load libraries
library(tidyverse)
library(ggtree)
library(treeio) #load nwk files
library(cowplot)
library(ape)

#load datasets
tree <- read.newick("data/Maximum_Likelihood_Phylogenetic_Tree/tree.nwk")
metadata_raw <- read.csv("data/Maximum_Likelihood_Phylogenetic_Tree/20221222_Tree_Metadata_for_Figure.csv")

#format metadata
metadata <- metadata_raw %>% 
  #add alpha for graphic purposes
  mutate(alpha = ifelse(region == "Caribbean", "high", "low")) %>% 
  #create more descriptive name for strains
  mutate(gisaid_epi_isl = ifelse(grepl("Haiti_4*", strain) == TRUE, strain, gisaid_epi_isl)) %>% 
  mutate(strain_renamed = paste(country, sep="/", gisaid_epi_isl, date)) %>% 
  mutate(strain_renamed = gsub("-", "", strain_renamed)) %>% 
  mutate(strain_renamed = gsub("/NA", "", strain_renamed)) %>% 
  mutate(strain_renamed = gsub("EPI_ISL_", "", strain_renamed)) %>% 
  mutate(strain_renamed = gsub("/Haiti_4", "/4", strain_renamed)) %>% 
  #rename haiti divisions into full words from abbreviations
  mutate(division = case_when(
    division == "ART" ~ "Artibonite", 
    division == "OST" ~ "Ouest", 
    division == "CEN" ~ "Centre", 
    division == "NRO" ~ "Nord-Ouest", 
    division == "SDE" | division == "Sud Est" ~ "Sud-Est", 
    division == "GRA" ~ "Grand'Anse", 
    division == "SUD" ~ "Sud",
    TRUE ~ "NA"
  ))


#rename tree tip labels do DR is written out
tree_renamed <- rename_taxa(tree = tree, data = metadata, key = strain, value = strain_renamed)

#adjust metadata so the strain name is correct 
metadata <- metadata %>% select(strain_renamed, date, region, country, division, alpha) %>% rename(strain = strain_renamed)

# set region colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#D55E00", "#d61e6e")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania","Dominican Republic", "Caribbean")

#find parent node to the clusters identified from taxonium
parent_node_1 <- getMRCA(tree_renamed, c("USA/2885145/20200506", "USA/1015435/20200626"))
parent_node_2 <- getMRCA(tree_renamed, c("Italy/525555/20200407", "Haiti/2274029/20200515"))
parent_node_3 <- getMRCA(tree_renamed, c("Haiti/49230084/20200521", "USA/507487/20200406"))
parent_node_4 <- getMRCA(tree_renamed, c("Dominican Republic/525468/20200506", "United Kingdom/500003/20200520"))

# --- FIGURES --------
# 4 total figures for 4 separate clades

# cluster 1 tree figure
cluster_1_tree <- tree_subset(tree_renamed, node = parent_node_1, levels_back=0) %>% 
  ggtree(color = "#4d4d4d", size = 0.5) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+
  geom_tippoint(aes(color = region), size=2)+
  scale_color_manual(values = region_colors)+
  hexpand(.4)

#cluster 2 tree figure
cluster_2_tree <- tree_subset(tree_renamed, node = parent_node_2, levels_back=0) %>% 
  ggtree(color = "#4d4d4d", size = 0.5) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+
  geom_tippoint(aes(color = region), size=2)+
  scale_color_manual(values = region_colors)+
  hexpand(.4)

#cluster 3 tree figure
cluster_3_tree <- tree_subset(tree_renamed, node = parent_node_3, levels_back=0) %>% 
  ggtree(color = "#4d4d4d", size = 0.5) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+
  geom_tippoint(aes(color = region), size=2)+
  scale_color_manual(values = region_colors)+
  hexpand(.4)

#collapse homogeneous clades without Caribbean Islands samples 
cluster_3_pruned_tree <- cluster_3_tree %>% 
  collapse(node=33)+
  geom_point2(aes(subset=(node == 33)), shape = 24, size = 3, fill = "#d49e26") #north america clade

cluster_3_pruned_tree <- cluster_3_pruned_tree %>% 
  collapse(node=28)+
  geom_point2(aes(subset=(node == 28)), shape = 24, size = 3, fill = "#d49e26") #north america clade

# cluster 4 tree 
cluster_4_tree <- tree_subset(tree_renamed, node = parent_node_4, levels_back=0) %>% 
  ggtree(color = "#4d4d4d", size = 0.5) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+
  geom_tippoint(aes(color = region), size=2)+
  scale_color_manual(values = region_colors)+
  hexpand(.4)

# figure legend
legend <- cowplot::get_legend(
  ggtree(tree_renamed, color = "#4d4d4d", size = 0.1) %<+% 
    metadata+
    geom_tiplab(size=3)+
    geom_tippoint(aes(color = region), size=2)+
    scale_color_manual(values = region_colors)
)

#layout all 4 tree clusters 
tree_layout <- cowplot::plot_grid(cluster_1_tree + theme(legend.position="none"), 
                                  cluster_2_tree + theme(legend.position="none"), 
                                  cluster_3_tree + theme(legend.position="none"), 
                                  cluster_4_tree + theme(legend.position="none"), 
                                  nrow=2, ncol=2, 
                                  labels="AUTO")
tree_layout <- cowplot::plot_grid(tree_layout, legend, ncol=2, rel_widths=c(8,1))

