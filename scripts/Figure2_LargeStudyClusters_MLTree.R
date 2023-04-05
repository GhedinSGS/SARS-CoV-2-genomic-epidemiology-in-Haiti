# This script creates Figure 2 from the nwk tree and associated metadata

#load libraries
library(tidyverse)
library(ggtree)
library(treeio) #load nwk files
library(cowplot)

#load datasets
tree <- read.newick("data/Maximum_Likelihood_Phylogenetic_Tree/tree.nwk")
metadata_raw <- read.csv("ata/Maximum_Likelihood_Phylogenetic_Tree/20221222_Tree_Metadata_for_Figure.csv")

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
  )) %>% 
  #identify whether samples were sequenced for this study (Y) or publicly available (N)
  mutate(ghedin_lab_yn = ifelse(grepl("Haiti_49*", strain) == TRUE, "Y", "N"))
  

#rename tree tip labels do Countries are all written out
tree_renamed <- rename_taxa(tree = tree, data = metadata, key = strain, value = strain_renamed)

metadata <- metadata %>% select(strain_renamed, date, region, country, division, alpha, ghedin_lab_yn) %>% rename(strain = strain_renamed)
  
# set division colors
division_colors <- c("#332288", "#88CCEE", "#44AA99", "#DDDDDD", "#999933", "#DDCC77", "#CC6677", "#882255")
names(division_colors)<-c("Artibonite", "Centre","Grand'Anse","NA", "Nord-Ouest", "Ouest","Sud", "Sud-Est")

#define divisions of Haiti
metadata$division <- factor(metadata$division, 
                            levels=c("Artibonite", "Centre", "Grand'Anse", "Nord-Ouest", "Ouest", "Sud", "Sud-Est", "NA"))

# tree subset of B.1 clade
B.1_Tree <- tree_subset(tree_renamed, node = 8043, levels_back=2) %>% 
  ggtree(color = "#4d4d4d", size = 0.3) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+
  geom_tippoint(aes(color = division, shape = ghedin_lab_yn), size=3)+
  scale_color_manual(values = division_colors)+
  scale_shape_manual(values=c(16, 18))+
  hexpand(.4)


#collapse homogeneous nodes that are large clusters without Haiti samples
B.1_pruned_tree <- B.1_Tree %>%
  collapse(node=96) %>% 
  collapse(node=120) %>% 
  collapse(node=103) %>% 
  collapse(node=115) %>% 
  collapse(node=127) +
  geom_point2(aes(subset=(node == 96)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=96, label="USA", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 120)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=120, label="Colombia", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 103)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=103, label="Colombia", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 115)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=115, label="USA", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 127)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=127, label="Argentina", fontsize=3, offset.text=0.00001)



## Tree for B.1.478 Lineage
B.1.478_Tree <- tree_subset(tree_renamed, node = 7845, levels_back=1) %>% 
  ggtree(color = "#4d4d4d", size = 0.3) %<+% 
  metadata+
  geom_tiplab(size=3, hjust=-0.1)+ #add text to top
  geom_tippoint(aes(color = division, shape = ghedin_lab_yn), size=3)+ #add color to top
  scale_color_manual(values = division_colors)+ #assign color based on division
  scale_shape_manual(values=c(16, 18))+
  hexpand(.4)

#collapse homogeneous nodes that are large clusters without Haiti samples
B.1.478_pruned_tree <- B.1.478_Tree %>% 
  collapse(node=208) %>% 
  collapse(node=273) %>% 
  collapse(node=179) %>% 
  collapse(node=176) %>% 
  collapse(node=260) + 
  geom_point2(aes(subset=(node == 208)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=208, label="Canada", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 273)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=273, label="Europe", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 179)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=179, label="USA", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 176)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=176, label="United Kingdome", fontsize=3, offset.text=0.00001)+
  geom_point2(aes(subset=(node == 260)), shape = 24, size = 2, fill = "#DDDDDD")+
  geom_cladelab(node=260, label="North America", fontsize=3, offset.text=0.00001)

#create a buffer for easier layout
top_buffer <- ggplot()+
  geom_rect(aes(xmin = 0, xmax = 5, ymin = 0, ymax = 2), fill="white")+theme_void()

#legend of divisions
legend <- cowplot::get_legend(
  ggtree(tree_renamed, color = "#4d4d4d", size = 0.1) %<+% 
    metadata+
    geom_tiplab()+
    geom_tippoint(aes(color = division, shape = ghedin_lab_yn))+
    scale_color_manual(values = division_colors, name="Divison")+
    scale_shape_manual(values=c(16, 18), labels = c("GISAID", "This Study"), name = "Sample Sequencing")
)

#full figure
B.1.plot <- cowplot::plot_grid(top_buffer, B.1_pruned_tree+theme(legend.position="none"), labels = c("A", ""), ncol = 1, rel_heights = c(1,20))
B.1.478.plot <- cowplot::plot_grid(top_buffer, B.1.478_pruned_tree+theme(legend.position="none"), labels = c("B", ""), ncol = 1, rel_heights = c(1,20))
full_fig <- cowplot::plot_grid(B.1.plot, B.1.478.plot, legend, nrow = 1, rel_widths = c(5,5,1))
full_fig
