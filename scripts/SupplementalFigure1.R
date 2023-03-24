# this script summaries the number of GISAID samples in the first 6 months of the pandemic by Caribbean country

#load libraries
library(tidyverse)
library(ggtree)
library(treeio) #load nwk files

#load datasets
tree <- read.newick("data/Maximum_Likelihood_Phylogenetic_Tree/tree.nwk")
metadata <- read.csv("data/Maximum_Likelihood_Phylogenetic_Tree/20221222_Tree_Metadata_for_Figure.csv")

# add column for opacity based on region 
metadata <- metadata %>% 
  mutate(alpha = ifelse(region == "Caribbean", "high", "low"))

# set region colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#D55E00", "#d61e6e")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania","Dominican Republic", "Caribbean")

options(ignore.negative.edge=TRUE)

#plot tree of all nwk tree and metadata
tree_figures_labeled <- ggtree(tree, color = "#d4d4d4", size = 0.1) %<+% 
  metadata+
  geom_tippoint(aes(color = region, alpha = alpha), size=1.25)+
  scale_color_manual(values = region_colors)+
  scale_alpha_discrete(range = c(0.95, 0.4))+
  #add clade labels
  geom_cladelab(node = 8042, label="Fig 2A")+
  geom_hilight(node = 8042, fill="yellow", alpha=.6)+
  
  geom_cladelab(node = 7857, label="Fig 2B")+
  geom_hilight(node = 7857, fill="yellow", alpha=.6)+
  
  geom_cladelab(node = 8546, label="Fig 3A")+
  geom_hilight(node = 8546, fill="yellow", alpha=.6)+
  
  geom_cladelab(node = 7494, label="Fig 3B")+
  geom_hilight(node = 7494, fill="yellow", alpha=.6)+
  
  geom_cladelab(node = 7337, label="Fig 3C")+
  geom_hilight(node = 7337, fill="yellow", alpha=.6)+
  
  geom_cladelab(node = 8603, label="Fig 3D")+
  geom_hilight(node = 8603, fill="yellow", alpha=.6)+
  
  guides(color=guide_legend(title="Region"))+ #change legend title
  theme(legend.position="right")




