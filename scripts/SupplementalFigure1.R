# this script creates supplemental figure 1, which is a bargraph and phylogenetic tree of all samples in the maximum likelihood
# phylogenetic tree of Haiti samples on a global background

#load libraries
library(tidyverse)
library(ggtree)
library(treeio) #load nwk files
library(cowplot)

#load datasets
tree <- read.newick("data/Maximum_Likelihood_Phylogenetic_Tree/tree.nwk")
metadata <- read.csv("data/Maximum_Likelihood_Phylogenetic_Tree/20221222_Tree_Metadata_for_Figure.csv")

#adjust metadata
metadata <- metadata %>% 
  # combien central america and caribbean into 1 region
  mutate(region = ifelse(region == "Central America", "Caribbean", region))

# set region colors
region_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#4da890", "#F0E442", "#0072B2", "#D55E00")
names(region_colors)<-c("Asia", "North America","South America","Europe", "Africa", "Oceania", "Caribbean")

# clade colors
auspice_clade_colors <- c("#dddddd", "#88cdd2", "#5360F6", "#6AA0F6", "#a8e3a5", "#cfef82", "#f5e970", "#F7ce62", "#eb4e3b", "#205948", "#612F8C", "#cc69ca")
clade_colors <- c("#525252", "#F7D027", "#6BA547","#619ED6", "#E48F1B", "#B77EA3", "#E64345", "#60CEED", "#e88bc9", "#5e3280", "#d633c3", "#96a3b3")
names(clade_colors)<-c("?", "20A", "19A","20C","20B", "20D", "19B","20G", "21I (Delta)", "20E (EU1)", "20F", "21J (Delta)")

#plot tree with just colors
tree_plot <- ggtree(tree, color = "#4d4d4d", size = 0.1) %<+% 
  metadata+
# color tree tips by nextstrain clade
  geom_tippoint(aes(color = Nextstrain_clade), size=1.2)+
#change legend title
  guides(color=guide_legend(title="Nextstrain Clade"))+ 
  theme(legend.position="right")+
  scale_color_manual(values = clade_colors)+
  guides(colour = guide_legend(override.aes = list(size=6)))
  #geom_text(aes(label=node), size=1)   #add clade labels

#rotate one brach
tree_plot <- rotate(tree_plot, 7176)
#tree_plot <- rotate(tree_plot, 5546)

#create bar graph of number of samples per clade by region
bar_graph_stacked <- ggplot(metadata %>% 
                              group_by(Nextstrain_clade, region) %>% 
                              summarise(count = n()) %>% 
                              ungroup() %>% 
                              filter(Nextstrain_clade != "?"))+
  geom_col(aes(x = region, y = count, fill = Nextstrain_clade), position="stack")+
  scale_fill_manual(values = clade_colors)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust=1))+
  xlab("Region")+
  ylab("Number of Samples")+
  guides(fill=guide_legend(title="Nextstrain Clade"))

# clade legend
clade_legend <- get_legend(bar_graph_stacked)


#stacked bargraph
full_layout_stacked_bargraph <- cowplot::plot_grid(tree_plot + theme(legend.position = "none"),
                                                   bar_graph_stacked + theme(legend.position="none"),
                                                   clade_legend,
                                                   rel_widths = c(4,2,1), 
                                                   ncol = 3,
                                                   labels = c("A", "B", ""))




