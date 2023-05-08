# libraries
library(tidyverse)
library(treeio)
library(ggtree)

#tree file
beast_tree <- read.beast("data/B.1.478_Tree/B.1.478.comb1-4.mcc_renamed_annotated.tree")

#metadata
metadata <- read_tsv("data/B.1.478_Tree/B.1.478_GISAID_Data.metadata.tsv")

# set region colors
location_colors <- c("#b2a3bf", "#d49e26", "#85c2e6", "#d61e6e")
names(location_colors)<-c("Canada", "DominicanRep","USA", "Haiti")


# location colors
location_colors <- c("#0072B2", "#4da890", "#D55E00", "#b2a3bf")
names(location_colors) <- c("Canada", "DominicanRep", "Haiti", "USA")

# plot tree
ggtree(beast_tree, mrsd = "2021-03-05", layout = "roundrect", aes(color = location)) + 
  theme_tree2()+
  geom_tippoint(aes(fill = location), stroke = 0.6, shape=21, color = "black")+
  scale_color_manual(values = location_colors)+
  scale_fill_manual(values = location_colors)+
  geom_tiplab(size=2.5, color = "black", hjust = -0.07)+
  geom_nodelab(aes(x=branch, label=ifelse(round(as.numeric(posterior), digits = 2) >= 0.75,
                                          round(as.numeric(posterior), digits = 2),
                                          "")), vjust=-1, size=2.5, color = "black")+
  hexpand(0.3)+
  scale_x_continuous(n.breaks=8)
  #geom_text(aes(label=node), size=4, color = "black") #node labels

ggsave("BEAST_Figures/20230502_Tree_ggtree_V2.0.pdf", height = 11, width = 11, unit = "in")

