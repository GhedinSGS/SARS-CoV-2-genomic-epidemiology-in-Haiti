# This figure makes a pie chart of number of samples by nextstrain clade in the Caribbean and Globally 

#load libraries
library(tidyverse)
library(cowplot)

#load data with lineages from ncov results
raw_data <- read_tsv("data/Maximum_Likelihood_Phylogenetic_Tree/metadata_with_nextclade_qc.tsv")

# Data Wrangling 
# Calculate percent of each clade or lineage for the global dataset 
data_percentages_by_pango_clade <- raw_data %>% 
  #count number of samples per pangolin lineage
  group_by(Nextclade_pango) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  #remove unclassified clades
  filter(Nextclade_pango != "?")

data_percentages_by_nextstrain_clade <- raw_data %>% 
  # count number of samples per nextstrain clade
  group_by(Nextstrain_clade) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  #remove unclassified samples
  filter(Nextstrain_clade != "?")

# Compute percentages of samples per nextstrain clade
data_percentages_by_nextstrain_clade$fraction = data_percentages_by_nextstrain_clade$count / sum(data_percentages_by_nextstrain_clade$count)

# Compute the cumulative percentages (top of each rectangle)
data_percentages_by_nextstrain_clade$ymax = cumsum(data_percentages_by_nextstrain_clade$fraction)

# Compute the bottom of each rectangle
data_percentages_by_nextstrain_clade$ymin = c(0, head(data_percentages_by_nextstrain_clade$ymax, n=-1))

# Calculate percent of each clade or lineage for the Caribbean dataset 
# percent of samples per pangolin lineage
data_percentages_by_pango_clade_Caribbean <- raw_data %>% 
  # keep only Caribbean samples
  filter(region == "Caribbean") %>% 
  #count number of samples of each pangolin lineage
  group_by(Nextclade_pango) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  #remove unclassified samples
  filter(Nextclade_pango != "?")

#percent of samples per nextstrain clade in Caribbean samples
data_percentages_by_nextstrain_clade_Caribbean <- raw_data %>%
  # keep only Caribbean samples
  filter(region == "Caribbean") %>% 
  #count the number of samples of each nextstrain clade
  group_by(Nextstrain_clade) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  # remove unclassified samples
  filter(Nextstrain_clade != "?")

# Compute percentages
data_percentages_by_nextstrain_clade_Caribbean$fraction = data_percentages_by_nextstrain_clade_Caribbean$count / sum(data_percentages_by_nextstrain_clade_Caribbean$count)

# Compute the cumulative percentages (top of each rectangle)
data_percentages_by_nextstrain_clade_Caribbean$ymax = cumsum(data_percentages_by_nextstrain_clade_Caribbean$fraction)

# Compute the bottom of each rectangle
data_percentages_by_nextstrain_clade_Caribbean$ymin = c(0, head(data_percentages_by_nextstrain_clade_Caribbean$ymax, n=-1))
  
# FIGURES ----------

#donut plot of nextstrain clade for all global samples
World_nextstrain_clade_donut <- ggplot(data_percentages_by_nextstrain_clade, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Nextstrain_clade)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
  theme_void()

#donut plot of nextstrain clades for all Caribbean samples
Caribbean_nextstrain_clade_donut <- ggplot(data_percentages_by_nextstrain_clade_Caribbean, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Nextstrain_clade)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
  theme_void()

#legend
legend <- cowplot::get_legend(
  World_nextstrain_clade <- ggplot(data_percentages_by_nextstrain_clade, aes(x="", y=count, fill=Nextstrain_clade)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme(legend.position="top")+
    guides(fill=guide_legend(title="Nextstrain Clade"))
  #theme_void() # remove background, grid, numeric labels
)

#final plot layout
# lay out the global and Caribbean pie charts next to each other
final_layout <- cowplot::plot_grid(World_nextstrain_clade_donut + theme(legend.position = "none"),
                                   Caribbean_nextstrain_clade_donut + theme(legend.position="none"),
                                   rel_widths=c(2,1), 
                                   labels=c("World", "Caribbean")
                                   )

#add the legend to the pie charts 
final_layout <- cowplot::plot_grid(final_layout, legend, 
                                   nrow = 2, rel_heights = c(6,1))

