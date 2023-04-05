#this script makes a genemap of SARS-CoV-2 and adds B.1.478 lineage defining mutations

#load library
library(tidyverse)
library(gggenes)
library(RColorBrewer)
library(ggfittext)
library(ggpubr)
library(cowplot)

#load genemap of SARS-CoV-2 genome
ntpos_gene_update <- read.csv("data/GeneMap/ntpos_gene_update.csv", header = TRUE)

#create dataframe for SARS-CoV-2 genome
sc2_genome <- data.frame(molecule = as.character(), gene = as.character(), start = as.numeric(), end = as.numeric())

#list of genes based on each geneid
genes <- unique(ntpos_gene_update$gene_id) 

# format start and stop of each gene into sc2_genome dataframe
for (i in 1:length(genes)){
  current_gene <- genes[i] #select a gene
  ntpos_gene_current <- ntpos_gene_update %>% filter(gene_id == current_gene)
  current_start <- min(ntpos_gene_current$ntpos) #set start to first occurence of that gene
  current_end <- max(ntpos_gene_current$ntpos) #set end to last occurance of that gene
  
  sc2_genome[i, "gene"] <- current_gene
  sc2_genome[i, "start"] <- current_start
  sc2_genome[i, "end"] <- current_end
  sc2_genome[i, "molecule"] <- "SARS-CoV-2"
}

# additional data formatting
sc2_genome <- sc2_genome %>% 
  filter(gene != "") %>% #remove intergenic labels
  mutate(y = "1") #set y value for plotting

#simplified genome version
sc2_genome_simplified <- sc2_genome %>% 
  mutate(gene = ifelse(start >= 0 & start <= 18039, "ORF1a", 
                       ifelse(start > 18039 & start <= 21555, "ORF1b", gene))) %>% 
  group_by(gene, molecule, y) %>% 
  summarise(start = min(start), end = max(end)) %>%
  ungroup() %>% 
  mutate(end = ifelse(gene == "ORF1a", 13468, end)) %>% 
  mutate(start = ifelse(gene == "ORF1b", 13469, start))
  

#load mutations that define the B.1.478 lineage 
mutations_raw <- read.csv("B1478_Lineage_Definition/B.1.478_Lineage_Defining_Mutations.csv")

#format mutations dataframe to be compatible with sc2 genome dataframe and gggenes
mutations <- mutations_raw %>% 
  mutate(xstart=ntpos) %>% 
  mutate(xend=xstart) %>% 
  mutate(ystart = "1") %>% 
  mutate(yend = "1.0001") %>% 
  mutate(start = "0") %>% 
  mutate(end = max(ntpos))

#overlay of SC2 genome and mutations markers
ggplot() +
  geom_segment(data = mutations, aes(x = xstart, xend = xend, y = ystart, yend = yend))+
  geom_gene_arrow(data = sc2_genome_simplified, aes(xmin = start, xmax = end, y = y, label = gene, fill = gene), arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
  geom_gene_label(data = sc2_genome_simplified, aes(xmin = start, xmax = end, y = y, label = gene, fill = gene), align = "left") +
  theme_genes() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="bottom")+
  labs(x = "nucleotide position", color = "Mutation Included in Analysis")+
  annotate(geom = "text", x = 4800, y = 0.90, label = "SARS-CoV-2 Genome")
