# Inspired by Matthew Hirschey's Twitter post in 2020
# https://x.com/matthewhirschey/status/1235948462673260544

library(rvest)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(cowplot)
library(ggrepel)

## read gene2pubmed from NCBI
# association of genes with publications
gene2pubmedurl <- "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene2pubmed.gz" 
gene2pubmed_raw <- read_tsv( gene2pubmedurl, col_names = TRUE) %>%
  clean_names() 

## read gene annotations (since gene2pubmed includes only gene numbers, not gene names)
# obtained from: https://ftp.ncbi.nlm.nih.gov/gene/DATA/GENE_INFO/Mammalia/
gene_info <- read_tsv("/Users/nikolasvankeersbilck/Downloads/Homo_sapiens.gene_info")

## filter for just the necessary columns
gene_info_filt <- gene_info %>%
  select("GeneID", "Symbol", "type_of_gene")

## keep just human genes, sort, and count
gene2pubmed <- gene2pubmed_raw %>%
  filter(number_tax_id == 9606) %>% #only the rows corresponding to humans
    group_by(gene_id) %>%
    count(sort = TRUE) 

## merge these two dataframes
gene_joined <- left_join(gene2pubmed, gene_info_filt, by=c("gene_id" = "GeneID"))

## filter out those with NA for symbol and those that are not protein-coding
gene_joined_filt <- gene_joined %>%
    filter(!is.na(Symbol)) %>%
    filter(type_of_gene == "protein-coding")

## build plot
plot <- ggplot() +
  geom_point(data = gene_joined_filt, mapping = aes(x = fct_reorder(Symbol, n), y = n), alpha = 0.2) +
  geom_text_repel(data = subset(gene_joined_filt, n > 3000), mapping = aes(x = fct_reorder(Symbol, n), y = n, label = Symbol), max.overlaps = Inf) +
  labs(x = "Human Gene", y = "Number of Publications", title = "Number of Publications per human gene (protein coding)") + 
  theme_cowplot() + 
  theme(axis.text.x= element_blank(), axis.ticks.x= element_blank()) +
  coord_cartesian(xlim = c(0, 20000))

plot
