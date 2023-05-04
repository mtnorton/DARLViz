# From Roe 2021
# Started 3/27/23
# by Michael Norton

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(RColorBrewer)
library(stringr)

setwd('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/data')
df <- read_csv('roe2021.csv')
df <- df %>% filter(df$`Mitigation measure...4`=="Cost-effective")
df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=="Reduce peatland degradation and conversion")] <- "Reduce peatland degradation"
df <- df %>% mutate(`Mitigation measure...3` = tolower(`Mitigation measure...3`))
df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=='beccs')] <- 'bioenergy - BECCS'
#df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=='afforestation and reforestation')] <- 'afforestation and reforestation'
df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=='grassland and savanna fire mgmt')] <- 'grassland fire mgmt'
df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=='reduce mangrove loss')] <- 'reduce mangrove conversion'
df$`Mitigation measure...3`[which(df$`Mitigation measure...3`=='coastal wetland (mangrove) restoration')] <- 'mangrove restoration'



df$`Mitigation measure...3` <- factor(df$`Mitigation measure...3`,levels=c(
  'enteric fermentation',
  'manure management',
  'rice cultivation',
  'nutrient management',
  'soil carbon croplands',
  'soil carbon grasslands',
  'agroforestry',
  'biochar',
  'bioenergy - BECCS',
  'food waste',
  'healthy diets',
  'clean cookstoves',
  'forest management',
  'grassland fire mgmt',
  'reduce deforestation',
  'reduce peatland degradation',
  'reduce mangrove conversion',
  'afforestation and reforestation',
  'peatland restoration',
  'mangrove restoration'
))

df$`Mitigation classification` <- str_wrap(df$`Mitigation classification`,width=10)

ggplot(data=df,aes(x=`Mitigation classification`,y=Global,fill=factor(`Mitigation measure...3`))) +
  theme(legend.title= element_blank()) +
  xlab('') +
  ylab(bquote('Mt'~CO[2]~ 'e'~year^-1)) +
  scale_fill_manual(values=c('#393c60','#99005a','#9d82b5','#b923cf','#fcb39d','#b57968','#e28ebc',
                       '#c7a2b7', '#266c6c', '#2b3d43', '#7a7a7a', '#dbdbdb', '#91deb3', '#e3f2c9', 
                       '#90bc71', '#65a286', '#46bf66', '#3e7d32', '#3eb4b6', '#69edc6')) +
  theme(panel.background = element_blank()) +
  geom_bar(stat='identity',position='stack') +
  coord_cartesian(clip = "off") +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.02), y = c(0, 0.13))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0, 0))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.13, 0.13))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.02), y = c(0.16, 0.38))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.16, 0.16))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.38, 0.38))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.02), y = c(0.41, 0.53))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.41, 0.41))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.53, 0.53))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.02), y = c(0.56, 0.78))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.56, 0.56))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.78, 0.78))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.02), y = c(0.81, 0.97))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.81, 0.81))) +
  annotation_custom(grid::linesGrob(x = c(1.02, 1.06), y = c(0.97, 0.97))) 
