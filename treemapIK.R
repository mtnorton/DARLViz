# Treemap of energy share
# Started 3/23/23
# by Michael Norton

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(readr)
library(ggpubr)

# Two-paned plot of GHG emissions from energy on farms
# 3/23/23

setwd('C:/Users/wb383351/WBG/Ilyun Koh - data')

df <- read_csv('C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/Emissions_totals/Emissions_Totals_E_All_Data_NOFLAG2.csv')
df_all <- df %>% filter(`Element Code`==723113 & Item=='All sectors with LULUCF')
meanEmissions <- df_all %>% dplyr::select(`2018`,`2019`,`2020`) %>% 
  summarize(m2018=mean(`2018`,na.rm=TRUE),m2019=mean(`2019`,na.rm=TRUE),m2020=mean(`2020`,na.rm=TRUE))
meanEmissions <- meanEmissions %>% mutate(total = rowSums(meanEmissions[,1:3])/3)

df_farmOnly <- df %>% filter(Item == "IPCC Agriculture")
meanEmissions_farmOnly <- df_farmOnly %>% dplyr::select(`2018`,`2019`,`2020`) %>% summarize(m2018=mean(`2018`,na.rm=TRUE),m2019=mean(`2019`,na.rm=TRUE),m2020=mean(`2020`,na.rm=TRUE))

cats <- c('Crop Residues','Drained organic soils','Enteric Fermentation','Manure Management','Rice Cultivation',
          'Savanna fires','Synthetic Fertilizers','On-farm electricity use','Burning - Crop residues','Net Forest conversion',
          'Fertilizers Manufacturing','Food Household Consumption','Food Packaging','Food Processing',
          'Food Retail','Food Transport','Waste - agri-food systems')

df_byCat <- df %>% filter(`Element Code`==723113 & 
                            Item %in% cats)
meanEmissions_byCat <- df_byCat %>% group_by(Item) %>% 
  dplyr::select(`2018`,`2019`,`2020`) %>% 
  summarize(m2018=mean(`2018`,na.rm=TRUE),m2019=mean(`2019`,na.rm=TRUE),m2020=mean(`2020`,na.rm=TRUE))
meanEmissions_byCat <- meanEmissions_byCat %>% mutate(total = rowSums(meanEmissions_byCat[,2:4])/3)
meanEmissions_byCat$Item <- factor(meanEmissions_byCat$Item,levels=cats)

c('On-farm energy use','On-farm electricity use',
  'Crop Residues','Rice Cultivation','Burning - Crop residues',
  'Enteric Fermentation','Manure Management',
  'Waste - agri-food systems','Fertilizers Manufacturing',
  'Food Processing', 'Food Packaging','Food Retail',
  'Food Household Consumption','Food Transport','Farm-gate emissions',
  'Land Use change','Emissions on agricultural land')

# library
library(treemap)

data <- c((meanEmissions$total-sum(meanEmissions_byCat$total)),meanEmissions_byCat$total)


# Create data
group <- c("Non-agricultural emissions",rep("Agricultural Emissions",17))
subgroup <- c(4,2,1,1,1,3,3,3,3,3,3,1,2,1,1,1,1,3)
subgroups <- c("Farm-gate emissions","Land-use change","Pre- and post-production")
subgroup <- subgroups[subgroup]
title <- c("Non-farm emissions",as.character(meanEmissions_byCat$Item))
  #c("Total",rep("Farm-gate emissions",8),rep("Land-use change",2),rep("Pre- and post-production",7))
value <- data
data <- data.frame(group,subgroup,title,value)
pal <- c('#ebebeb',
         '#ac190d','#be1c0e','#d01f10','#e32111','#ee2a1a','#f04c3e','#f36e63','#f69087','#f9b2ac','#fbd3d0','#fef5f5',
         '#22723a','#31a354','#a1d99b',
         '#0966de','#096ff1','#187af6','#3d8ff8','#63a5f9','#89bbfa','#afd1fc','#d5e7fd','#e7f1fe','#fafcff')
# Custom labels:
treemap(data, index=c("group","subgroup","title"),
        vSize="value", type="index",
        title="Mean emissions 2018-2020 by category",
        vColor="subgroup",palette=pal)

library(treemapify)

percs <- data$value/sum(data$value) 
percs <- paste0(round(percs,3)*100,"%")
data <- data.frame(group,subgroup,title,value,percs)
data$subgroup[1] <- "Non-food emissions"
data$title <- c("All categories",as.character(meanEmissions_byCat$Item))
data$title <- factor(data$title,levels = c("All categories",cats))

data$title2 <- paste0(data$title," (",data$percs,")")


data2<-read_xlsx('C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/Norton, Michael/data/treemap_IK052023.xlsx',
                sheet='Sheet1')
data2$title <- factor(data2$title,levels=c('All categories','Enteric Fermentation','Drained organic soils','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Crop Residues','Manure applied to Soils','Burning - Crop residues',
                                           'Net Forest conversion','Fires in organic soils','Fires in humid tropical forests',
                                           'Agrifood Systems Waste Disposal','Food Household Consumption','Food Retail','Food Processing','Food Transport','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Pesticides Manufacturing','On-farm Heat Use'))

data2$title2

ggplot(data2,aes(area = value, fill = title,
                subgroup = subgroup,subgroup2=title2,label=str_wrap(title2,8)))+
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#333333", size = 5) +
  #geom_treemap_subgroup_text(place = "bottomleft", grow = TRUE,
  #                           alpha = 0.75, colour = 'black',
  #                           fontface=4,start = "bottomleft",
  #                           padding.x = grid::unit(2, "mm"),
  #                           padding.y = grid::unit(4, "mm"),angle=90) +
  
  geom_treemap_text(colour = "black", place = "topright",
                    size = 10, grow = FALSE,fontface="italic",
                    padding.x = grid::unit(2, "mm"),
                    padding.y = grid::unit(2, "mm")) +
  
  theme(legend.title= element_blank(),legend.position="none") + 
  scale_fill_manual(values=pal)



