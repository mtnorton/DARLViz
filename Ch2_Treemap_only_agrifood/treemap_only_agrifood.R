# Treemap of energy share
# Started 3/23/23
# by Michael Norton

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(readr)
library(ggpubr)
library(treemapify)


# Two-paned plot of GHG emissions from energy on farms
# 3/23/23

setwd('C:/Users/wb383351/OneDrive - WBG/Documents/R-projects/DARLViz/Ch2_Treemap_only_agrifood')

df <- read_csv('Emissions_Totals_E_All_Data_NOFLAG2.csv')
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

# ---------------------------

# New code from here

data <- meanEmissions_byCat$total

group <- c(2,1,1,1,3,3,3,3,3,3,1,2,1,1,1,1,3)
subgroups <- c("Farm-gate emissions","Land-use change","Pre- and post-production")
subgroup <- subgroups[group]
title <- as.character(meanEmissions_byCat$Item)

value <- data
data <- data.frame(group,subgroup,title,value)
#write.csv(data,'C:/Users/wb383351/OneDrive - WBG/Documents/R-projects/DARLViz/data/treemapdata.csv',row.names=FALSE)
pal <- c('#fff5f0','#fee0d2','#fcbba1','#fc9272',
         '#fb6a4a','#ef3b2c','#cb181d','#99000d','#a1d99b','#31a354',
         '#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594')


percs <- data$value/sum(data$value) 
percs <- paste0(round(percs,3)*100,"%")
data <- data.frame(group,subgroup,title,value,percs)
data$title <- c(as.character(meanEmissions_byCat$Item))
data$title <- factor(data$title,levels = c("All categories",cats))


ggplot(data,aes(area = value, fill = title, subgroup=subgroup,
                subgroup2 = title,label=str_wrap(title,8)))+
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#333333", size = 5) +
  geom_treemap_subgroup_text(place = "bottomleft", grow = TRUE,
                             alpha = 0.75, colour = 'black',
                             fontface=4,start = "bottomleft",
                             padding.x = grid::unit(2, "mm"),
                             padding.y = grid::unit(4, "mm"),angle=90) +
  
  geom_treemap_text(colour = "black", place = "topright",
                    size = 8, grow = FALSE,fontface="italic",
                    padding.x = grid::unit(2, "mm"),
                    padding.y = grid::unit(2, "mm")) +
  
  theme(legend.title= element_blank(),legend.position="none") + 
  scale_fill_manual(values=pal)

#--------------------------------------------

# All treemap code - replaced above

# Create data
group <- c("Non-agricultural emissions",rep("Agricultural Emissions",17))
subgroup <- c(4,2,1,1,1,3,3,3,3,3,3,1,2,1,1,1,1,3)
subgroups <- c("Farm-gate emissions","Land-use change","Pre- and post-production")
subgroup <- subgroups[subgroup]
title <- c("Non-farm emissions",as.character(meanEmissions_byCat$Item))
  #c("Total",rep("Farm-gate emissions",8),rep("Land-use change",2),rep("Pre- and post-production",7))
value <- data
data <- data.frame(group,subgroup,title,value)
#write.csv(data,'C:/Users/wb383351/OneDrive - WBG/Documents/R-projects/DARLViz/data/treemapdata.csv',row.names=FALSE)
pal <- c('#999999','#fff5f0','#fee0d2','#fcbba1','#fc9272',
        '#fb6a4a','#ef3b2c','#cb181d','#99000d','#a1d99b','#31a354',
        '#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594')
# Custom labels:
treemap(data, index=c("group","subgroup","title"),
        vSize="value", type="index",
        title="Mean emissions 2018-2020 by category",
        vColor="subgroup",palette=pal)


percs <- data$value/sum(data$value) 
percs <- paste0(round(percs,3)*100,"%")
data <- data.frame(group,subgroup,title,value,percs)
data$subgroup[1] <- "Non-food emissions"
data$title <- c("All categories",as.character(meanEmissions_byCat$Item))
data$title <- factor(data$title,levels = c("All categories",cats))


ggplot(data,aes(area = value, fill = title,
                subgroup = subgroup,subgroup2=title,label=str_wrap(title,8)))+
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#333333", size = 5) +
  geom_treemap_subgroup_text(place = "bottomleft", grow = TRUE,
                             alpha = 0.75, colour = 'black',
                             fontface=4,start = "bottomleft",
                             padding.x = grid::unit(2, "mm"),
                             padding.y = grid::unit(4, "mm"),angle=90) +

  geom_treemap_text(colour = "black", place = "topright",
                    size = 8, grow = FALSE,fontface="italic",
                    padding.x = grid::unit(2, "mm"),
                    padding.y = grid::unit(2, "mm")) +
  
  theme(legend.title= element_blank(),legend.position="none") + 
  scale_fill_manual(values=pal)

data$title2 <- paste0(data$title," (",data$percs,")")

ggplot(data,aes(area = value, fill = title,
                subgroup = subgroup,subgroup2=title,label=str_wrap(title2,15)))+
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#333333", size = 5) +
  geom_treemap_subgroup_text(place = "bottomleft", grow = TRUE,
                             alpha = 0.75, colour = 'black',
                             fontface=4,start = "bottomleft",
                             padding.x = grid::unit(2, "mm"),
                             padding.y = grid::unit(4, "mm"),angle=90) +
  
  theme(legend.title= element_blank()) + 
  scale_fill_manual(values=pal)



