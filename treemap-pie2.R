#051823
#pie chart for total global emissions

#create percentage function to show %
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
g_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/non_agri_food208-2020.csv")
              

g_raw$Item <- factor(g_raw$Item,                 # Relevel group factor
                     levels = c("agrifood_2018-2020","non_food_2018-2020"))


pg<-ggplot(g_raw, aes(x="", y=Value, fill=Item)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(perc )), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_discrete(breaks=c("non_food_2018-2020","agrifood_2018-2020"))+ #order in the label
  scale_fill_manual(values=c("#e0e0e0","#999999"))
pg

#barchart
pg<-ggplot(g_raw, aes(x="", y=Value, fill=Item)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity",position='stack')+
  scale_fill_discrete(breaks=c("agrifood_2018-2020","non_food_2018-2020"))+ #order in the label
  scale_fill_manual(values=c("#fbc54a","#e0e0e0"))+
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), #remove background
        panel.background = element_blank())  #https://rpubs.com/Mentors_Ubiqum/ggplot_remove_elements
pg



###########################################################
#now load agg data
#this is new ppp
d_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/newppp19902020_051923_2.xlsx",
                 sheet="Sheet1")
d_raw<-subset(d_raw, d_raw$Year>=2018)
d_raw<-d_raw%>% 
  group_by(Item, Year,Group) %>% 
  summarise(Value=sum(Value)) 

#d_raw<-subset(d_raw, d_raw$Item!="Pre- and Post- Production" & d_raw$Item!="Energy Use")

####test
d_raw3%>%
  group_by(Year,Group)%>%
  summarise(Value = sum(Value))


####fg-luc data
d_raw2<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood2016_2020.xlsx",
                 sheet="Sheet1")
d_raw2<-subset(d_raw2, d_raw2$Item!="Drained organic soils (CO2)" &d_raw2$Item!="Drained organic soils (N2O)" )
d_raw2<-subset(d_raw2, d_raw2$Year>=2018)
d_raw3<-rbind(d_raw, d_raw2)
write.csv(d_raw3, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood2016_2020_2.csv")
d_raw4<-d_raw3%>% 
  group_by(Item) %>% 
  summarise(Value=mean(Value)) %>%
  mutate(perc = percent(Value / sum(Value))) # 3 year avearge

waste<-subset(d_raw4,d_raw4$Item=="Domestic Wastewater" | d_raw4$Item=="Incineration" | d_raw4$Item=="Industrial Wastewater" | d_raw4$Item=="Solid Food Waste")

Value<-sum(waste$Value[1:4])
perc<-7.85
Item<-"Agrifood Systems Waste Disposal"
waste<-data.frame(Item,Value,perc)
d_raw4<-subset(d_raw4, d_raw4$Item!="Domestic Wastewater" & d_raw4$Item!="Incineration" & d_raw4$Item!="Industrial Wastewater" & d_raw4$Item!="Solid Food Waste")
d_raw4<-rbind(d_raw4, waste)
write.csv(d_raw4, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood2016_2020_3.csv")


#load cleaned data
#d_raw4<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood2016_2020_3.xlsx",
#                  sheet="Sheet1")

d_raw4$Item <- factor(d_raw4$Item,levels=c('Enteric Fermentation','Drained organic soils','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Crop Residues','Manure applied to Soils','Burning - Crop residues',
                                           'Net Forest conversion','Fires in organic soils','Fires in humid tropical forests',
                                           'Agrifood Systems Waste Disposal','Food Household Consumption','Food Retail','Food Processing','Food Transport','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Pesticides Manufacturing','On-farm Heat Use'))
###old plot#################################
pd<-ggplot(d_raw4, aes(x="", y=Value, fill=Item)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(round(perc,3)*100,"%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_discrete(breaks=c('Enteric Fermentation','Drained organic soils','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Crop Residues','Manure applied to Soils','Burning - Crop residues',
                                           'Net Forest conversion','Fires in organic soils','Fires in humid tropical forests',
                                           'Agrifood Systems Waste Disposal','Food Household Consumption','Food Retail','Food Processing','Food Transport','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Pesticides Manufacturing','On-farm Heat Use'))+ #order in the label
  scale_fill_manual(values=c('#3d0005','#5b0007','#7a000a','#99000d','#cb181d','#ef3b2c','#fb6a4a','#fc9272','#fcbba1','#fee0d2','#fff5f0',
                             '#22723a','#31a354','#a1d99b',
                             '#021834','#042958','#063776','#084594','#2171b5','#4292c6','#6baed6','#9ecae1','#c6dbef','#eff3ff'))


#scale_fill_manual(values=c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d','#7a000a','#5b0007','#3d0005',
#                           '#a1d99b','#31a354','#22723a',
#                           '#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594','#063776','#042958','#021834'))
#original

pd
###############################Legend outside
pd<-ggplot(d_raw4, aes(x="", y=Value, fill=Item)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(x=2.5, label = paste0(Item)), position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +  
  scale_fill_discrete(breaks=c('Enteric Fermentation','Drained organic soils','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Crop Residues','Manure applied to Soils','Burning - Crop residues',
                               'Net Forest conversion','Fires in organic soils','Fires in humid tropical forests',
                               'Agrifood Systems Waste Disposal','Food Household Consumption','Food Retail','Food Processing','Food Transport','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Pesticides Manufacturing','On-farm Heat Use'))+ #order in the label
  scale_fill_manual(values=c('#a7291e','#bf2f23','#d73527','#ef3b2c','#f04e41','#f26256','#f3756b','#f58980','#f79d95','#f8b0aa','#fac4bf',
                             '#2c924b','#5ab576','#98d1a9',
                             '#174f7e','#1d65a2','#2171b5','#377fbc','#4d8dc3','#639bcb','#79a9d2','#a6c6e1','#d2e2f0','#e8f0f7'))

pd
#original scheme
#scale_fill_manual(values=c('#3d0005','#5b0007','#7a000a','#99000d','#cb181d','#ef3b2c','#fb6a4a','#fc9272','#fcbba1','#fee0d2','#fff5f0',
#                           '#22723a','#31a354','#a1d99b',
#                           '#021834','#042958','#063776','#084594','#2171b5','#4292c6','#6baed6','#9ecae1','#c6dbef','#eff3ff'))

write.csv(d_raw4,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood emissions by items 2018_2020average.csv")
####################################################################
dounut<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood2016_2020_3.xlsx",
        sheet="Sheet1")
dounut<-dounut %>%
  group_by(group)%>%
  summarise(Value = sum(Value))%>%
  mutate(perc = percent(Value / sum(Value)))
  


pdounut<-ggplot(dounut, aes(x="", y=Value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x=2.5, label = paste0(group)), position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +  
  scale_fill_discrete(breaks=c('FU','LU','PPP'))+ #order in the label
  scale_fill_manual(values=c('#ef3b2c',
                             '#31a354',
                             '#2171b5'))

pdounut
####################################################################
######################
#sum
agri_5yravg<-sum(d_raw4$Value)
F
52841099*agri_5yravg/16261315
value <-c(51845696,15954990)
name <-c('All sectors with LULUCF','Agrifood systems')
emission<-data.frame(name,value)

emission$name <- factor(emission$name ,                 # Relevel group factor
                 levels = c("All sectors with LULUCF","Agrifood systems"))


pemission<-ggplot(emission, aes(x="", y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(perc )), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_discrete(breaks=c("Agrifood systems","All sectors with LULUCF" ))+ #order in the label
  scale_fill_manual(values=c("#999999","#31a354"))
pemission
#######another version
d_raw5<-subset(d_raw4, select=c("Item","Value"))
Value<-51845696
Item<-'All sectors with LULUCF'
totemission<-data.frame(Item,Value)
d_raw5<-rbind(d_raw5,totemission)

d_raw5$Item <- factor(d_raw5$Item,levels=c(
                                           'Enteric Fermentation','Drained organic soils','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Crop Residues','Manure applied to Soils','Burning - Crop residues',
                                           'Net Forest conversion','Fires in organic soils','Fires in humid tropical forests',
                                           'Agrifood Systems Waste Disposal','Food Household Consumption','Food Retail','Food Processing','Food Transport','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Pesticides Manufacturing','On-farm Heat Use',
                                           'All sectors with LULUCF'))

pd_raw5<-ggplot(d_raw5, aes(x="", y=Value, fill=Item)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(round(perc,3)*100,"%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  #scale_fill_discrete(breaks=c('Energy Use','Enteric Fermentation','Drained organic soils','Drained organic soils (CO2)','Manure left on Pasture','Rice Cultivation','Synthetic Fertilizers','On-farm energy use','Manure Management','Savanna fires','Others (Farm gate)',
  #                             'Net Forest conversion','Others (Land use change)',
  #                            'Agrifood Systems Waste Disposal','Food Household Consumption','Food Processing','Food Transport','Food Retail','Fertilizers Manufacturing','On-farm Electricity Use','Food Packaging','Others (Pre- and post- production)'))+ #order in the label
  scale_fill_manual(values=c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d','#7a000a','#5b0007','#3d0005',
                             '#a1d99b','#31a354','#22723a',
                             '#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594','#063776','#042958','#021834',
                             "#999999"))
pd_raw5
