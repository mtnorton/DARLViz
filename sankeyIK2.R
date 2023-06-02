
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(readr)
library(ggpubr)
library(networkD3)

sankey_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/crippa/agri_all_gases9020.xlsx",
                      sheet="Sheet1")
#name change
sankey_raw<-within(sankey_raw, Element[Element=="Emissions (CO2)"]<-"CO2")
sankey_raw<-within(sankey_raw, Element[Element=="Emissions (CO2eq) from CH4 (AR5)"]<-"CH4")
sankey_raw<-within(sankey_raw, Element[Element=="Emissions (CO2eq) from F-gases (AR5)"]<-"F-gases")
sankey_raw<-within(sankey_raw, Element[Element=="Emissions (CO2eq) from N2O (AR5)"]<-"N2O")

sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F15","BEL",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F51","CZE",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F62","ETH",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F164","FSM",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F206","SDN",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F228","RUS",ISO))
sankey_raw <- sankey_raw %>% mutate(ISO=ifelse(ISO=="F248","SRB",ISO))

sankey_raw9002<-subset(sankey_raw, sankey_raw$Year==1990 |sankey_raw$Year==1991 | sankey_raw$Year==1992)

s_raw<-sankey_raw9002 %>% 
  group_by(Area,ISO,Element,Item) %>% 
  summarise(Value=mean(Value))  # 3 year avearge

####################################################################################################
#data 2018-2020
#below includes old PPP
sankey_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/crippa/farn_lu_2016_2020.xlsx",
                      sheet="Sheet1")
sankey_raw<-subset(sankey_raw, sankey_raw$Item!='Pre- and post- production')

newppp<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/crippa/ppp_raw.csv") #new ppp
sankey_raw<-rbind(sankey_raw, newppp)
sankey_raw1820<-subset(sankey_raw, sankey_raw$Year==2018 |sankey_raw$Year==2019 | sankey_raw$Year==2020)

#name change
sankey_raw1820<-within(sankey_raw1820, Element[Element=="Emissions (CO2)"]<-"CO2")
sankey_raw1820<-within(sankey_raw1820, Element[Element=="Emissions (CO2eq) from CH4 (AR5)"]<-"CH4")
sankey_raw1820<-within(sankey_raw1820, Element[Element=="Emissions (CO2eq) from F-gases (AR5)"]<-"F-gases")
sankey_raw1820<-within(sankey_raw1820, Element[Element=="Emissions (CO2eq) from N2O (AR5)"]<-"N2O")


s_raw<-sankey_raw1820 %>% 
  group_by(Area,ISO,Element,Item) %>% 
  summarise(Value=mean(Value))  # 3 year avearge

s_raw<-subset(s_raw, s_raw$Item=="Farm gate"| s_raw$Item=="Land Use change"|s_raw$Item=="Pre- and Post- Production") #use this for only showing 3 category
#s_raw<-subset(s_raw, s_raw$Item!="Farm gate"& s_raw$Item!="Land Use change"& s_raw$Item!="Pre- and post- production")
#########################################################################################################

##merge income group #wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")#wb country list
country$`Income group` <- gsub('Lower middle income', 'Middle income', country$`Income group`)
country$`Income group` <- gsub('Upper middle income', 'Middle income', country$`Income group`)

library('stringr')
country$`Income group` <- str_replace_all(country$`Income group`, 'Lower middle income', 'Middle income')
country$`Income group` <- str_replace_all(country$`Income group`, 'Higher middle income', 'Middle income')

s<-merge(x=s_raw, y=country, by.x="ISO", by.y="Code", all.x=F)
test <- s[is.na(s$`Income group`),]

s<-s %>% 
  group_by(`Income group`, Element,Item) %>% 
  summarise(Value=sum(Value, na.rm=T))  # 3 year avearge
s <- s[!is.na(s$`Income group`),] #remove na income group

s_1<-s %>% 
  group_by(`Income group`,Element) %>% 
  summarise(Value=sum(Value))
colnames(s_1)[1]<-"source"
colnames(s_1)[2]<-"target"
s_2<-subset(s, select=c(Element,Item,Value))
colnames(s_2)[1]<-"source"
colnames(s_2)[2]<-"target"
s_3<-rbind(s_1,s_2)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(s_3$source), 
         as.character(s_3$target)) %>% unique()
)

nodes<- nodes %>% arrange(factor(name, levels = c("Low income",
                                                  "Middle income",
                                                  "High income",
                                                  "CO2",
                                                  "CH4",
                                                  "N2O",
                                                  "F-gases")))

nodes<- nodes %>% arrange(factor(name, levels = c("Low income",
                                                  "Middle income",
                                                  "High income",
                                                  "Emissions (CO2)",
                                                  "Emissions (CO2eq) from CH4 (AR5)",
                                                  "Emissions (CO2eq) from N2O (AR5)",
                                                  "Drained organic soils (CO2)",
                                                  "Drained organic soils (N2O)",
                                                  "Synthetic Fertilizers",
                                                  "Crop Residues",
                                                  "Manure left on Pasture",
                                                  "Manure applied to Soils",
                                                  "Manure Management",
                                                  "Enteric Fermentation",
                                                  "Savanna fires",
                                                  "Burning - Crop residues",
                                                  "Rice Cultivation",
                                                  "On-farm energy use",
                                                  "Fires in organic soils",
                                                  "Fires in humid tropical forests",
                                                  "Net Forest conversion",
                                                  "On-farm electricity use",
                                                  "Fertilizers Manufacturing",
                                                  "Food Household Consumption",
                                                  "Food Packaging",
                                                  "Food Processing",
                                                  "Food Transport",
                                                  "Food Retail",
                                                  "Food systems waste disposal")))

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
s_3$IDsource <- match(s_3$source, nodes$name)-1 
s_3$IDtarget <- match(s_3$target, nodes$name)-1
s_3<-as.data.frame(s_3)
# Make the Network
ps <- sankeyNetwork(Links = s_3, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "Value", NodeID = "name", 
                    sinksRight=FALSE,fontSize = 17)

ps


#################################################################
# From these flows we need to create a node data frame: it lists every entities involved in the flow
s_3_1<- s_3[1:12, ] #extract first half of the chart
nodes3_1 <- data.frame(
  name=c(as.character(s_1$source), 
         as.character(s_1$target)) %>% unique()
)

nodes3_1<- nodes3_1 %>% arrange(factor(name, levels = c("Low income",
                                                        "Middle income",
                                                        "High income",
                                                        "CO2",
                                                        "CH4",
                                                        "N2O",
                                                        "F-gases")))

# Give a color for each group:#https://imagecolorpicker.com/
my_color <- 'd3.scaleOrdinal() .domain(["Low income",
                                                  "Middle income",
                                                  "High income",
                                                  "CO2",
                                                  "CH4",
                                                  "N2O",
                                                  "F-gases"]) .range(["#f8766d", "#00ba38","#619cff","#c6d7ef","#609eca","#ffcf9f","#ffa454"])'

s_3_1$line_group <- sub(' .*', '', nodes3_1[s_3_1$IDsource + 1, 'name']) #line group is for add line color
# Make the Network
ps_3_1 <- sankeyNetwork(Links = s_3_1, Nodes = nodes3_1,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "Value", NodeID = "name", 
                        sinksRight=FALSE,fontSize = 17,
                        colourScale=my_color, NodeGroup="name",LinkGroup = 'line_group',iterations = 0)#iterations = 0 this is to disable the algorithm which automatically determines the node placement 
ps_3_1

s_3_1 <- s_3_1 %>%
  mutate(freq = round(Value / sum(Value), 3)) %>% 
  arrange(desc(freq)) #generate percentage
write.csv(s_3_1,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/crippa/Sankey diagram for GHG emissions from the food system 1990_1992_1.csv')

s_3_1 %>%
  group_by(source)%>%
  summarise(Valuetot = sum(Value))%>%
  mutate(freq = round(Valuetot / sum(Valuetot), 4)) 
s_3_1 %>%
  group_by(target)%>%
  summarise(Valuetot = sum(Value))%>%
  mutate(freq = round(Valuetot / sum(Valuetot), 4)) 

###############

s_3_2<- s_3[13:42, ]
nodes3_2 <- data.frame(
  name=c(as.character(s_2$source), 
         as.character(s_2$target)) %>% unique()
)

data$title2 <- paste0(data$title," (",data$percs,")")



nodes3_2<- nodes3_2 %>% arrange(factor(name, levels = c("CO2",
                                                        "CH4",
                                                        "N2O",
                                                        "F-gases",
                                                        "Farm gate",
                                                        "Land Use change",
                                                        "Pre- and Post- Production")))
my_color <- 'd3.scaleOrdinal() .domain(["CO2","CH4","N2O","F-gases", "Farm gate","Land Use change","Pre- and Post- Production"]) .range(["#c6d7ef","#609eca","#ffcf9f","#ffa454","#ef3b2c","#31a354","#4292c6"])'

s_3_2$IDsource <- match(s_3_2$source, nodes3_2$name)-1 
s_3_2$IDtarget <- match(s_3_2$target, nodes3_2$name)-1
s_3_2<-as.data.frame(s_3_2)


ps_3_2 <- sankeyNetwork(Links = s_3_2, Nodes = nodes3_2,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "Value", NodeID = "name", 
                        sinksRight=FALSE,fontSize = 17,
                        colourScale=my_color, NodeGroup="name",iterations = 0)#iterations = 0 this is to disable the algorithm which automatically determines the node placement 
ps_3_2
##########################################################################
#summarise s_3_2 so that income group is disolved
s_3_2<- s_3[13:42, ]
s_3_2_1<-s_3_2%>%
  group_by(source, target)%>%
  summarise(Value = sum(Value))

nodes3_2_1 <- data.frame(
  name=c(as.character(s_2$source), 
         as.character(s_2$target)) %>% unique())

nodes3_2_1<- nodes3_2_1 %>% arrange(factor(name, levels = c("CO2",
                                                            "CH4",
                                                            "N2O",
                                                            "F-gases",
                                                            "Farm gate",
                                                            "Land Use change",
                                                            "Pre- and Post- Production")))
#my_color3_2_1 <- 'd3.scaleOrdinal() .domain(["CO2","CH4","N2O","F-gases", "Farm gate","Land Use change","Pre- and Post- Production"]) .range(["#c6d7ef","#609eca","#ffcf9f","#ffa454","red","#31a354","#4292c6"])'
my_color3_2_1 <- 'd3.scaleOrdinal() .domain(["Low income",
                                                  "Middle income",
                                                  "High income",
                                                  "CO2",
                                                  "CH4",
                                                  "N2O",
                                                  "F-gases"]) .range(["#f8766d", "#00ba38","#619cff","#c6d7ef","#609eca","#ffcf9f","#ffa454"])'


s_3_2_1$IDsource <- match(s_3_2_1$source, nodes3_2_1$name)-1 
s_3_2_1$IDtarget <- match(s_3_2_1$target, nodes3_2_1$name)-1
s_3_2_1<-as.data.frame(s_3_2_1)

s_3_2_1$line_group <- sub(' .*', '', nodes3_2_1[s_3_2_1$IDsource + 1, 'name']) #line group is for add line color

ps_3_2_1 <- sankeyNetwork(Links = s_3_2_1, Nodes = nodes3_2_1,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "Value", NodeID = "name", 
                          sinksRight=FALSE,fontSize = 17,
                          colourScale=my_color3_2_1, NodeGroup="name",LinkGroup = 'line_group',iterations = 0)
#line group is for add line color
#iterations = 0 this is to disable the algorithm which automatically determines the node placement 
ps_3_2_1

s_3_2_1 <- s_3_2_1 %>%
  mutate(freq = round(Value / sum(Value), 3)) %>% 
  arrange(desc(freq)) #generate percentage
write.csv(s_3_2_1,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/crippa/Sankey diagram for GHG emissions from the food system 1990_1992_2.csv')

s_3_2_1 %>%
  group_by(source)%>%
  summarise(Valuetot = sum(Value))%>%
  mutate(freq = round(Valuetot / sum(Valuetot), 4)) 
s_3_2_1 %>%
  group_by(target)%>%
  summarise(Valuetot = sum(Value))%>%
  mutate(freq = round(Valuetot / sum(Valuetot), 4)) 



