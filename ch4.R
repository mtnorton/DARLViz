# Load Data
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(haven)
library(openxlsx)
library(tidyverse)
library(scales)
library(readr)

#create percentage function to show %
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

###########methane pie chart
#average from 2015_2020
m_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/methane/methane_ag.xlsx",
                 sheet="Sheet1")
m_raw<-m_raw %>% 
  group_by(ISO, Item) %>% 
  summarise(Value=mean(Value))

m_raw<-m_raw %>% filter(ISO !="F351") # remove china aggregated

########################fig1 ag / tot by income group pie chart
#wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
m<-merge(x=m_raw, y=country, by.x="ISO", by.y="Code", all.x=T)

m$`Income group` <- gsub('Lower middle income', 'Middle income', m$`Income group`)
m$`Income group` <- gsub('Upper middle income', 'Middle income', m$`Income group`)

library('stringr')
m$`Income group` <- str_replace_all(m$`Income group`, 'Lower middle income', 'Middle income')
m$`Income group` <- str_replace_all(m$`Income group`, 'Higher middle income', 'Middle income')

m1_ag<-subset(m, m$Item=="Agrifood systems") #choose these two for pct
m1_ag<- m1_ag[!is.na(m1_ag$`Income group`),] #remove na
m1_ag<-m1_ag %>% 
  group_by(`Income group`) %>% 
  summarise(Value=sum(Value))%>% 
  mutate(freq = percent(round(Value / sum(Value), 4))) %>% 
  arrange(desc(freq))

m1_ag$`Income group` <- factor(m1_ag$`Income group`,                 # Relevel group factor
                                    levels = c("High income", "Middle income", "Low income"))

pm1_ag<-ggplot(m1_ag, aes(x="", y=Value, fill=`Income group`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(freq )), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 
#  +scale_fill_manual(values=c("#ae1717","#17ae17","#1717ae"))

pm1_ag + labs(title = "            Agri-food systems emissions from CH4 by income group") #theme_bw() white background
write.csv(m1_ag,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/m1_agch4income.csv") 
####################################################################################
#ch4 income group trend 
m_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/methane/methane_ag9020.xlsx",
                 sheet="Sheet1")
m_raw<-subset(m_raw, m_raw$Item=="Agrifood systems") 
m<-merge(x=m_raw, y=country, by.x="ISO", by.y="Code", all.x=T)

m$`Income group` <- gsub('Lower middle income', 'Middle income', m$`Income group`)
m$`Income group` <- gsub('Upper middle income', 'Middle income', m$`Income group`)

library('stringr')
m$`Income group` <- str_replace_all(m$`Income group`, 'Lower middle income', 'Middle income')
m$`Income group` <- str_replace_all(m$`Income group`, 'Higher middle income', 'Middle income')
m<- m[!is.na(m$`Income group`),] #remove na
m_trend<-m %>% 
  group_by(`Income group`,Year) %>% 
  summarise(Value=sum(Value))%>% 
  mutate(freq = percent(round(Value / sum(Value), 4))) %>% 
  arrange(desc(freq))

m_trend$`Income group` <- factor(m_trend$`Income group`,                 # Relevel group factor
                               levels = c("High income", "Middle income", "Low income"))

m_trend$Value<-m_trend$Value/1000000 #display in billion

pm_trend<-ggplot(data=m_trend, aes(x=Year, y=Value, fill = `Income group`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")
#scale_fill_brewer() #this automatically assign color
#geom_text(aes(label = freq), color="#dff6f8", position = position_stack(vjust = 0.5)) add color for cost effective

pm_trend<-pm_trend+ xlab("")+ylab("Gton")+  scale_fill_manual(values=c("#dff6f8", "#95e1e9"), 
                                                                 name="Mitigation Potentials (2020-2050)",
                                                                 breaks=c("tot_tech", "tot_feas"),
                                                                 labels=c("Total land-based mitigation", "Cost-effective avg mitigation"))

pm_trend<-ggplot(data=m_trend, aes(x=factor(`Income group`, level=c("High income", "Middle income", "Low income")), y=value, fill = `Income group`)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Year) #group by year


pm_trend<-pm_trend+ xlab("")+ylab("GtCO2eq")+ labs(title="                    Ch4 trends by income group")+ theme_bw() #white background #scale_fill_manual(values=c("#ffaa00", "#458b74","#0a75ad")) 
pm_trend
write.csv(m_trend,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/m1_agch4incomeyr.csv")

####################################################################################
#pie chart - details of ag items
#average from 2015_2020
mlist_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/methane/methane_ag_list.xlsx",
                 sheet="Sheet1")
mlist_raw<-mlist_raw %>% 
  group_by(ISO, Item) %>% 
  summarise(Value=mean(Value))

mlist_raw<-mlist_raw %>% filter(ISO !="F351") # remove china aggregated

#wb country list merge
mlist<-merge(x=mlist_raw, y=country, by.x="ISO", by.y="Code", all.x=T)

mlist$`Income group` <- gsub('Lower middle income', 'Middle income', mlist$`Income group`)
mlist$`Income group` <- gsub('Upper middle income', 'Middle income', mlist$`Income group`)
mlist$`Income group` <- str_replace_all(mlist$`Income group`, 'Lower middle income', 'Middle income')
mlist$`Income group` <- str_replace_all(mlist$`Income group`, 'Higher middle income', 'Middle income')

mlist<-mlist %>% 
  group_by(Item) %>% 
  summarise(Value=sum(Value))%>% 
  mutate(freq = percent(round(Value / sum(Value), 4))) %>% 
  arrange(desc(freq))

write.csv(mlist,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/m1_aglistch4.csv") 
mlist$Item <- factor(mlist$Item,                 # Relevel group factor
                          levels = c("Enteric Fermentation","Rice Cultivation","Manure Management",
                                     "Savanna fires","Burning - Crop residues","On-farm electricity use",
                                     "Fires in humid tropical forests","Fires in organic soils",
                                     "Food systems waste disposal","Food Household Consumption","Food Processing",
                                     "Food Packaging","Food Retail","On-farm energy use","Food Transport"))


pmlist<-ggplot(mlist, aes(x="", y=Value, fill=Item)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(freq )), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
        scale_fill_manual(values=c("#60393a","#915558","#c17275","#f4a5a8","#f7bbbe","#f9d2d3",
                                   "#84d980","#a8f4a5",
                                   "#393a60","#555891","#7275c1","#8f93f2","#a5a8f4","#bbbef7","#d2d3f9"))
pmlist + labs(title = "            Agri-food systems emissions from CH4 by components") #theme_bw() white background

###################################################################################################################################
###################################################################################################################################
#nonco2 generate each non-co2 ratio over time 032023
nonco2_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/nonco2gases.xlsx",
                      sheet="Sheet1")
nonco2_1<-nonco2_raw %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Element, Year) %>% #group by year and lend type
  
  summarise(Value = sum(Value, na.rm=T))#%>% 

nonco2_2<-nonco2_1 %>%
  group_by(Year) %>%
  summarise(Valuesum = sum(Value)) %>%
 # mutate(pct =  (Value/sum(Value))) %>%
  
  #mutate(pct =  100 *Value/sum(Value)) %>% 
  ungroup #generated each non-co2 ratio

nonco2_2$GtCO2eq<-nonco2_2$Value/1000000 #convert into Billion



nonco2_2$Element <- factor(nonco2_2$Element ,                 # Relevel group factor
                           levels = c("Emissions (CO2eq) from F-gases (AR5)", "Emissions (CO2eq) from N2O (AR5)", "Emissions (CO2eq) from CH4 (AR5)"))
pnonco2_2<-ggplot(data=nonco2_2, aes(x=Year, y=pct, fill = Element)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_area(stat="identity")+
  ggtitle("                       Non CO2 GHG emissions trends in Agri-food system")
  #geom_line(data = nonco2_2, aes(x=Year, y=Value, group = Element, col=Element)) + 
  #geom_point(data = nonco2_2, aes(x=Year, y=Value, group = Element, col=Element))   # secondary y axis

#scale_fill_brewer() #this automatically assign color
#geom_text(aes(label = freq), color="#dff6f8", position = position_stack(vjust = 0.5)) add freq % for cost effectiveness

pnonco2_2<-pnonco2_2+ xlab("")+ylab("Share")+theme_bw()#+scale_y_continuous(sec.axis = sec_axis(~.*5499000, name = "[%]"))
pnonco2_2

write.csv(nonco2_2, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/nonco2gases_2.csv")
#failed to create a total emission on the right side.

###################################################################################################################################
# each non-co2 ratio over time- share of each non-co2gas
# ch4
nonco2_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ilyun/ch4/nonco2gases.xlsx",
                      sheet="Sheet1")
n2o_raw<-subset(nonco2_raw, nonco2_raw$Element=="Emissions (CO2eq) from N2O (AR5)")
n2o_raw_1<-n2o_raw %>%
  group_by(Year) %>%
  mutate(pct =  (Value/sum(Value))) %>% 
  ungroup #generated each non-co2 ratio

n2o_raw_1$GtCO2eq<-n2o_raw_1$Value/1000000 #convert into Billion


n2o_raw_1<-n2o_raw_1 %>%
  group_by(Year) %>%
  mutate(Valuesum =  sum(Value)/1000000)%>% 
  ungroup

n2o_raw_1$Item <- factor(n2o_raw_1$Item ,                 # Relevel group factor
                           levels = c("Land Use change", "Pre- and post- production","Farm gate"))
pn2o_raw_1<-ggplot(data=n2o_raw_1, aes(x=Year, y=pct, fill = Item)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_area(stat="identity")+
  geom_line(data = n2o_raw_1, aes(x=Year, y=Valuesum))+
  scale_y_continuous(name = "Share", limits=c(0,1),        # Add a second axis and specify its features    
                     sec.axis = sec_axis( trans=~.*1, name="GtCO2eq"))+
  ggtitle("Non CO2 GHG emissions trends in Agri-food system: Nitrous oxide")
pn2o_raw_1<-pn2o_raw_1+ xlab("")+ylab("Share")+theme_bw()#+scale_y_continuous(sec.axis = sec_axis(~.*5499000, name = "[GtCO2eq]"))
pn2o_raw_1





######## unable to genertate second axis plot
pch4_raw_1<-ggplot(data=ch4_raw_1, aes(x=Year, y=pct, fill = Item)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")+
  geom_line(data = ch4_raw_1, aes(x=Year, y=Valuesum))+
  scale_y_continuous(
    name="Share", 
    sec.axis=sec_axis(~.*1/5, 
                      name="GtCO2eq",
                      labels = function(x) {
                        paste0(x, "")})) +
  ggtitle("                       Non CO2 GHG emissions trends in Agri-food system: Methane")
pch4_raw_1<-pch4_raw_1+ xlab("")+ylab("Share")+theme_bw()+
pch4_raw_1
############### unable to genertate second axis plot

###############################################################################################
#Figure on sectoral breakdown of ch4 agrifood vs other sectors 2015-2019 avg
wri_ch4_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wri/ch4/historical_emissions.xlsx",
                      sheet="Sheet1")
wri_ch4_raw<-subset(wri_ch4_raw, wri_ch4_raw$Country=="World")
wri_ch4_raw<-subset(wri_ch4_raw, wri_ch4_raw$Sector=="Agriculture"|wri_ch4_raw$Sector=="Energy"|wri_ch4_raw$Sector=="Waste"|wri_ch4_raw$Sector=="Land-Use Change and Forestry"|wri_ch4_raw$Sector=="Industrial Processes")
wri_ch4_raw<-subset(wri_ch4_raw, select=-c(Country,Datasource,Gas,Unit))
 #wide to long
wri_ch4_1<-melt(wri_ch4_raw, id.vars=c("Sector"))
colnames(wri_ch4_1)[2]<-"Year"
wri_ch4_1<-subset(wri_ch4_1, wri_ch4_1$Year==2015|wri_ch4_1$Year==2016|wri_ch4_1$Year==2017|wri_ch4_1$Year==2018|wri_ch4_1$Year==2019)
wri_ch4_1$Year<-as.numeric(as.character(wri_ch4_1$Year)) #need as.character otherwise error due to factor format
wri_ch4_1$value<-as.numeric(wri_ch4_1$value)
wri_ch4_1<-wri_ch4_1%>%
  group_by(Sector)%>%
  summarise(value = sum(value))
wri_ch4_1<-wri_ch4_1 %>%
  group_by() %>%
  mutate(pct =  percent(value/sum(value))) 
  #ungroup #generated each sector ratio
wri_ch4_1$Sector <- factor(wri_ch4_1$Sector,                 # Relevel group factor
                               levels = c("Agriculture", "Energy", "Waste","Land-Use Change and Forestry","Industrial Processes"))

pwri_ch4_1<-ggplot(wri_ch4_1, aes(x="", y=value, fill=Sector)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = ""), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 
#  +scale_fill_manual(values=c("#ae1717","#17ae17","#1717ae"))

pwri_ch4_1 + labs(title = "            Global Methane emissions by sectors (2015-2019 average)") #theme_bw() white background
write.csv(wri_ch4_1,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wri/ch4/ch4_sector.csv") 







