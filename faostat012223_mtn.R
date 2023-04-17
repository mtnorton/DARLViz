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

setwd('C:/Users/wb383351/WBG/Ilyun Koh - data')

#create percentage function to show %
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

f_raw<-read_xlsx("./fao/Emissions_Totals/emissions_afolu.xlsx",
                 sheet="Sheet1")
f_raw<-read.csv("./fao/Emissions_Totals/Emissions_Totals_E_All_Data_NOFLAG2.csv", encoding="UTF-8",check.names = FALSE)
f_countryname<-f_raw %>% count(Area)#check if utf-8 error in the country name

#wb country list
country<-read_xlsx("./wb_countries/country_income.xlsx", sheet="List of economies")
country<-within(country, Region[Region=="East Asia & Pacific"]<-"EAP")
country<-within(country, Region[Region=="Europe & Central Asia"]<-"ECA")
country<-within(country, Region[Region=="Latin America & Caribbean"]<-"LCR")
country<-within(country, Region[Region=="Middle East & North Africa"]<-"MNA")
country<-within(country, Region[Region=="North America"]<-"NAR")
country<-within(country, Region[Region=="South Asia"]<-"SAR")
country<-within(country, Region[Region=="Sub-Saharan Africa"]<-"SSA")
f<-merge(x=f_raw, y=country, by.x="Area", by.y="Economy", all.x=T)
f_match <- f[!is.na(f$Code),] #keep matching rows
#####################
#some iso did not match due to different names
#####this is to examine no ISO names
f_noISO <- f[is.na(f$Code),]
f_noISOcountryname<-f_noISO %>% count(Area)#check if utf-8 error in the country name
f_noISO <- subset(f_noISO, select = -c(73, 74,75,76,77)) #drop the columns from country dataset
#replace names into names from country dataset
f_noISO<-within(f_noISO, Area[Area=="Côte d'Ivoire"]<-"Côte d’Ivoire")
f_noISO<-within(f_noISO, Area[Area=="Czechia"]<-"Czech Republic")
f_noISO<-within(f_noISO, Area[Area=="Czechoslovakia"]<-"Czech Republic")
f_noISO<-within(f_noISO, Area[Area=="Ethiopia PDR"]<-"Ethiopia")
f_noISO<-within(f_noISO, Area[Area=="Sao Tome and Principe"]<-"São Tomé and Príncipe")
f_noISO<-within(f_noISO, Area[Area=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom")
f_noISO<-within(f_noISO, Area[Area=="USSR"]<-"Russian Federation")
f_noISO<-within(f_noISO, Area[Area=="Bahamas"]<-"Bahamas, The")
f_noISO<-within(f_noISO, Area[Area=="Belgium-Luxembourg"]<-"Belgium")
f_noISO<-within(f_noISO, Area[Area=="Bolivia (Plurinational State of)"]<-"Bolivia")
f_noISO<-within(f_noISO, Area[Area=="Congo"]<-"Congo, Rep.")
f_noISO<-within(f_noISO, Area[Area=="China, mainland"]<-"China")
f_noISO<-within(f_noISO, Area[Area=="China, Taiwan Province of"]<-"Taiwan, China")
f_noISO<-within(f_noISO, Area[Area=="China, Hong Kong SAR"]<-"Hong Kong SAR, China")
f_noISO<-within(f_noISO, Area[Area=="China, Macao SAR"]<-"Macao SAR, China")
f_noISO<-within(f_noISO, Area[Area=="Democratic People's Republic of Korea"]<-"Korea, Dem. People's Rep.")
f_noISO<-within(f_noISO, Area[Area=="Democratic Republic of the Congo"]<-"Congo, Dem. Rep.")
f_noISO<-within(f_noISO, Area[Area=="Egypt"]<-"Egypt, Arab Rep.")
f_noISO<-within(f_noISO, Area[Area=="Gambia"]<-"Gambia, The")
f_noISO<-within(f_noISO, Area[Area=="Iran (Islamic Republic of)"]<-"Iran, Islamic Rep.")
f_noISO<-within(f_noISO, Area[Area=="Kyrgyzstan"]<-"Kyrgyz Republic")
f_noISO<-within(f_noISO, Area[Area=="Lao People's Democratic Republic"]<-"Lao PDR")
#f_noISO<-within(f_noISO, Area[Area=="Micronesia (Federated States of)"]<-"Micronesia, Fed. Sts.")
f_noISO<-within(f_noISO, Area[Area=="Palestine(1996-)"]<-"West Bank and Gaza")
f_noISO<-within(f_noISO, Area[Area=="Republic of Korea"]<-"Korea, Rep.")
f_noISO<-within(f_noISO, Area[Area=="Republic of Moldova"]<-"Moldova")
f_noISO<-within(f_noISO, Area[Area=="Saint Kitts and Nevis"]<-"St. Kitts and Nevis")
f_noISO<-within(f_noISO, Area[Area=="Saint Lucia"]<-"St. Lucia")
f_noISO<-within(f_noISO, Area[Area=="Saint Vincent and the Grenadines"]<-"St. Vincent and the Grenadines")
f_noISO<-within(f_noISO, Area[Area=="Slovakia"]<-"Slovak Republic")
#f_noISO<-within(f_noISO, Area[Area=="Turkey"]<-"Türkiye")
f_noISO<-within(f_noISO, Area[Area=="United Republic of Tanzania"]<-"Tanzania")
f_noISO<-within(f_noISO, Area[Area=="United States of America"]<-"United States")
f_noISO<-within(f_noISO, Area[Area=="Venezuela (Bolivarian Republic of)"]<-"Venezuela, RB")
f_noISO<-within(f_noISO, Area[Area=="Viet Nam"]<-"Vietnam")
f_noISO<-within(f_noISO, Area[Area=="Yemen"]<-"Yemen, Rep.")

f_noISO<-merge(x=f_noISO, y=country, by.x="Area", by.y="Economy", all.x=T) #merge no iso
f_noISO2 <- f_noISO[is.na(f_noISO$Code),]
f_noISOcountryname<-f_noISO2 %>% count(Area)

# ADDING
f_noISO$Region2 <- f_noISO$Region2.y
f_noISO <- f_noISO %>% dplyr::select(-Region2.x,-Region2.y)

f<-rbind(f_match,f_noISO) #join matching and unmatching dataset

f$`Income group` <- gsub('Lower middle income', 'Middle income', f$`Income group`)
f$`Income group` <- gsub('Upper middle income', 'Middle income', f$`Income group`)

library('stringr')
f$`Income group` <- str_replace_all(f$`Income group`, 'Lower middle income', 'Middle income')
f$`Income group` <- str_replace_all(f$`Income group`, 'Higher middle income', 'Middle income')
######################################################################################################################
#Figure 1: Global agrifood systems emissions by component and indicator
f<-subset(f, select=-c(2,3,4,6,8,9))
f1<-subset(f, f$Item=="Farm-gate emissions"|f$Item=="Land Use change"|f$Item=="Pre- and post- production")
f1<-subset(f1, f1$Element=="Emissions (CO2eq) (AR5)")
#wide to long
f1long<-melt(f1, id.vars=c("Area","Item","Element","Unit","Code","Region","Income group","Lending category","Other (EMU or HIPC)"))
colnames(f1long)[10]<-"Year"
#year to numeric
f1long$Year<-as.numeric(as.character(f1long$Year)) #need as.character otherwise error due to factor format

#f1long_pa<-f1long %>% 
  #  filter(!is.na(treated)) %>% 
#  group_by(`Income group`,Item, Year) %>% #group by year and lend type
#  summarise(value = sum(value, na.rm=T))
f1long_pa<- f1long_pa[!is.na(f1long_pa$`Income group`),] #keep matching row
f1long_pa<-subset(f1long_pa, f1long_pa$value>0)
f1long_pa<-subset(f1long_pa, f1long_pa$Year<=2020)
f1long_pa$value<-f1long_pa$value/1000000 #convert into Billion

f1long_pa$`Income group` <- factor(f1long_pa$`Income group`,                 # Relevel group factor
                                levels = c("High income", "Middle income", "Low income"))
f1long_pa$Item <- factor(f1long_pa$Item,                 # Relevel group factor
                      levels = c("Farm-gate emissions", "Land Use change", "Pre- and post- production"))
pf1long_pa<-ggplot(data=f1long_pa, aes(x=factor(`Income group`, level=c("High income", "Middle income", "Low income")), y=value, fill = Item)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Year) #group by year
pf1long_pa<-pf1long_pa+ xlab("")+ylab("GtCO2eq")+  scale_fill_manual(values=c("#ffaa00", "#458b74","#0a75ad")) 



proe_pa<-proe_pa+ xlab("")+ylab("MtCO2e/yr")+  scale_fill_manual(values=c("#dff6f8", "#95e1e9"), 
                                                                 name="Mitigation Potentials (2020-2050)",
                                                                 breaks=c("tot_tech", "tot_feas"),
                                                                 labels=c("Total land-based mitigation", "Cost-effective avg mitigation"))


                                                                                                       
################################










#chapter2 writing 
#f<-subset(f, select=-c(2,3,4,6,8,9))

f_item2<-f1%>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Item) %>% #group by year and lend type
  summarise(value = n())
#write.csv(f_item,"./fao/Emissions_Totals/f_item.csv" )
f1<-subset(f,f$Item=="Crop Residues"|f$Item=="Rice Cultivation"|f$Item=="Burning - Crop residues"|f$Item=="Enteric Fermentation"|
             f$Item=="Manure Management"|f$Item=="Manure left on Pasture"|f$Item=="Manure applied to Soils"|
             f$Item=="Synthetic Fertilizers"|f$Item=="Drained organic soils"|f$Item=="Drained organic soils (CO2)"|f$Item=="Drained organic soils (N2O)"|
             f$Item=="Forestland"|f$Item=="Net Forest conversion"|f$Item=="Savanna fires"|f$Item=="Fires in organic soils"|f$Item=="Forest fires"|
             f$Item=="On-farm energy use"|f$Item=="On-farm electricity use"|f$Item=="Fertilizers manufacturing"|f$Item=="Pesticides manufacturing"|
             f$Item=="Food household consumption"|f$Item=="Food packaging"|f$Item=="Food processing"|f$Item=="Food transport"|
             f$Item=="Food retail"|f$Item=="Food waste disposal")
f1<-subset(f1, f1$Element=="Emissions (CO2eq) (AR5)")
#wide to long
f1long<-melt(f1, id.vars=c("Area","Item","Element","Unit","Code","Region","Income group","Lending category","Other (EMU or HIPC)"))
colnames(f1long)[10]<-"Year"
#year to numeric
f1long$Year<-as.numeric(as.character(f1long$Year)) #need as.character otherwise error due to factor format

f1long$value <- as.numeric(f1long$value)

f1long_pa<-f1long %>% 
#  filter(!is.na(treated)) %>% 
  group_by(`Income group`,Item, Year) %>% #group by year and lend type
  summarise(value = sum(value, na.rm=T))
f1long_pa<- f1long_pa[!is.na(f1long_pa$`Income group`),] #keep matching row
f1long_pa<-subset(f1long_pa, f1long_pa$value>0)
f1long_pa2<-subset(f1long_pa, f1long_pa$Year==2020 | f1long_pa$Year==1990)
f1long_pa2$value<-f1long_pa2$value/1000000 #convert into Billion

f1long_pa2<-within(f1long_pa2, Item[Item=="Burning - Crop residues"]<-"Burning biomass")
f1long_pa2<-within(f1long_pa2, Item[Item=="Fires in organic soils"]<-"Burning biomass")
f1long_pa2<-within(f1long_pa2, Item[Item=="Forest fires"]<-"Burning biomass")
f1long_pa2<-within(f1long_pa2, Item[Item=="Drained organic soils (CO2)"]<-"Drained organic soils")
f1long_pa2<-within(f1long_pa2, Item[Item=="Drained organic soils (N2O)"]<-"Drained organic soils")
f1long_pa2<-within(f1long_pa2, Item[Item=="Manure applied to Soils"]<-"Manure")
f1long_pa2<-within(f1long_pa2, Item[Item=="Manure left on Pasture"]<-"Manure")
f1long_pa2<-within(f1long_pa2, Item[Item=="Manure Management"]<-"Manure")

f1long_pa3<-f1long_pa2 %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(`Income group`,Item, Year) %>% #group by year and lend type
  summarise(value = sum(value, na.rm=T))

f1long_pa3<-subset(f1long_pa3, f1long_pa3$Item!="Forestland")

f1long_pa3$`Income group` <- factor(f1long_pa3$`Income group`,                 # Relevel group factor
                                   levels = c("High income", "Middle income", "Low income"))

ag_class<-read.csv("./fao/Emissions_Totals/ag_classification.csv")
test<-merge(x=f1long_pa3, y=ag_class, by.x="Item", by.y="category")
#write.csv(test, "./fao/Emissions_Totals/f1long_pa3.csv")


################### HERE

f1long_pa3<-read.csv("./fao/Emissions_Totals/f1long_pa4.csv")

f1long_pa3$class <- factor(f1long_pa3$class,                 # Relevel group factor
                         levels = c("Farm-gate emissions", "Land Use change", "Pre- and post- production"))

f1long_pa3$Income.group <- factor(f1long_pa3$Income.group, level=c("Low income", "Middle income", "High income"))
f1long_pa3$Item <- factor(f1long_pa3$Item,levels=c("Crop Residues","Drained organic soils","Enteric Fermentation","Manure","Rice Cultivation", "Savanna fires", "Synthetic Fertilizers", "On-farm electricity use", "Burning biomass", "Net Forest conversion", "Fertilizers Manufacturing", "Food Household Consumption", "Food Packaging", "Food Processing", "Food Retail", "Food Transport", "Food systems waste disposal"))

#pf1long_pa3<-
  ggplot(data=f1long_pa3, aes(x=Year, y=value, fill = Item)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Income.group)+
  theme(legend.title= element_blank(),
        panel.background = element_blank()
        ) + 
  xlab("") + 
  ylab(bquote("Gt"~CO[2]~"eq")) +
  labs(tag="test") +
  scale_fill_manual(values=c('#fff5f0','#fee0d2','#fcbba1','#fc9272',
                             '#fb6a4a','#ef3b2c','#cb181d','#99000d','#a1d99b','#31a354',
                             '#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594')) +
  scale_x_continuous(breaks=c(1990,2020),labels=c("1990","2020")) 




proe_pa<-proe_pa+ xlab("")+ylab("MtCO2e/yr")+  scale_fill_manual(values=c("#dff6f8", "#95e1e9"), 
                                                                 name="Mitigation Potentials (2020-2050)",
                                                                 breaks=c("tot_tech", "tot_feas"),
                                                                 labels=c("Total land-based mitigation", "Cost-effective avg mitigation"))
