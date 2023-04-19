# Load Data
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(haven)
library(openxlsx)
library(tidyverse)
library(scales)

#create percentage function to show %
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#figure1
f1_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Mitigation Potential of Food System Practices2.csv", check.names = FALSE)
#wide to long
f1<-melt(f1_raw, id.vars=c("a"))
f1$valuepct<-f1$value*100
#level_order <- c("Existing practices and technologies", "Rice paddies: Improved water management in rice paddies", "Crop: Nutrient management (e.g. balance nitrogen application)", "Crop: Biochar", "Crop : No-till and residue management", "Livestock: Grazing management; animal feeding", "Livestock: Manure management", "Cross-cutting (crop-livestock): Agroforestry", "Off-farm/demand side/other: Avoided forest conversion", "Off-farm/demand side/other: Reduce food loss and waste", "Off-farm/demand side/other: Shift diet demands from livestock- to plant-based protein")
#####
pf1<-f1 %>%
  mutate(a = factor(a, levels=c("Off-farm/demand side/other: Shift diet demands from livestock- to plant-based protein",
                                "Off-farm/demand side/other: Reduce food loss and waste",
                                "Off-farm/demand side/other: Avoided forest conversion",
                                "Cross-cutting (crop-livestock): Agroforestry", 
                                "Livestock: Manure management", 
                                "Livestock: Grazing management, animal feeding", 
                                "Crop : No-till and residue management",
                                "Crop: Biochar", 
                                "Crop: Nutrient management (e.g. balance nitrogen application)",
                                "Rice paddies: Improved water management in rice paddies",
                                "Existing practices and technologies"))) %>%
  ggplot( aes(x = valuepct, y = a,fill = variable)) +
  geom_col(position = "fill")+
  scale_x_continuous(labels=percent_format())

pf1<-pf1+ xlab("")+ylab("")+  scale_fill_discrete(name="Mitigation potential/cost",
                                                  breaks=c("b2", "b1"),
                                                  labels=c(">100 US$/tCO2e", "<=100 US$/tCO2e"))
pf1
write.csv(f1,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f1_output.csv")
###########################################################################
#figure2
roe_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/roe2021/roe_dat.csv")
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
country<-within(country, Region[Region=="East Asia & Pacific"]<-"EAP")
country<-within(country, Region[Region=="Europe & Central Asia"]<-"ECA")
country<-within(country, Region[Region=="Latin America & Caribbean"]<-"LCR")
country<-within(country, Region[Region=="Middle East & North Africa"]<-"MNA")
country<-within(country, Region[Region=="North America"]<-"NAR")
country<-within(country, Region[Region=="South Asia"]<-"SAR")
country<-within(country, Region[Region=="Sub-Saharan Africa"]<-"SSA")
roe_raw<-merge(x=roe_raw, y=country, by.x="ISO", by.y="Code", all.x=T)
library('stringr')
roe_raw$`Income group` <- str_replace_all(roe_raw$`Income group`, 'Lower middle income', 'Middle income')
roe_raw$`Income group` <- str_replace_all(roe_raw$`Income group`, 'Upper middle income', 'Middle income')
################################################################################################################
roe_p<-subset(roe_raw, roe_raw$tot_tech>0)

roe_pa<-roe_p %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Region) %>% #group by year and lend type
  summarise(
    tot_tech = sum(tot_tech, na.rm=T), 
    tot_feas = sum(tot_feas , na.rm=T))
#roe_pa<- subset(roe_pa, roe_pa$Region !="Middle East & North Africa")
#roe_pa<- subset(roe_pa, roe_pa$Region !="North America") 	

roe_pa<- roe_pa[!is.na(roe_pa$Region),] #keep matching row
#source: https://r-graph-gallery.com/ggplot2-package.html
#roe_pa$pct<-roe_pa$pct/1000000 #display in billion

#create freq
roe_pa$freq<-roe_pa$tot_feas / roe_pa$tot_tech
roe_pa$freq<-percent(roe_pa$freq)

roe_pa$tot_tech<-roe_pa$tot_tech - roe_pa$tot_feas #for stack, tot_tech needs to be converted to total - cost-effective avg
roe_pa<-melt(roe_pa, id.vars=c("Region","freq"))

proe_pa<-ggplot(data=roe_pa, aes(x=Region, y=value, fill = variable)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")
  #scale_fill_brewer() #this automatically assign color
  #geom_text(aes(label = freq), color="#dff6f8", position = position_stack(vjust = 0.5)) #add color for cost effective
  
proe_pa<-proe_pa+ xlab("")+ylab("MtCO2e/yr")+ theme_bw()+  scale_fill_manual(values=c("#dff6f8", "#95e1e9"), 
                                                  name="Mitigation Potentials (2020-2050)",
                                                  breaks=c("tot_tech", "tot_feas"),
                                                  labels=c("Total land-based mitigation", "Cost-effective avg mitigation"))

proe_pa
roe_pa_region <- roe_pa
write.csv(roe_pa_region,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f2_region_output.csv")

#repeat for Income group####################################################
roe_p<-subset(roe_raw, roe_raw$tot_tech>0)

roe_pa<-roe_p %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(`Income group`) %>% #group by year and lend type
  summarise(
    tot_tech = sum(tot_tech, na.rm=T), 
    tot_feas = sum(tot_feas , na.rm=T))

roe_pa<- roe_pa[!is.na(roe_pa$`Income group`),] #keep matching row
#source: https://r-graph-gallery.com/ggplot2-package.html
#roe_pa$pct<-roe_pa$pct/1000000 #display in billion

#create freq
roe_pa$freq<-roe_pa$tot_feas / roe_pa$tot_tech
roe_pa$freq<-percent(roe_pa$freq)

roe_pa$tot_tech<-roe_pa$tot_tech - roe_pa$tot_feas #for stack, tot_tech needs to be converted to total - cost-effective avg
roe_pa<-melt(roe_pa, id.vars=c("Income group","freq"))
roe_pa$`Income group` <- factor(roe_pa$`Income group`,                 # Relevel group factor
                                 levels = c("Low income", "Middle income","High income" ))
proe_pa<-ggplot(data=roe_pa, aes(x=`Income group`, y=value, fill = variable)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")
  #scale_fill_brewer() #this automatically assign color
  #geom_text(aes(label = freq), color="#dff6f8", position = position_stack(vjust = 0.5)) add freq % for cost effectiveness

proe_pa<-proe_pa+ xlab("")+ylab("MtCO2e/yr")+ theme_bw()+  scale_fill_manual(values=c("#dff6f8", "#95e1e9"), 
                                                                 name="Mitigation Potentials (2020-2050)",
                                                                 breaks=c("tot_tech", "tot_feas"),
                                                                 labels=c("Total land-based mitigation", "Cost-effective avg mitigation"))

proe_pa
write.csv(roe_pa,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f2_income_output.csv")
#################################################################################
#Figure3v1
roe_p<-subset(roe_raw, roe_raw$tot_tech>0)
roe_p<-subset(roe_p, roe_p$ISO=="BRA" | roe_p$ISO=="CHN" | roe_p$ISO=="IDN" | roe_p$ISO=="USA" | roe_p$ISO=="IND" | roe_p$ISO=="RUS" | roe_p$ISO=="CAN" | 
                roe_p$ISO=="DRC" | roe_p$ISO=="COL" | roe_p$ISO=="MEX" | roe_p$ISO=="ARG" | roe_p$ISO=="AUS" | roe_p$ISO=="BOL" | roe_p$ISO=="PER" | 
                roe_p$ISO=="MMR" | roe_p$ISO=="KAZ")

proe_pb<-ggplot(data=roe_p, aes(x=tot_tech, y=tot_feas, color=Region,label=ISO)) 
proe_pb<-proe_pb+xlab("Total land-based mitigation (MtCO2e/yr)")+ylab("Cost-effective avg (MtCO2e/yr)")+ 
  geom_text(size=3,aes(colour = factor(Region)), check_overlap = F,)+ #Avoid overlaps
  xlim (0,4500)+ ylim(0,4500)+
  geom_abline(intercept = 0, slope = 1, size = 0.5, color="red")+ 
  theme(legend.position = c(.9, .65))
#  theme(legend.position = c(.9, .2)) 
proe_pb

#Figure3v2
roe_p<-subset(roe_raw, roe_raw$tot_tech>0)
roe_p<-subset(roe_p, roe_p$ISO=="BRA" | roe_p$ISO=="CHN" | roe_p$ISO=="IDN" | roe_p$ISO=="USA" | roe_p$ISO=="IND" | roe_p$ISO=="RUS" | roe_p$ISO=="CAN" | 
                roe_p$ISO=="DRC" | roe_p$ISO=="COL" | roe_p$ISO=="MEX" | roe_p$ISO=="ARG" | roe_p$ISO=="AUS" | roe_p$ISO=="BOL" | roe_p$ISO=="PER" | 
                roe_p$ISO=="MMR" | roe_p$ISO=="KAZ")

lm <- lm(tot_feas ~ tot_tech, roe_p)

proe_pb<-ggplot(data=roe_p, aes(x=tot_tech, y=tot_feas, color=Region,label=ISO)) 
proe_pb<-proe_pb+xlab("Total land-based mitigation (MtCO2e/yr)")+ylab("Cost-effective avg (MtCO2e/yr)")+ 
  geom_text(size=3,aes(colour = factor(Region)), check_overlap = F,)+ #Avoid overlaps
  xlim (0,4500)+ ylim(0,1750)+
  geom_abline(slope = coef(lm)[["tot_tech"]], intercept = coef(lm)[["(Intercept)"]], col="red")+
  theme(legend.position = c(.87, .25))
#  theme(legend.position = c(.9, .2)) 
proe_pb+theme_bw()
write.csv(roe_p,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f3_15countries_output.csv")

#################################################################################
#Figure4
roe_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/roe2021/roe_dat_asheshfig4.csv")
roe_f4<-subset(roe_raw, roe_raw$ISO=="BRA" | roe_raw$ISO=="CHN" | roe_raw$ISO=="IDN" | roe_raw$ISO=="USA"| roe_raw$ISO=="EU" | roe_raw$ISO=="IND" | roe_raw$ISO=="RUS" | roe_raw$ISO=="CAN" | 
                roe_raw$ISO=="DRC" | roe_raw$ISO=="COL" | roe_raw$ISO=="MEX" | roe_raw$ISO=="ARG" | roe_raw$ISO=="AUS" | roe_raw$ISO=="BOL" | roe_raw$ISO=="PER" | 
                roe_raw$ISO=="MMR" | roe_raw$ISO=="KAZ")

roe_f4<-subset(roe_f4, select=c(ISO, totag_re_feas, totag_sc_feas, totdem_feas, formgmts_feas, defors_feas,ars_feas))
#roe_f4$sum<- roe_f4$totag_re_feas+roe_f4$totag_sc_feas+roe_f4$totdem_feas+roe_f4$formgmts_feas+roe_f4$defors_feas+roe_f4$ars_feas
roe_f4 %>%
  arrange(desc(roe_f4$totag_re_feas+roe_f4$totag_sc_feas+roe_f4$totdem_feas+roe_f4$formgmts_feas+roe_f4$defors_feas+roe_f4$ars_feas))
#wide to long
roe_f4<-melt(roe_f4, id.vars=c("ISO"))

roe_f4$variable <- factor(roe_f4$variable,                 # Relevel group factor
                                levels = c("totag_re_feas", "totag_sc_feas", "totdem_feas", "formgmts_feas", "defors_feas","ars_feas"))

roe_f4$ISO <- factor(roe_f4$ISO,                 # Relevel group factor
                                levels = c("PER","BOL","COL","MMR","KAZ","MEX","AUS","DRC","CAN","ARG","RUS","IDN","EU","IND","USA","BRA","CHN"))
proe_f4<- ggplot(data=roe_f4, aes(x = value, y = ISO,fill = variable)) +
  geom_bar(stat="identity")
proe_f4<-proe_f4+theme_bw()+ xlab("Cost-effective mitigation potential (MtCO2e/yr)")+ylab("")+  scale_fill_manual(values=c("#ae1717", "#ce7373","#555453", "#a3cbb5", "#48976c","#156439"), 
                                                                 name="Mitigation category",
                                                                 breaks=c("totag_re_feas", "totag_sc_feas", "totdem_feas", "formgmts_feas", "defors_feas","ars_feas"),
                                                                 labels=c("Agriculture - Reduce emissions", "Agriculture - Sequester carbon", "Demand side", "Forest & other ecosystems - Manage","Forest & other ecosystems - Protect", "Forest & other ecosystems - Restore" ))
proe_f4

write.csv(roe_f4,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f4_output.csv")

roe_f4 %>%
  ggplot(aes(x=forcats::fct_reorder(ISO,value),y=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x="Social Media", y="Number of Users")

#############################################
#figure4 using income data
roe_p<-subset(roe_raw, roe_raw$tot_tech>0)

roe_pc<-roe_p %>% 
    filter(!is.na(`Income group`)) %>% #remove NA income
  group_by(`Income group`) %>% 
  summarise(
    totag_re_feas = sum(totag_re_feas, na.rm=T), 
    totag_sc_feas = sum(totag_sc_feas , na.rm=T),
    totdem_feas= sum(totdem_feas, na.rm=T),
    formgmts_feas= sum(formgmts_feas, na.rm=T),
    defors_feas= sum(defors_feas, na.rm=T),
    ars_feas= sum(ars_feas, na.rm=T))

roe_pc %>%
  arrange(desc(roe_pc$totag_re_feas+roe_pc$totag_sc_feas+roe_pc$totdem_feas+roe_pc$formgmts_feas+roe_pc$defors_feas+roe_pc$ars_feas))
#wide to long
names(roe_pc)[1]<-"Income"
roe_pc<-melt(roe_pc, id.vars=c("Income"))

roe_pc$variable <- factor(roe_pc$variable,                 # Relevel group factor
                          levels = c("totag_re_feas", "totag_sc_feas", "totdem_feas", "formgmts_feas", "defors_feas","ars_feas"))

roe_pc$Income <- factor(roe_pc$Income,                 # Relevel group factor
                     levels = c("High income","Middle income","Low income"))
proe_pc<- ggplot(data=roe_pc, aes(x = value, y = Income,fill = variable)) +
  geom_bar(stat="identity")
proe_pc<-proe_pc+theme_bw()+ xlab("Cost-effective mitigation potential (MtCO2e/yr)")+ylab("")+  scale_fill_manual(values=c("#ae1717", "#ce7373","#555453", "#a3cbb5", "#48976c","#156439"), 
                                                                                                                  name="Mitigation category",                                                                                                                 breaks=c("totag_re_feas", "totag_sc_feas", "totdem_feas", "formgmts_feas", "defors_feas","ars_feas"),
                                                                                                                  labels=c("Agriculture - Reduce emissions", "Agriculture - Sequester carbon", "Demand side", "Forest & other ecosystems - Manage","Forest & other ecosystems - Protect", "Forest & other ecosystems - Restore" ))
proe_pc

write.csv(roe_pc,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/f4_income.csv")

