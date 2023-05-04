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
#match iso
f_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fao_emission_total4.xlsx",
                 sheet="Sheet1")
#f_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/Emissions_Totals/Emissions_Totals_E_All_Data_NOFLAG2.csv", encoding="UTF-8",check.names = FALSE)
#githubiso<-read.xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/github_countrycode.xlsx",
#                     sheet="Sheet1")
#wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")

f<-merge(x=f_raw, y=country, by.x="ISO", by.y="Code", all.x=T)
f<- f[!is.na(f$Region),] #remove na

#load merged and continent updated (crippa 2021 paper's continent) version
p_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fao_emission_capita1.xlsx",
                 sheet="Sheet1")
p<-merge(x=p_raw, y=country, by.x="ISO", by.y="Code", all.x=T)

world<-f %>% 
  group_by(Region, Year, Item) %>% 
  summarise(ghg_avg=sum(Value))
world<-subset(world, world$Year==2020)
world$ghg_avg<-world$ghg_avg/1000000 #convert to GtCO2
write.csv(world,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fao_emission_crippa/worldemission.csv" )  

world_p<-p %>% 
  group_by(Region, Year, Item) %>% 
  summarise(capita_avg=mean(Value))
world_p<-subset(world_p, world_p$Year==2020)
write.csv(world_p,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fao_emission_crippa/worldemission_capita.csv" )  



##################################################
#map emission _average from 2015_2020
m_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/map/totalemission2015_2020.csv")
m_raw<-m_raw %>% 
  group_by(ISO) %>% 
  summarise(emission_avg=mean(Value))
mp_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/map/emission_percapita2015_2020.csv")
mp_raw<-mp_raw %>% 
  group_by(ISO) %>% 
  summarise(emission_percapita=mean(Value))
m_raw2<-merge(x=m_raw, y=mp_raw, by.x="ISO", by.y="ISO", all.x=T)
write.csv(m_raw2,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/map/emission_map.csv" )

###############################
#pop per capita and total emissions_MACC
#pop
pop_raw<-read.xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/wb_population.xlsx",
                   sheet="Sheet1")
pop_raw$pop1520<-(pop_raw$`2015`+pop_raw$`2016`+pop_raw$`2017`+pop_raw$`2018`+pop_raw$`2019`+pop_raw$`2020`)/6
pop<-subset(pop_raw, select=c(ISO,pop1520))
#generate agrifood emission and total sector emission
ag_em_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/map/totalemission2015_2020.csv")
ag_em_raw<-ag_em_raw %>% 
  group_by(ISO) %>% 
  summarise(ag_em_avg=mean(Value_ag))
tot_em_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/map/totalemission_allsector2015_2020.csv")
tot_em_raw<-subset(tot_em_raw, select=c(ISO, Year, Value_tot))
tot_em_raw<-tot_em_raw %>% 
  group_by(ISO) %>% 
  summarise(tot_em_avg=mean(Value_tot))
emi_raw<-merge(ag_em_raw,tot_em_raw, by=c("ISO"))
emi_raw<-merge(emi_raw,pop, by=c("ISO"))

#wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
emi_raw2<-merge(x=emi_raw, y=country, by.x="ISO", by.y="Code", all.x=T)
emi_raw3<- emi_raw2[!is.na(emi_raw2$pop1520),] #remove na
emi_raw4<-subset(emi_raw3, select=c(ag_em_avg,tot_em_avg,pop1520,Region2)) #use region2 that separates high income in EAP and ECA

emi_raw4<-emi_raw4 %>% 
  group_by(Region2) %>% 
  summarise(ag_em_avg=sum(ag_em_avg),
            tot_em_avg=sum(tot_em_avg),
            pop1520=sum(pop1520))
emi_raw4$ag_pcap<-emi_raw4$ag_em_avg*1000/emi_raw4$pop1520
emi_raw4$tot_pcap<-emi_raw4$tot_em_avg*1000/emi_raw4$pop1520
write.csv(emi_raw4,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/pop_tonpercap/emi_raw4.csv" )
#MACC paper graph
emi_raw4<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/pop_tonpercap/emi_raw4.csv")


emi_raw4$Region <- factor(emi_raw4$Region,                 # Relevel group factor
                           levels = c("North America",
                                      "East Asia & Pacific_HIC",
                                      "Europe & Central Asia_MLIC",
                                      "Middle East & North Africa",
                                      "Europe & Central Asia_HIC",
                                      "East Asia & Pacific_MLIC",
                                      "Latin America & Caribbean",
                                      "Sub-Saharan Africa",
                                      "South Asia"))
#wide to long
emi_raw5 <- melt(emi_raw4, id.vars=c("date.yr", "group"))

p_emi_raw4<-ggplot(data=emi_raw4, aes(x=`Income group`, y=value, fill = variable)) +
                     # geom_col(aes(fill = Region), position = "dodge") +
                     geom_bar(stat="identity"), aes(x=Population, y=em_percapita,fill=Region)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity",aes(width=width)) 

proe_pa<-ggplot(data=roe_pa, aes(x=`Income group`, y=value, fill = variable)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")


p_m_raw4+ggtitle("")+xlab("Population (Billions)")+ylab("Tonnes/capita")+theme(plot.title = element_text(hjust = 0.5))

######################################################################################################################################################
#fig2
#match iso
a_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fig2/ag_emission9020.xlsx",
                 sheet="Sheet1")
#wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
a<-merge(x=a_raw, y=country, by.x="ISO", by.y="Code", all.x=T)
a<- a[!is.na(a$Region),] #remove na

a$`Income group` <- gsub('Lower middle income', 'Middle income', a$`Income group`)
a$`Income group` <- gsub('Upper middle income', 'Middle income', a$`Income group`)

library('stringr')
a$`Income group` <- str_replace_all(a$`Income group`, 'Lower middle income', 'Middle income')
a$`Income group` <- str_replace_all(a$`Income group`, 'Higher middle income', 'Middle income')
a$Value<-a$Value/1000000
a_2<-a %>% 
  group_by(`Income group`, Year) %>% 
  summarise(ag_em=sum(Value))

a_2$`Income group`[is.na(a_2$`Income group`)] <- 'Global total' #replace NA with text

a_2$`Income group` <- factor(a_2$`Income group`,                 # Relevel group factor
                           levels = c("Global total", "High income", "Middle income","Low income"))
pa_2 <- a_2 %>% #line chart
  ggplot( aes(Year, ag_em, color=`Income group`)) + labs(y= "GtCO2eq") +
  geom_line(size=1.5) + #adjust line thickness 
  theme_bw()

a_2

#area chart
a_2<-a %>% 
  filter(!is.na(`Income group`)) %>% 
  group_by(`Income group`, Year) %>% 
  summarise(ag_em=sum(Value))

a_2$`Income group` <- factor(a_2$`Income group`,                 # Relevel group factor
                             levels = c("High income", "Middle income","Low income"))

pa_2<-ggplot(data=a_2, aes(x=Year, y=ag_em, fill = `Income group`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_area(stat="identity")
pa_2+theme_bw()+labs(y= "GtCO2eq")

write.csv(a_2,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fig2/ag_emission9020_incomegroup.csv")
############same for total emission use a_raw's iso to merge 
b_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/fig2/tot_emission9020.xlsx",
                 sheet="Sheet1")
#wb country list
country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
b<-merge(x=b_raw, y=country, by.x="ISO", by.y="Code", all.x=T)

b$`Income group` <- gsub('Lower middle income', 'Middle income', b$`Income group`)
b$`Income group` <- gsub('Upper middle income', 'Middle income', b$`Income group`)

library('stringr')
b$`Income group` <- str_replace_all(b$`Income group`, 'Lower middle income', 'Middle income')
b$`Income group` <- str_replace_all(b$`Income group`, 'Higher middle income', 'Middle income')
b$Value<-b$Value/1000000
b_2<-b %>% 
  group_by(`Income group`, Year) %>% 
  summarise(tot_em=sum(Value))

b_2$`Income group`[is.na(b_2$`Income group`)] <- 'Global total' #replace NA with text

b_2$`Income group` <- factor(b_2$`Income group`,                 # Relevel group factor
                             levels = c("Global total", "High income", "Middle income","Low income"))
pb_2 <- b_2 %>%
  ggplot( aes(Year, tot_em, color=`Income group`)) + labs(y= "GtCO2eq") +
  geom_line(size=1.5) + #adjust line thickness 
  theme_bw()

pb_2
###################################################################################################################
#financing goes to agriculture
fag_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/financing_ag/financing_ag.csv", check.names = FALSE)
#####
pfag_raw<-fag_raw %>%
  mutate(a = factor(a, levels=c("financing"))) %>%
  ggplot( aes(x = pct, y = a,fill = variable)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_col(position = "fill")


pfag_raw
pfag_raw+ xlab("")+ylab("")+ theme_bw()+theme_void()+ scale_fill_manual(values=c("#999999","#31a354"),
                                                             name="",
                                                            breaks=c("Financing goes to all other sectors", "Financing goes to agriculture"),
                                                            labels=c("Financing goes to all other sectors", "Financing goes to agriculture"))


