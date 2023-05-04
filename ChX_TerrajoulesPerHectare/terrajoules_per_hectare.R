# Terrajoules per hectare
# Started 3/23/23
# by Michael Norton

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(readr)


setwd('C:/Users/wb383351/WBG/Ilyun Koh - data')

df <- read_csv('./request_authors/alex_fao/energy_use9020.csv') %>% filter(Year>=1990)

countryInc<-read_xlsx("./wb_countries/country_income.xlsx", sheet="List of economies")
countryInc <- countryInc %>% rename(Area=Economy)
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="China","China, mainland",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Congo, Rep.","Congo",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Congo, Dem. Rep.","Democratic Republic of the Congo",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Czech Republic","Czechia",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Côte d’Ivoire","C\xf4te d'Ivoire",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Korea, Dem. People's Rep","Democratic People's Republic of Korea",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Korea, Rep.","Republic of Korea",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Egypt, Arab Rep.","Egypt",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Gambia, The","Gambia",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Iran, Islamic Rep.","Iran (Islamic Republic of)",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Kyrgyz Republic","Kyrgyzstan",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Lao PDR","Lao People's Democratic Republic",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Micronesia, Fed. Sts.","Micronesia",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="United States","United States of America",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Yemen, Rep.","Yemen",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Vietnam","Viet Nam",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Bolivia","Bolivia (Plurinational State of)",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Turkey","Türkiye",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Ethiopia","Ethiopia PDR",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Slovak Republic","Slovakia",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="São Tomé and Príncipe","Sao Tome and Principe",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Moldova","Republic of Moldova",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Venezuela, RB","Venezuela (Bolivarian Republic of)",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Tanzania","United Republic of Tanzania",Area))
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="United Kingdom","United Kingdom of Great Britain and Northern Ireland",Area))

totalE <- left_join(df,countryInc,by='Area')
totalE$`Income group` <- gsub('Lower middle income', 'Middle income', totalE$`Income group`)
totalE$`Income group` <- gsub('Upper middle income', 'Middle income', totalE$`Income group`)


landData <- read_csv('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/data/FAOSTAT_data_en_3-24-2023.csv')
landData <- landData %>% filter(Item=="Agriculture") %>% dplyr::select(Area,Year,Value)
#landData <- landData %>% mutate(Area=ifelse(Area=="Türkiye","T\xfcrkiye",Area))

totalE <- left_join(totalE,landData,by=c('Area','Year'))

totalE_byIncome <- totalE %>% group_by(`Income group`,Year) %>% 
  summarize(totalEmissionsByIncome=sum(Value.x,na.rm=TRUE),totalLand=sum(Value.y,na.rm=TRUE))
totalE_byIncome <- totalE_byIncome %>% filter(!is.na(`Income group`))
totalE_byIncome <- totalE_byIncome %>% mutate(totalEmissionsByLand=totalEmissionsByIncome/totalLand)
totalE_byIncome$`Income group` <- factor(totalE_byIncome$`Income group`,levels=c("Low income","Middle income","High income" ))

ggplot(data=totalE_byIncome) +
  geom_line(aes(x=Year,y=totalEmissionsByLand,group=`Income group`,color=`Income group`),linewidth=1.5) +
  scale_color_manual(values=c('gold1','orangered1','gray50')) +
  theme(legend.position="bottom") +
  ggtitle('Terrajoules per hectare over time') +
  xlab("Year") +
  ylab('Terrajoules/hectare')

ggplot(data=totalE_byIncome) +
  geom_line(aes(x=Year,y=totalLand,group=`Income group`,color=`Income group`),linewidth=1.5) +
  scale_color_manual(values=c('gold1','orangered1','gray50')) +
  theme(legend.position="bottom") +
  ggtitle('Hectares by income over time') +
  xlab("Year") +
  ylab('Hectares')

