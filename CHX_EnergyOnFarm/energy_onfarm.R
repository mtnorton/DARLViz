# Energy plots on farm
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

df <- read_csv('./fao/Emissions_totals/Emissions_Totals_E_All_Data.csv')
df <- df %>% filter(Item %in% c('On-farm energy use','On-farm electricity use') & `Element Code`==723113)
df <- df %>% dplyr::select(Area,Item,colnames(df)[98+0:30*3])
df <- df %>% pivot_longer(cols=colnames(df)[3:33],names_to=c("Year"),values_to="Value")
df$Year <- gsub("Y","",df$Year) %>% as.numeric(.)
shareElec <- df %>% pivot_wider(id_cols=everything(),names_from=Item,values_from=Value)
df <- df %>% group_by(Area,Year) %>% summarize(totalEm=sum(Value))
df <- left_join(df,shareElec,by=c('Area','Year'))

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
countryInc <- countryInc %>% mutate(Area=ifelse(Area=="Turkey","T\xfcrkiye",Area))
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

totalE_byIncome <- totalE %>% group_by(`Income group`,Year) %>% summarize(totalEmissionsByIncome=sum(totalEm,na.rm=TRUE))
totalE_byIncome <- totalE_byIncome %>% filter(!is.na(`Income group`))
totalE_byIncome <- totalE_byIncome %>% mutate(totalEmissionsByIncome=totalEmissionsByIncome/1000)
totalE_byIncome$`Income group` <- factor(totalE_byIncome$`Income group`,levels=c("Low income","Middle income","High income" ))

g1 <- ggplot(data=totalE_byIncome) +
  geom_line(aes(x=Year,y=totalEmissionsByIncome,group=`Income group`,color=`Income group`),linewidth=1.5) +
  scale_color_manual(values=c('gold1','orangered1','gray50')) +
  xlab("Year") +
  theme(legend.position="bottom") +
  ylab(bquote('Mt '~CO[2]~ 'eq'))

totalEtop2020 <- totalE %>% filter(Year==2020) 
totalEtop2020 <- totalEtop2020[order(totalEtop2020$totalEm,decreasing=TRUE),]
totalEtop2020 <- totalEtop2020 %>% filter(!is.na(Code))
totalEtop2020 <- totalEtop2020[1:10,] %>% select(Area,totalEm) %>% mutate(totalEm=round(totalEm/1000,0))
totalEtop2020$Area[1] <- "China"


g2 <- ggplot(data=totalEtop2020,aes(x=totalEm,y=reorder(Area,totalEm))) +
  geom_bar(stat='identity', fill='royalblue4') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("") +
  xlab("") +  
  geom_label(aes(label=totalEm),hjust=0.8) +
  theme(axis.text = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggarrange(g1, g2, ncol = 2, nrow = 1)


#------------------------------------

# Share of electricity by income group
# 3/24/23

shareElec_byIncome <- totalE %>% group_by(`Income group`,Year) %>% 
  summarize(enByIncome=sum(`On-farm energy use`,na.rm=TRUE),
            elByIncome=sum(`On-farm electricity use`,na.rm=TRUE))
shareElec_byIncome <- shareElec_byIncome %>% mutate(shareElec=elByIncome/(enByIncome+elByIncome))
shareElec_byIncome <- shareElec_byIncome %>% filter(!is.na(`Income group`))
shareElec_byIncome$`Income group` <- factor(shareElec_byIncome$`Income group`,levels=c("Low income","Middle income","High income" ))

ggplot(data=shareElec_byIncome) +
  geom_line(aes(x=Year,y=shareElec,group=`Income group`,color=`Income group`),size=1.5) +
  scale_color_manual(values=c('gold1','orangered1','gray50'))+
  xlab("Year") +
  theme(legend.position="bottom") +ylab("Share (%)") +
  ggtitle("Share of energy-related emissions from on-farm electricity") +
  theme(legend.position="bottom") 

