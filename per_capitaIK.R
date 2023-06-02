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
#setwd('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/DARLViz/data')
#popdata <- read_csv('C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/Norton, Michael/data/FAOSTAT_population.csv')
popdata<-read.xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/bill/chapter2/wb_population.xlsx",
                   sheet="Sheet1")
popdata<-subset(popdata, select=c(ISO,`2018`,`2019`, `2020`))
#colnames(popdata)[2]<-"Value"
popdata$Value<-(popdata$`2018`+popdata$`2019`+popdata$`2020`)/3 #3 yr avg
popdata<-subset(popdata, select=c(ISO, Value))

setwd('C:/Users/wb383351/WBG/Ilyun Koh - data')
#f_raw<-read_csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/fao/Emissions_totals/Emissions_Totals_E_All_Data_NOFLAG2.csv")
d_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/newppp19902020_051923_2.xlsx",
                 sheet="Sheet1")
d_raw<-subset(d_raw, d_raw$Year==2018|d_raw$Year==2019|d_raw$Year==2020)
d_raw<-subset(d_raw, select=-c(Item))
colnames(d_raw)[5]<-"Item"
d_raw<-d_raw%>% 
  group_by(ISO,Area,Item,Year) %>% 
  summarise(Value=sum(Value))

d_raw2<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood_fu_lu9020/agrifood_fu_lu9020.xlsx",
                  sheet="Sheet1")
d_raw2<-subset(d_raw2, d_raw2$Item!="Drained organic soils (CO2)" &d_raw2$Item!="Drained organic soils (N2O)" )
d_raw2<-subset(d_raw2, d_raw2$Year>=2018)
d_raw2<-subset(d_raw2, d_raw2$Item=="Farm gate"| d_raw2$Item=="Land Use change")
#####test
test<-d_raw2%>%
  group_by(Item)%>%
  summarise(Value = sum(Value))
#####

#f_fg_lu_raw<-read_csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood_fg_lu_2020.csv")
f_raw<-rbind(d_raw,d_raw2)
f_raw<-f_raw%>%
  group_by(ISO,Area,Item)%>%
  summarise(Value = mean(Value))
#write.csv(f_raw,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/newfao/newdataset20162020/agrifood_fg_lu_ppp_2020.csv' )
#cats <- c('Crop Residues','Drained organic soils','Enteric Fermentation','Manure Management','Rice Cultivation',
#          'Savanna fires','Synthetic Fertilizers','On-farm electricity use','Burning - Crop residues','Net Forest conversion',
#          'Fertilizers Manufacturing','Food Household Consumption','Food Packaging','Food Processing',
#          'Food Retail','Food Transport','Waste - agri-food systems')

#f_raw <- f_raw %>% filter(Item %in% cats & `Element Code`==723113) 

#remove bottom because I'm joining by iso
countryInc<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
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

europe<- c('Austria','Belgium','Denmark','Finland','France','Germany','Greece','Hungary','Iceland','Ireland','Isle of Man','Italy',
           'Liechtenstein','Luxembourg','Monaco','Netherlands','Norway','Poland','Portugal','Slovenia','Slovakia','Spain','Sweden',
           'Switzerland','Türkiye','United Kingdom of Great Britain and Northern Ireland','Romania','Bulgaria','Croatia','Estonia','Latvia',
           'Lithuania', 'Albania','Serbia','Montenegro','Bosnia and Herzegovina','North Macedonia')
#countryInc <- countryInc %>% mutate(Region=ifelse(Area %in% c('Japan','Australia','Korea'),'Japan,Australia,Korea',Region))
#countryInc <- countryInc %>% mutate(Region=ifelse(Area %in% europe,'Western Europe',Region))

colnames(f_raw)[1]<-"Code"
colnames(f_raw)[4]<-"total"
colnames(f_raw)[3]<-"Group"
#f_join <- left_join(f_raw,countryInc) %>% filter(!is.na(Region))
f_join<-merge(x=f_raw, y=countryInc, by.x="Code", by.y="Code", all.x=F)
#f_noISO2 <- f_join2[is.na(f_join2$Region),] check unmatched countries
################test###########################################################################
f_join%>%
  group_by(Region)%>%
  summarise(total = sum(total))
f_latin<-subset(f_join, f_join$Region=='Latin America & Caribbean')
f_join_new<-f_join%>%
  group_by(Code, Area.x,Group, Region)%>%
  summarise(total = sum(total))
f_join_new2<-merge(x=f_join_new, y=popdata, by.x="Code", by.y="ISO", all.x=F)
f_join_new2[is.na(f_join_new2$Value),] 
write.csv(f_nojoin,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/Norton, Michael/data/f_nojoin.csv' )

f_agri<-sum(f_raw$Value)
write.csv(f_join,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/Norton, Michael/data/f_joincountries.csv' )
#############################################################


f_totals <- f_join %>% group_by(Region,Group) %>%
  summarize(totalE = sum(total,na.rm=TRUE))
f_totals <- f_totals %>% pivot_wider(names_from=Group,values_from=totalE) #long to wide

#popdata <- left_join(popdata,countryInc) %>% dplyr::select(Area,Region,Value) %>% filter(!is.na(Region))
popdata<-merge(x=popdata, y=countryInc, by.x="ISO", by.y="Code", all.x=F)
popTotals <- popdata %>% group_by(Region) %>% summarize(totalPop=sum(Value))
popTotals$totalPop<-popTotals$totalPop/1000

f_totals <- left_join(f_totals,popTotals,by='Region')

f_totals <- f_totals %>% mutate(pc1=`Farm gate`/totalPop,
                                pc2=`Land Use change`/totalPop,
                                pc3=`Pre- and Post- Production`/totalPop)
f_totals <- f_totals %>% mutate(pc4 = pc1+pc2+pc3)
f_totals <- f_totals %>% arrange(desc(pc4))
f_totals <- f_totals %>% mutate(totalPop=totalPop/1000000)
f_totals$ag_emi<-f_totals$`Farm gate`+f_totals$`Land Use change`+f_totals$`Pre- and Post- Production` #newly added tot sum of ag emissions
f_totals<-f_totals %>% arrange(desc(ag_emi))

xmins <- c(0,cumsum(f_totals$totalPop)[1:6])
xmaxs <- cumsum(f_totals$totalPop)[1:7]
ymins <- cbind(rep(0,7),f_totals$pc1,f_totals$pc1+f_totals$pc2)
ymins <- as.vector(t(ymins)) 
ymaxs <- cbind(f_totals$pc1,f_totals$pc1+f_totals$pc2,f_totals$pc1+f_totals$pc2+f_totals$pc3)
ymaxs <- as.vector(t(ymaxs)) 
df <- as.data.frame(cbind(xmins,xmaxs))
df <- df %>% slice(rep(1:n(), each = 3))
df <- df %>% mutate(ymins=ymins,ymaxs=ymaxs)
df <- df %>% mutate(Item=factor(rep(c('Farm gate', 'Land Use change','Pre- and Post- Production'),7)))
names(df)[5] <- "Item"

ymaxs2 <- cbind(f_totals$pc1,f_totals$pc1+f_totals$pc2,f_totals$pc1+f_totals$pc2+f_totals$pc3)
labels <- as.data.frame(cbind(f_totals$Region,xmaxs,ymaxs2[,3]))
names(labels) <- c("labels1","x1","y1")
labels$x1 <- as.numeric(labels$x1)
labels$y1 <- as.numeric(labels$y1)

df$Item <- factor(df$Item,                 # Relevel group factor
                           levels = c('Pre- and Post- Production','Land Use change','Farm gate'))
ggplot() +
  scale_fill_manual(values=c('#2171b5','#31a354','#cb181d')) +
  geom_rect(data=df,aes(fill=Item,xmin = xmins, xmax = xmaxs, ymin = ymins, ymax = ymaxs),color='goldenrod1',linewidth=0.5)+#,show.legend = FALSE) +
  #geom_text(data=labels,aes(x=x1,y=y1,label=labels1),hjust=-0.03,vjust=-0.01,color='#555555',angle=45,size=3) +
  theme(panel.background = element_blank(),
        legend.title= element_blank()) +
  xlim(c(0,8)) +
  ylim(c(0,6)) +
  xlab('Population (billions)') +
  ylab(bquote('t'~CO[2]~' Equivalent per person'))

ggplot() +
  scale_fill_manual(values=c('#2171b5','#31a354','#cb181d')) +
  geom_rect(data=df,aes(fill=Item,xmin = xmins, xmax = xmaxs, ymin = ymins, ymax = ymaxs),color='goldenrod1',linewidth=0.5)+#,show.legend = FALSE) +
  #geom_text(data=labels,aes(x=x1,y=y1,label=labels1),hjust=-0.03,vjust=-0.01,color='#555555',angle=45,size=3) +
  theme(panel.background = element_blank(),
        legend.title= element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  xlim(c(0,8)) +
  ylim(c(0,6)) +
  xlab('Population (billions)') +
  ylab(bquote('t'~CO[2]~' Equivalent per person'))

theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
      axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
      axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
      axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))

#save data
write.csv(f_totals,'C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/Norton, Michael/data/agrifood emission per-capita by region 2018-2020avg.csv',row.names = FALSE)
