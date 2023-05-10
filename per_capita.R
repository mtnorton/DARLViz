## Per-capita food system emissions by region
# Started 3/31/23
# by Michael Norton

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(readxl)

setwd('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/DARLViz/data')
popdata <- read_csv('FAOSTAT_population.csv')

setwd('C:/Users/wb383351/WBG/Ilyun Koh - data')
f_raw<-read_csv("./fao/Emissions_Totals/Emissions_Totals_E_All_Data_NOFLAG2.csv")

cats <- c('Crop Residues','Drained organic soils','Enteric Fermentation','Manure Management','Rice Cultivation',
          'Savanna fires','Synthetic Fertilizers','On-farm electricity use','Burning - Crop residues','Net Forest conversion',
          'Fertilizers Manufacturing','Food Household Consumption','Food Packaging','Food Processing',
          'Food Retail','Food Transport','Waste - agri-food systems')

f_raw <- f_raw %>% filter(Item %in% cats & `Element Code`==723113) 

subgroup <- c(1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,3)
subgroups <- c("Farm-gate emissions","Land-use change","Pre- and post-production")
subgroup <- subgroups[subgroup]
subgroup <- cbind(subgroup,cats)
colnames(subgroup) <- c("Group","Item")

f_raw <- left_join(f_raw,as.data.frame(subgroup),by='Item')

f_grouped <- f_raw %>% group_by(Area,Group) %>% summarize(total=sum(`2020`,na.rm=TRUE))

f_grouped2 <- f_raw %>% group_by(Area,Item) %>% summarize(total2020=sum(`2020`,na.rm=TRUE),total1990=sum(`1990`,na.rm=TRUE))
f_grouped2 <- f_grouped2 %>% mutate(percChange=(total2020-total1990)/total1990)
f_grouped2 <- f_grouped2 %>% mutate(diff=(total2020-total1990))
f_grouped2 <- f_grouped2[order(f_grouped2$diff,decreasing=TRUE),]


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

europe<- c('Austria','Belgium','Denmark','Finland','France','Germany','Greece','Hungary','Iceland','Ireland','Isle of Man','Italy',
           'Liechtenstein','Luxembourg','Monaco','Netherlands','Norway','Poland','Portugal','Slovenia','Slovakia','Spain','Sweden',
           'Switzerland','Türkiye','United Kingdom of Great Britain and Northern Ireland','Romania','Bulgaria','Croatia','Estonia','Latvia',
           'Lithuania', 'Albania','Serbia','Montenegro','Bosnia and Herzegovina','North Macedonia')
#countryInc <- countryInc %>% mutate(Region=ifelse(Area %in% c('Japan','Australia','Korea'),'Japan,Australia,Korea',Region))
#countryInc <- countryInc %>% mutate(Region=ifelse(Area %in% europe,'Western Europe',Region))

f_join <- left_join(f_grouped,countryInc) %>% filter(!is.na(Region))

f_totals <- f_join %>% group_by(Region,Group) %>%
  summarize(totalE = sum(total,na.rm=TRUE))
f_totals <- f_totals %>% pivot_wider(names_from=Group,values_from=totalE)


popdata <- left_join(popdata,countryInc) %>% dplyr::select(Area,Region,Value) %>% filter(!is.na(Region))
popTotals <- popdata %>% group_by(Region) %>% summarize(totalPop=sum(Value))

f_totals <- left_join(f_totals,popTotals,by='Region')

f_totals <- f_totals %>% mutate(pc1=`Farm-gate emissions`/totalPop,
                                pc2=`Land-use change`/totalPop,
                                pc3=`Pre- and post-production`/totalPop)
f_totals <- f_totals %>% mutate(pc4 = pc1+pc2+pc3)
f_totals <- f_totals %>% arrange(desc(pc4))
f_totals <- f_totals %>% mutate(totalPop=totalPop/1000000)

xmins <- c(0,cumsum(f_totals$totalPop)[1:6])
xmaxs <- cumsum(f_totals$totalPop)[1:7]
ymins <- cbind(rep(0,7),f_totals$pc1,f_totals$pc1+f_totals$pc2)
ymins <- as.vector(t(ymins)) 
ymaxs <- cbind(f_totals$pc1,f_totals$pc1+f_totals$pc2,f_totals$pc1+f_totals$pc2+f_totals$pc3)
ymaxs <- as.vector(t(ymaxs)) 
df <- as.data.frame(cbind(xmins,xmaxs))
df <- df %>% slice(rep(1:n(), each = 3))
df <- df %>% mutate(ymins=ymins,ymaxs=ymaxs)
df <- df %>% mutate(Item=factor(rep(c('Farm-gate emissions', 'Land-use change','Pre- and post-production'),7)))
names(df)[5] <- "Item"

ymaxs2 <- cbind(f_totals$pc1,f_totals$pc1+f_totals$pc2,f_totals$pc1+f_totals$pc2+f_totals$pc3)
labels <- as.data.frame(cbind(f_totals$Region,xmaxs,ymaxs2[,3]))
names(labels) <- c("labels1","x1","y1")
labels$x1 <- as.numeric(labels$x1)
labels$y1 <- as.numeric(labels$y1)


ggplot() +
  scale_fill_manual(values=c('#cb181d','#31a354','#2171b5')) +
  geom_rect(data=df,aes(fill=Item,xmin = xmins, xmax = xmaxs, ymin = ymins, ymax = ymaxs),color='goldenrod1',linewidth=0.5)+#,show.legend = FALSE) +
  geom_text(data=labels,aes(x=x1,y=y1,label=labels1),hjust=-0.03,vjust=-0.01,color='#555555',angle=45,size=3) +
  theme(panel.background = element_blank(),
        legend.title= element_blank()) +
  xlim(c(0,8)) +
  ylim(c(0,6)) +
  xlab('Population (billions)') +
  ylab(bquote('t'~CO[2]~' Equivalent per person'))

setwd('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/data')
write.csv(f_totals,'./per_capita_data.csv',row.names = FALSE)
  