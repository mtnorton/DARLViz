# Load Data
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(haven)
library(openxlsx)
library(tidyverse)

#dat_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/iiasa/AR6_Scenarios_Database_R5_regions_v1.1.csv", check.names = FALSE) #If you add check.names = FALSE as a parameter to read.xxxx() you’ll force R to accept the names of the columns as they are.

#glo_raw <- read_csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/globiom/globiom.csv")
#dat <- pivot_longer(dat, cols=as.character(seq(2000, 2100, by=10)), names_to="Year")

#create percentage function to show %
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

dat_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/malte/Disaster Data.xlsx",
                          sheet="HICs")
colnames(dat_raw)[12] <- "Region_x"
substrRight <- function(x, n){ #create a fcn to extract right characters
  substr(x, nchar(x)-n+1, nchar(x))
}
dat_raw$iso <- substrRight(dat_raw$`Dis No`, 3) #generate iso

country<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/wb_countries/country_income.xlsx", sheet="List of economies")
country<-within(country, Region[Region=="East Asia & Pacific"]<-"EAP")
country<-within(country, Region[Region=="Europe & Central Asia"]<-"ECA")
country<-within(country, Region[Region=="Latin America & Caribbean"]<-"LCR")
country<-within(country, Region[Region=="Middle East & North Africa"]<-"MNA")
country<-within(country, Region[Region=="North America"]<-"NAR")
country<-within(country, Region[Region=="South Asia"]<-"SAR")
country<-within(country, Region[Region=="Sub-Saharan Africa"]<-"SSA")

library('stringr')
country$`Income group` <- str_replace_all(country$`Income group`, 'Lower middle income', 'Middle income')
country$`Income group` <- str_replace_all(country$`Income group`, 'Upper middle income', 'Middle income')

dat<-merge(x=dat_raw, y=country, by.x="iso", by.y="Code", all.x=T)
#dat_na <- dat[!is.na(dat$iso),] #keep matching rows
#region code
#reg_code<-c("EAP","ECA","LCR","MNA", "NAR", "SAR","SSA")
#reg_code <- matrix(reg_code,nrow=7,ncol=1,byrow=TRUE)
#reg_code<-as.data.frame(reg_code)

datv1<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(`Income group`) %>% #group by year and lend type
  summarise(tot_aff = mean(`Total Affected`, na.rm=T)) 

datv2<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(`Income group`) %>% #group by year and lend type
  summarise(cases=n())

datv1<-merge(x=datv1, y=datv2, by.x="Income group", by.y="Income group", all.x=F)
datv1_2<-  datv1[!is.na(datv1$`Income group`),] #keep matching rows
datv1_2$`Income group` <- factor(datv1_2$`Income group`,                 # Relevel group factor
                                levels = c("High income", "Middle income", "Low income"))


p_datv1_2<-ggplot() + 
  geom_bar(data = datv1_2, aes(x=`Income group`, y=cases, fill = `Income group`), stat= "identity",position=position_dodge()) +
  labs(x= "Income group", y= "Total cases count")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p_datv1_2

##################################################
datv3<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(`Income group`, `Disaster Type`) %>% #group by two types
  summarise(tot_aff = mean(`Total Affected`, na.rm=T)) 
  #summarise(cases=n())
datv3<-as.data.frame(datv3)
datv3<-  datv3[!is.na(datv3$`Income group`),] #keep matching rows
datv3<-  datv3[!is.na(datv3$tot_aff),] #keep matching rows

#create freq
datv3<-datv3 %>% 
  group_by(`Income group`) %>% 
  mutate(freq = round(tot_aff / sum(tot_aff), 3)) 
#convert to %
datv3$freq2 <- percent(datv3$freq)

datv3$`Income group` <- factor(datv3$`Income group`,                 # Relevel group factor
                                 levels = c("High income", "Middle income", "Low income"))

pdatv3<-ggplot(data=datv3, aes(x=`Income group`, y=tot_aff, fill = `Disaster Type`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity") +
  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))

pdatv3<-pdatv3 + xlab("")+ylab("Total affected")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5))
pdatv3

#############
#add gdp
gdp_raw<-read.csv("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/gdp/gdp.csv", check.names = FALSE) #If you add check.names = FALSE as a parameter to read.xxxx() you’ll force R to accept the names of the columns as they are.
#wide to long
gdp<-melt(gdp_raw, id.vars=c("Country_Name"))
#year to numeric
gdp$variable<-as.numeric(as.character(gdp$variable)) #need as.character otherwise error due to factor format
colnames(gdp)[1] <-"Income group"
colnames(gdp)[2] <-"Year"
colnames(gdp)[3] <-"GDP"
gdp$GDPbil <- gdp$GDP/1000000000
gdp$`Income group` <- factor(gdp$`Income group`,                 # Relevel group factor
                               levels = c("High income", "Middle income", "Low income"))
pgdp<-ggplot(data=gdp, aes(x=Year, y=GDPbil, color = `Income group`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_line(stat="identity",size=1.5)
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
pgdp<-pgdp + xlab("")+ylab("GDP Bilion USD)")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                                             axis.text.x = element_text(angle = 90, size = 7, vjust =0.5, margin=margin(5,0,0,0))) +
  scale_x_continuous(breaks = seq(min(gdp$Year), max(gdp$Year), by = 10), expand=c(0,0))
#https://stackoverflow.com/questions/58470039/need-help-displaying-every-10th-year-on-x-axis-ggplot-graph
pgdp



datv4<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Year,`Income group`) %>% #group by two types
  summarise(tot_dam = sum(`Total Damages, Adjusted ('000 US$)`, na.rm=T)) 
  #summarise(cases=n())
datv4<-as.data.frame(datv4)
datv4<-  datv4[!is.na(datv4$`Income group`),] #keep matching rows
datv4<-  datv4[!is.na(datv4$tot_dam),] #keep matching rows
#create freq
datv4<-datv4 %>% 
  group_by(Year) %>% 
  mutate(freq = round(tot_dam / sum(tot_dam), 3)) 
#convert to %
datv4$freq2 <- percent(datv4$freq)
datv4$`Income group` <- factor(datv4$`Income group`,                 # Relevel group factor
                               levels = c("High income", "Middle income", "Low income"))
#year to numeric
datv4$Year<-as.numeric(as.character(datv4$Year)) #need as.character otherwise error due to factor format
datv4$tot_dammil<-datv4$tot_dam/1000000

pdatv4<-ggplot(data=datv4, aes(x=Year, y=tot_dammil, color = `Income group`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_line(stat="identity",size=1.5)
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
pdatv4<-pdatv4 + xlab("")+ylab("Total Damages, Adjusted (milion USD)")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                       axis.text.x = element_text(angle = 90, size = 7, vjust =0.5, margin=margin(5,0,0,0))) +
  scale_x_continuous(breaks = seq(min(datv4$Year), max(datv4$Year), by = 10), expand=c(0,0))
#https://stackoverflow.com/questions/58470039/need-help-displaying-every-10th-year-on-x-axis-ggplot-graph
pdatv4

#merge gdp
datv4_2<-merge(datv4, gdp, by.x=c("Income group", "Year"), by.y=c("Income group", "Year"))
datv4_2$dam_pct<-datv4_2$tot_dam/datv4_2$GDP*100
datv4_2$`Income group` <- factor(datv4_2$`Income group`,                 # Relevel group factor
                               levels = c("High income", "Middle income", "Low income"))
pdatv4_2<-ggplot(data=datv4_2, aes(x=Year, y=dam_pct, color = `Income group`)) +
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_line(stat="identity",size=1.5)
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
pdatv4_2<-pdatv4_2 + xlab("")+ylab("Total Damages, Adjusted / GDP [%]")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                                             axis.text.x = element_text(angle = 90, size = 7, vjust =0.5, margin=margin(5,0,0,0))) +
  scale_x_continuous(breaks = seq(min(datv4_2$Year), max(datv4_2$Year), by = 10), expand=c(0,0))
#https://stackoverflow.com/questions/58470039/need-help-displaying-every-10th-year-on-x-axis-ggplot-graph
pdatv4_2



#line and bar chart together
pdatv4_2<-ggplot()+
  geom_bar(data=datv4_2, aes(x=Year, y=GDPbil, fill = `Income group` ), stat= "identity",position=position_dodge()) + 
  geom_line(data = datv4_2, aes(x=Year, y=tot_dammil, group = `Income group`, col=`Income group`))
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
pdatv4_2<-pdatv4_2 + xlab("")+ylab("Total Damages, Adjusted (milion USD)")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                                             axis.text.x = element_text(angle = 90, size = 7, vjust =0.5, margin=margin(5,0,0,0))) +
  scale_x_continuous(breaks = seq(min(datv4_2$Year), max(datv4_2$Year), by = 10), expand=c(0,0))+
  scale_y_continuous(sec.axis = sec_axis(~.))
#https://stackoverflow.com/questions/58470039/need-help-displaying-every-10th-year-on-x-axis-ggplot-graph
pdatv4


p3<-ggplot() + 
  geom_bar(data = p_yr, aes(x=date.yr, y=amount, fill = group ), stat= "identity",position=position_dodge()) + 
  ggtitle("MNA and SMNDR trends") +
  geom_line(data = p_yr, aes(x=date.yr, y=freq, group = group, col=group)) + 
  geom_point(data = p_yr, aes(x=date.yr, y=freq, group = group, col=group))
p3_2 <- p3 + xlab("")+ylab("Billion$")+ scale_y_continuous(sec.axis = sec_axis(~.*1, name = "[%]"))


#######
datv5<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Year,`Disaster Subgroup`) %>% #group by two types
  summarise(tot_dam = sum(`Total Damages, Adjusted ('000 US$)`, na.rm=T)) 
#summarise(cases=n())
datv5<-as.data.frame(datv5)
datv5<-  datv5[!is.na(datv5$`Disaster Subgroup`),] #keep matching rows
datv5<-  datv5[!is.na(datv5$tot_dam),] #keep matching rows
#create freq
datv5<-datv5 %>% 
  group_by(Year) %>% 
  mutate(freq = round(tot_dam / sum(tot_dam), 3)) 
#convert to %
datv5$freq2 <- percent(datv5$freq)

#year to numeric
datv5$Year<-as.numeric(as.character(datv5$Year)) #need as.character otherwise error due to factor format

pdatv5<-ggplot(data=datv5, aes(x=Year, y=tot_dam, fill = `Disaster Subgroup`)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity")
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
pdatv5<-pdatv5 + xlab("")+ylab("Total Damages, Adjusted ('000 US$)")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                       axis.text.x = element_text(angle = 90, size = 7, vjust =0.5, margin=margin(5,0,0,0))) +
  scale_x_continuous(breaks = seq(min(datv5$Year), max(datv5$Year), by = 10), expand=c(0,0))
#https://stackoverflow.com/questions/58470039/need-help-displaying-every-10th-year-on-x-axis-ggplot-graph
pdatv5
#########################
#linear line
datv6<-dat %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Year) %>% #group by two types
  summarise(tot_dam = sum(`Total Damages, Adjusted ('000 US$)`, na.rm=T)) 
#summarise(cases=n())
datv5<-as.data.frame(datv5)
lm<-lm(Year~tot_dam, data=datv5)
summary(lm) 


