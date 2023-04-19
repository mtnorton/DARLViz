#### This code uses data from "Data import"
#Databases used
###OECD_Agrim_MAFAP_FSS_Fish_TOT=TOT_SUPPORT.xlsx
###OECD_Agrim_MAFAP_FSS_Fish_TOT_INC=TOT_SUPPORT_INC.xlsx

install.packages("ggplot2")

library(ggplot2)
library(scales)
###Create a vector with data from 2017-2021 
OECD_Agrim_MAFAP_FSS_Fish_TOT<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/TOTAL_SUPPORT.xlsx",
                                         sheet="Sheet1")
OECD_Agrim_MAFAP_FSS_Fish_TOT_INC<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/TOTAL_SUPPORT_INC.xlsx",
                                         sheet="Sheet1")

####################TOTAL 
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave <- OECD_Agrim_MAFAP_FSS_Fish_TOT[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT$year>=2017), ]

ls(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave)

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave, select = 
                                                   c(Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                     Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                     Tot_Enabling))
###Average data 2017-2021
ave_2017_2021<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave)

ave_2017_2021

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=6)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021[1],ave_2017_2021[2],ave_2017_2021[3],ave_2017_2021[4],0,0,
          0,ave_2017_2021[5],0,0,ave_2017_2021[7],ave_2017_2021[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 
p+ theme_bw()+ labs(y = "USD Billion", x = "", fill = "")

print(p + theme_bw()+scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                   values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                      "chartreuse3","yellow2"))
                                                                                       + labs(y = "USD Billion", x = "", fill = "")
                                                                                      + theme(axis.text = element_text(size = 12)))

write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f1_total.csv") 

p_d<-ggplot(data=df, aes(x=Sup_type, y=Values, fill = Cat)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(position="stack",stat="identity")
#  geom_text(aes(label = freq2), color="white", position = position_stack(vjust = 0.5))
p_d<-p_d + xlab("")+ylab("USD Billion")+theme(legend.position = "right", axis.text.y = element_text(vjust = -5), 
                                                 axis.text.x = element_text(angle = 0, size = 7, vjust =0.5, margin=margin(5,0,0,0))) 
p_d
#####################################################################################################################################   
#F1 alternative
d<-subset(OECD_Agrim_MAFAP_FSS_Fish_TOT, select=c(year,Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                    Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                    Tot_Enabling))
d<-melt(d, id.vars=c("year" ))
colnames(h)[2]<-"Regions"
d$value<-d$value/1000
pd<-ggplot(d, aes(year,value,group=variable,col=variable)) + labs(y= "Billion USD", x="") + scale_color_discrete(name="")+ #remove legend title
  geom_point() + geom_smooth(span = 0.5) #span determines band width
pd

OECD_Agrim_MAFAP_FSS_Fish_TOT_INC %>% 
  #  filter(!is.na(treated)) %>% 
  group_by(Income_group) %>% #group by year and lend type
  dplyr::summarise(count=n()) 
####################HIC########################################################################
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_HIC <- OECD_Agrim_MAFAP_FSS_Fish_TOT_INC[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$year>=2017&
                                                                              OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group=="High income"), ]

ls(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_HIC)
###Select variables for figure

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_HIC = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_HIC, select = 
                                                   c(Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                     Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                     Tot_Enabling))

###Average data 2017-2021
ave_2017_2021_HIC<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_HIC)

ave_2017_2021_HIC

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=6)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021_HIC[1],ave_2017_2021_HIC[2],ave_2017_2021_HIC[3],ave_2017_2021_HIC[4],0,0,
          0,ave_2017_2021_HIC[5],0,0,ave_2017_2021_HIC[7],ave_2017_2021_HIC[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                         values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))

write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_HIC.csv") 

####################Upper Middle Income

unique(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group)

OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_UMI <- OECD_Agrim_MAFAP_FSS_Fish_TOT_INC[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$year>=2017&
                                                                                      OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group=="Upper middle income"), ]

ls(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_UMI)
###Select variables for figure

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_UMI = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_UMI, select = 
                                                   c(Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                     Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                     Tot_Enabling))

###Average data 2017-2021
ave_2017_2021_UMI<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_UMI)

ave_2017_2021_UMI

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=6)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021_UMI[1],ave_2017_2021_UMI[2],ave_2017_2021_UMI[3],ave_2017_2021_UMI[4],0,0,
          0,ave_2017_2021_UMI[5],0,0,ave_2017_2021_UMI[7],ave_2017_2021_UMI[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p + theme_bw()+scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                         values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))

write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_UMIC.csv") 

####################Lower Middle Income

unique(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group)

OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LMI <- OECD_Agrim_MAFAP_FSS_Fish_TOT_INC[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$year>=2017&
                                                                                      OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group=="Lower middle income"), ]

ls(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LMI)
###Select variables for figure

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LMI = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LMI, select = 
                                                   c(Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                     Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                     Tot_Enabling))

###Average data 2017-2021
ave_2017_2021_LMI<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LMI)

ave_2017_2021_LMI

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=6)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021_LMI[1],ave_2017_2021_LMI[2],ave_2017_2021_LMI[3],ave_2017_2021_LMI[4],0,0,
          0,ave_2017_2021_LMI[5],0,0,ave_2017_2021_LMI[7],ave_2017_2021_LMI[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                         values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))


write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_UMIC.csv") 

####################Low Income

unique(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group)

OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LIC <- OECD_Agrim_MAFAP_FSS_Fish_TOT_INC[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$year>=2017&
                                                                                      OECD_Agrim_MAFAP_FSS_Fish_TOT_INC$Income_group=="Low income"), ]

ls(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LIC)
###Select variables for figure

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LIC = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LIC, select = 
                                                   c(Tot_Dist_Pos_Price,Tot_Dist_Pos_Bud, Tot_FSS,
                                                     Tot_Dist_Neg, Tot_Other, Tot_Cons_Com_Supp,
                                                     Tot_Enabling))

###Average data 2017-2021
ave_2017_2021_LIC<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_LIC)

ave_2017_2021_LIC

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=6)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021_LIC[1],ave_2017_2021_LIC[2],ave_2017_2021_LIC[3],ave_2017_2021_LIC[4],0,0,
          0,ave_2017_2021_LIC[5],0,0,ave_2017_2021_LIC[7],ave_2017_2021_LIC[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                         values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))
write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_LIC.csv") 

####################Total Ag Support###############################################################################################

OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave <- OECD_Agrim_MAFAP_FSS_Fish_TOT[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT$year>=2017), ]

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_Ag = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave, select = 
                                                   c(Tot_Dist_Pos_Price_Ag,Tot_Dist_Pos_Bud_Ag,
                                                     Tot_Dist_NEG_Supp_Ag, Tot_Other_Prod_Supp_Ag, 
                                                     Tot_Enabling_Supp_Ag, Tot_Cons_Com_Supp_Ag))

###Average data 2017-2021
ave_2017_2021_Ag<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_Ag)

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=5)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support","Negative Price Support", "Enabling environment","Consumer and community support"), times=2)
Values<-c(ave_2017_2021_Ag[1],ave_2017_2021_Ag[2],ave_2017_2021_Ag[3],0,0,
          0,ave_2017_2021_Ag[4],0,ave_2017_2021_Ag[5],ave_2017_2021_Ag[6])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p+ theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support","Fossil Fuels","Negative Price Support", "Enabling environment","Consumer and community support"),
                                                                         values=c("cadetblue", "cadetblue2", "firebrick2","chocolate1", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))



write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_ag.csv") 

####################FISH Support############################################################################################################

###Keeping data until 2020 as only Viet Nam has data for 2021

OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_F <- OECD_Agrim_MAFAP_FSS_Fish_TOT[ which(OECD_Agrim_MAFAP_FSS_Fish_TOT$year>=2017&
                                                                            OECD_Agrim_MAFAP_FSS_Fish_TOT$year<2021), ]

ls(OECD_Agrim_MAFAP_FSS_Fish_TOT)

#Select variables to keep
OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_F = subset(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_F, select = 
                                               c(Tot_Dist_Pos_Price_Fish,Tot_Dist_Pos_Bud_Fish,
                                                 Tot_Other_Supp_Fish, Tot_Cons_Com_Supp_Fish,
                                                 Tot_Enabling_Supp_Fish))

###Average data 2017-2020
ave_2017_2020_F<-colMeans(OEDC_Agrim_MAFAP_FSS_Fish_TOTAL_Ave_F)

ave_2017_2020_F

##Create matrix for stacked bar plot
Sup_type<-rep(c("High impact","Moderate or no impact"), each=4)
Cat <-rep(c("Positive Price Support","Positive Budgetary Support", "Enabling environment","Community support"), times=2)
Values<-c(ave_2017_2020_F[1],ave_2017_2020_F[2], 0,0,
          0,ave_2017_2020_F[3],ave_2017_2020_F[5],ave_2017_2020_F[4])/1000
df<-data.frame(Sup_type,Cat,Values)


##Stacked bar plot
p<-ggplot(df, aes(fill=Cat, y=Values, x=Sup_type)) + 
  geom_bar(position="stack", stat="identity") 


print(p + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Positive Price Support","Positive Budgetary Support", "Enabling environment","Community support"),
                                                                         values=c("cadetblue","cadetblue2", 
                                                                                             "chartreuse3","yellow2"))
                                                                                             + labs(y = "USD Billion", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))
write.csv(df, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f2_fish.csv") 

#########################################################################################################################################################

aa<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/Sources of Output Growth by Region_Full Data_data.xlsx",
              sheet="Sheet1")
aa_high<-subset(aa, aa$Region=="High income")
paa_high<-ggplot(aa_high, aes(fill=`Sources of Output Growth`, y=`Growth (%/year)`, x=Decade)) + 
  geom_bar(position="stack", stat="identity") 


print(paa_high + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Input Intensification","TFP", "Land Expansion","Irrigation"),
                                                                                     values=c("cadetblue","cadetblue2", 
                                                                                              "chartreuse3","yellow2"))
      + labs(y = "Growth (%/year)", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))

write.csv(aa, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f3_growth_HIC.csv") 

###########
aa_low<-subset(aa, aa$Region=="Low income")
paa_low<-ggplot(aa_low, aes(fill=`Sources of Output Growth`, y=`Growth (%/year)`, x=Decade)) + 
  geom_bar(position="stack", stat="identity") 


print(paa_low + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Input Intensification","TFP", "Land Expansion","Irrigation"),
                                                                                            values=c("cadetblue","cadetblue2", 
                                                                                                     "chartreuse3","yellow2"))
      + labs(y = "Growth (%/year)", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))

write.csv(aa_low, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f3_growth_LIC.csv") 

###########
aa_upperm<-subset(aa, aa$Region=="High & upper middle income")
paa_upperm<-ggplot(aa_upperm, aes(fill=`Sources of Output Growth`, y=`Growth (%/year)`, x=Decade)) + 
  geom_bar(position="stack", stat="identity") 


print(paa_upperm + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Input Intensification","TFP", "Land Expansion","Irrigation"),
                                                                                           values=c("cadetblue","cadetblue2", 
                                                                                                    "chartreuse3","yellow2"))
      + labs(y = "Growth (%/year)", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))
write.csv(aa_upperm, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f3_growth_UMIC.csv") 

###########
aa_lowerm<-subset(aa, aa$Region=="Low & lower-middle Income")
paa_lowerm<-ggplot(aa_lowerm, aes(fill=`Sources of Output Growth`, y=`Growth (%/year)`, x=Decade)) + 
  geom_bar(position="stack", stat="identity") 


print(paa_lowerm + theme_bw()+ scale_y_continuous(labels = label_comma()) + scale_fill_manual(breaks=c("Input Intensification","TFP", "Land Expansion","Irrigation"),
                                                                                           values=c("cadetblue","cadetblue2", 
                                                                                                    "chartreuse3","yellow2"))
      + labs(y = "Growth (%/year)", x = "", fill = "")
      + theme(axis.text = element_text(size = 12)))
write.csv(aa_lowerm, "C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/santiago/f3_growth_UMIC.csv") 
