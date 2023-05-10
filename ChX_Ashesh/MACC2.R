

i_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Copy of Data for MACC_all.xlsx",
                 sheet="MACC_India_v2")
i_raw<-within(i_raw, Mitigation[Mitigation=="Controlling wind/water erosion through contour farming/wind breaks/water flow breaks etc"]<-"Controlling wind/water erosion")

i_raw$Mitigation <- factor(i_raw$Mitigation,                 # Relevel group factor
                   levels = c("Green fodder supplement (ruminants)", "Vermicompost", "Improved diet mgmt of small ruminants  (small ruminants)", 
                              "Molasses Urea Products (ruminants)", 
                              "Laser land levelling", "Biogass", "Increased concentrate feeding (ruminants)", 
                              "Efficicent fertiliser use", "Zero Tillage", "Improved diet (high fibre diet) - pigs", 
                              "Rice water management", "Eliminate residue burning", "Sprinkler/micro-sprinkler irrigation", 
                              "Fertigation", "Controlling wind/water erosion", "Restoration of wind/water eroded land", 
                              "Reclaimation of salinity/Alkalinity", "Monensin pre-mix (ruminants)", 
                              "Reclamination of water logged soil"))


p_i<-ggplot(data=i_raw, aes(x=`GHG savings[Mt Co2e]`, y=`Cost[USD/tco2e]`,fill=Mitigation)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  annotate(geom = 'text',x = 0, y = 270,label = '$100/tco2e line',hjust=0,vjust=.5, size = 3,col="red",family = 'Helvetica', alpha = .5) +
  theme(legend.position = 'none') +
  geom_hline(yintercept=100, col="red", linetype = 2)+
  geom_bar(stat="identity",aes(width=i_raw$width)) 
p_i+ggtitle("India")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
#########using totalcost
i_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Copy of Data for MACC_all.xlsx",
                 sheet="MACC_India_v2_totalcost")
i_raw<-within(i_raw, Mitigation[Mitigation=="Controlling wind/water erosion through contour farming/wind breaks/water flow breaks etc"]<-"Controlling wind/water erosion")

i_raw$Mitigation <- factor(i_raw$Mitigation,                 # Relevel group factor
                           levels = c("Efficicent fertiliser use","Zero Tillage","Rice water management",
                                      "Eliminate residue burning","Laser land levelling","Improved diet (high fibre diet) - pigs",
                                      "Improved diet mgmt of small ruminants  (small ruminants)","Biogass",
                                      "Molasses Urea Products (ruminants)","Fertigation", 
                                      "Sprinkler/micro-sprinkler irrigation","Increased concentrate feeding (ruminants)",
                                      "Green fodder supplement (ruminants)","Restoration of wind/water eroded land",
                                      "Controlling wind/water erosion","Reclaimation of salinity/Alkalinity",
                                      "Monensin pre-mix (ruminants)","Reclamination of water logged soil","Vermicompost"))


p_i<-ggplot(data=i_raw, aes(x=`GHG savings[Mt Co2e]`, y=`Cost[USD/tco2e]`,fill=Mitigation)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity",aes(width=i_raw$width)) 
p_i+ggtitle("India")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
write.csv(i_raw,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/fmacc_india_output.csv")

#######bangladesh 2030
b_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Copy of Data for MACC_all.xlsx",
                 sheet="MACC_BD_2030")

b_raw$Mitigation <- factor(b_raw$Mitigation,                 # Relevel group factor
                           levels = c("Nutrient Management", "Zero-Tillage","Rice Water Management","Short Duration Varieties",
                                      "Straw treatment with urea","Improved management (small ruminant)","Increased concentrate feeding",
                                      "Green fodder supplement","Manure management"))
library(ggbreak)
p_b<-ggplot(data=b_raw, aes(x=`GHG savings[Mt Co2e]`, y=`Cost[USD/tco2e]`,fill=Mitigation)) +  #original
  # geom_col(aes(fill = Region), position = "dodge") +
  annotate(geom = 'text',x = 0, y = 170,label = '$100/tco2e line',hjust=0,vjust=.5, size = 3,col="red",family = 'Helvetica', alpha = .5) +
  theme(legend.position = 'none') +
  geom_hline(yintercept=100, col="red", linetype = 2)+
  geom_bar(stat="identity",aes(width=b_raw$width)) #+
  #scale_y_cut(breaks=c(-44, 1), which=c(1, 3), scales=c(0, 3)) #cut scale to focus
p_b+ggtitle("Bangladesh")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

write.csv(b_raw,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/fmacc_bangladesh_output.csv")

###china_v1
c_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Copy of Data for MACC_all.xlsx",
                 sheet="MACC_chn")
#c_raw$Mitigation <- factor(c_raw$Mitigation,                 # Relevel group factor
#                           levels = c("L5","L2","C2","L3","C4","C7","C1","L4","L1","L8","C5","L7","L9","C3","C6","L6","C8","C9"))
c_raw$Mitigation <- factor(c_raw$Mitigation,                 # Relevel group factor
                           levels = c("Probiotics addition to the diet","Animal breeding","Fertilizer best management practices (wheat & maize) – right time and right placement",
                                      "Ionophores addition to the diet","Fertilizer best management practices (cash crops) – right product, right time and right placement",
                                      "Conservation tillage for upland crops","Fertilizer best management practices – right rate","Tea saponins addition to the diet",
                                      "Anaerobic digestion of manure","Reduction of stocking rate – medium grazing intensity","Enhanced-efficiency fertilizers",
                                      "Grazing prohibition for 35% of grazed grasslands","Reduction of stocking rate – light grazing intensity",
                                      "Fertilizer and water best management in rice paddies","More efficient recycling of organic manure","Lipid addition to the diet",
                                      "Straw addition in upland crops","Biochar addition"))
p_c<-ggplot(data=c_raw, aes(x=`GHG savings[Mt Co2e]`, y=`Cost[USD/tco2e]`,fill=Mitigation)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  geom_bar(stat="identity",aes(width=c_raw$width)) 
p_c+ggtitle("China")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

###china_v2
c_raw<-read_xlsx("C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/Copy of Data for MACC_all.xlsx",
                 sheet="MACC_chn")
c_raw<-subset(c_raw, c_raw$Mitigation_old!="L5"& Mitigation_old!="C9") #remove outliers
#c_raw$Mitigation <- factor(c_raw$Mitigation,                 # Relevel group factor
#                           levels = c("L5","L2","C2","L3","C4","C7","C1","L4","L1","L8","C5","L7","L9","C3","C6","L6","C8","C9"))
c_raw$Mitigation <- factor(c_raw$Mitigation,                 # Relevel group factor
                           levels = c("Animal breeding","Fertilizer best management practices (wheat & maize) – right time and right placement",
                                      "Ionophores addition to the diet","Fertilizer best management practices (cash crops) – right product, right time and right placement",
                                      "Conservation tillage for upland crops","Fertilizer best management practices – right rate","Tea saponins addition to the diet",
                                      "Anaerobic digestion of manure","Reduction of stocking rate – medium grazing intensity","Enhanced-efficiency fertilizers",
                                      "Grazing prohibition for 35% of grazed grasslands","Reduction of stocking rate – light grazing intensity",
                                      "Fertilizer and water best management in rice paddies","More efficient recycling of organic manure","Lipid addition to the diet",
                                      "Straw addition in upland crops"))
p_c<-ggplot(data=c_raw, aes(x=`GHG savings[Mt Co2e]`, y=`Cost[USD/tco2e]`,fill=Mitigation)) + 
  # geom_col(aes(fill = Region), position = "dodge") +
  annotate(geom = 'text',x = 0, y = 120,label = '$100/tco2e line',hjust=0,vjust=.5, size = 3,col="red",family = 'Helvetica', alpha = .5) +
  theme(legend.position = 'none') +
  geom_hline(yintercept=100, col="red", linetype = 2)+
  geom_bar(stat="identity",aes(width=c_raw$width)) 
p_c+ggtitle("China")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
write.csv(c_raw,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/request_authors/ashesh/fmacc_china_output.csv")











































## Input data
Data <- read.table("total_cost.txt", header=T, sep="\t", dec="." ,na.strings="", as.is=TRUE)
Data <- Data[Data[[2]]<0,] ## removing positive values
Data_sorted <- Data[order(Data[[3]]),] ## sorting data and storing in different dataframe


mitigation0 <- Data_sorted$GHG..kg.CO2.1000.## reading GHG colum 
mitigation <- mitigation0 *(-1) /1000000 ## converting t into Mt
costs <- Data_sorted$cost.INR.1000/1000000 ## reading cost and converting into billion Rs
labs <- Data_sorted$Mitigation.options ## reading mitigation options for labels 

## Color Palette
palette(rainbow(dim(Data_sorted)[1]*1.5)) ## colors of bars 

## Bars
barplot(costs, width=mitigation, space=0, yaxt="n", xaxt="n", ylab="Price [billion INR]", xlab="GHG savings [Mt CO2e]",ylim=c(-220, 1420), main="", col=1:dim(Data_sorted)[1])

## Adding straight line
abline(h= 0, col = "black")

## Adding legend
legend(0, 1200, legend=labs, col=1:dim(Data_sorted)[1], pch=seq(15,15,len=dim(Data_sorted)[1]) ,bty="n",cex = 0.6,text.width = 40, ncol=2)

## Boundary box
box("plot")

## Adding axes
axis(1, at= c(0,10, 20, 30,40, 50, 60, 70, 80, 90, 100,150,190, 200,250, 300,350,400),
     labels= c(0,10, 20, 30,40, 50, 60, 70, 80, 90, 100,150,190, 200,250, 300,350,400),
     las=1)

axis(2, at = c(-2400,-2200,-2000,-1800,-1600,-1400,-1200,-1000,-800,-600,-400,-200,-150, -100, -50,  0, 50, 100, 150, 200, 400, 600,800,1000,1200,1400),
     labels = c(-2400,-2200,-2000,-1800,-1600,-1400,-1200,-1000,-800,-600,-400,-200,-150, -100, -50,  0, 50, 100, 150, 200, 400, 600,800,1000,1200,1400),
     las=2)




## NET COST


## Input data
Data <- read.table("net_cost.txt", header=T, sep="\t", dec="." ,na.strings="", as.is=TRUE)
Data <- Data[Data[[2]]<0,] ## removing positive values
Data_sorted <- Data[order(Data[[3]]),] ## sorting data and storing in different dataframe


mitigation0 <- Data_sorted$GHG..kg.CO2.1000.## reading GHG colum 
mitigation <- mitigation0 *(-1) /1000000 ## converting t into Mt
costs <- Data_sorted$net.cost..INR.1000./1000000 ## reading cost and converting into billion Rs
labs <- Data_sorted$Mitigation.options ## reading mitigation options for labels 

## Color Palette
palette(rainbow(dim(Data_sorted)[1]*1.5)) ## colors of bars 

## Bars
barplot(costs, width=mitigation, space=0, yaxt="n", xaxt="n", ylab="Price [billion INR]", xlab=expression("GHG savings [Mt CO"[2]*"e]"),ylim=c(-220, 1420), main="", col=1:dim(Data_sorted)[1])

## Adding straight line
abline(h= 0, col = "black")

## Adding legend
legend(0, 1200, legend=labs, col=1:dim(Data_sorted)[1], pch=seq(15,15,len=dim(Data_sorted)[1]) ,bty="n",cex = 0.6,text.width = 40, ncol=2)

## Boundary box
box("plot")

## Adding axes
axis(1, at= c(0,10, 20, 30,40, 50, 60, 70, 80, 90, 100,150,190, 200,250, 300,350,400),
     labels= c(0,10, 20, 30,40, 50, 60, 70, 80, 90, 100,150,190, 200,250, 300,350,400),
     las=1)

axis(2, at = c(-2400,-2200,-2000,-1800,-1600,-1400,-1200,-1000,-800,-600,-400,-200,-150, -100, -50,  0, 50, 100, 150, 200, 400, 600,800,1000,1200,1400),
     labels = c(-2400,-2200,-2000,-1800,-1600,-1400,-1200,-1000,-800,-600,-400,-200,-150, -100, -50,  0, 50, 100, 150, 200, 400, 600,800,1000,1200,1400),
     las=2)


