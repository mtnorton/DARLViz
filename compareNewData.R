# Compare old FAO pre- and post- production data

# Mike, 5.2.2023

library(dplyr)
library(readr)

setwd('C:/Users/wb383351/WBG/DARL Flagship ASA - WB Group - Norton, Michael/DARLViz')

olddata <- read_csv('./data/Emissions_Totals_E_All_Data.csv')
newdata <- read_csv('./data/Emissions_Pre_Post_Production_E_All_Data_NOFLAG.csv')
newdata <- newdata %>% filter()

olddata <- olddata %>% filter(`Item Code` %in% newdata$`Item Code`)

columnNames <- paste0("Y",1990:2020)
columnNames2 <- c(paste0(columnNames,c('.x')),paste0(columnNames,c('.y')))
mergedData <- left_join(olddata,newdata,by=c('Item Code','Area Code','Element Code')) %>% filter(`Element Code`==723113&Item.x=='Pre- and post- production') %>% dplyr::select('Area.x','Item.x','Element Code',all_of(columnNames2))
