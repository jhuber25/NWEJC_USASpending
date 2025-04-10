############ Libraries #####################################
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

##### Load in data from Stats America Measuring Distress Tool Data Download ########
unemp_rate_24 <- read_csv("D:/Counties/RAW_DATA/Measuring_Distress_ibrc_eda/Measuring Distress - US, States, Counties - 24-Month Unemployment Rate.csv")
bea_pcpi <- read_csv("D:/Counties/RAW_DATA/Measuring_Distress_ibrc_eda/Measuring Distress - US, States, and Counties - BEA Per Capita Personal Income.csv")
acs_pcmi <- read_csv("D:/Counties/RAW_DATA/Measuring_Distress_ibrc_eda/Measuring Distress - US, States, and Counties - ACS Per Capita Money Income.csv")

### Visual inspection, most recent ACS is 2022, most recent BEA is 2023################
#### Going with BEA, Bobby also used BEA ################################
######## BEA is released in the spring (april) per IBRC EDA Distress Tool Metadata document #######
######## Electing to use March or April 24 month unemployment to match ######


#### Subsetting to just 2023 bea data ########
bea_pcpi_2023 <- subset(bea_pcpi, Year == 2023)

#### Subsetting to just April 2023 unemployment rate data #########
unemp_rate_apr_2023 <- subset(unemp_rate_24, Year == 2023 & Month == '04')

### Creating robust FIPS field for export/join to GIS, just adding G to full fips to prevent conversion to numeric ####
bea_pcpi_2023$nwejc_geoid_fips <- paste('G',bea_pcpi_2023$Statefips, bea_pcpi_2023$Countyfips, sep="")
unemp_rate_apr_2023$nwejc_geoid_fips <- paste('G',unemp_rate_apr_2023$Statefips, unemp_rate_apr_2023$Countyfips, sep="")

### Joining bea pcpi and unemployment rate for thresholding analysis ####
eda_distress_2023 <- merge(x = bea_pcpi_2023, y = unemp_rate_apr_2023, by = "nwejc_geoid_fips", all = TRUE)
eda_distress_2023$distress_threshold <- ifelse(eda_distress_2023$`Threshold Calculation.x`< 80 | eda_distress_2023$`Threshold Calculation.y`> 1, "Distressed", "Not Distressed")

### making .x and .y in column names useful, removing bad characters before joining to gis file ### 
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = "\\.x", replacement = "_bea")  
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = "\\.y", replacement = "_unemp")  
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = " ", replacement = "_")  
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = "(([)]))", replacement = "")  
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = "(([()]))", replacement = "")  
names(eda_distress_2023) <- gsub(x = names(eda_distress_2023), pattern = "-", replacement = "")  

### writing to CSV to join to GIS file ###
write.csv(eda_distress_2023, 'D:/Counties/Tabular/Working_files/eda_distress_2023.csv', row.names=FALSE)













