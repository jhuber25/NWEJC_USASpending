############ Libraries #####################################
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

## Initializing dataframe for Total Fed Spend, Total J40 Spend, Percentage of Total Spend
AZ_TrendDF <- data.frame(matrix(ncol=7, nrow=0))
headers <- c('Year','Total_Obligated','Total_FedSpend','Total_Obligated_J40', 'FedSpend_J40', 'J40_Perc_FedSpend','J40_Perc_TotObligated')
colnames(AZ_TrendDF) <- headers


######################### FY2008 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2008_All_Assistance_Full_20250306")
FY2008 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2008_All_Assistance_Full_20250306"), read_csv))
AZ_FY2008 <- subset(FY2008, FY2008$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2008$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2008_filt_CFDA <- subset(AZ_FY2008, as.numeric(AZ_FY2008$cfda_number) %in% Justice40CFDAs) 
AZ_FY2008_year <- 2008
AZ_FY2008_TotObl <- sum(AZ_FY2008$total_obligated_amount)
AZ_FY2008_FedSpend <- sum(AZ_FY2008$federal_action_obligation)
AZ_FY2008_TotOblJ40 <- sum(AZ_FY2008_filt_CFDA$total_obligated_amount)
AZ_FY2008_FedSpendJ40 <- sum(AZ_FY2008_filt_CFDA$federal_action_obligation)
AZ_FY2008_J40Perc_FedSpend <- AZ_FY2008_FedSpendJ40/AZ_FY2008_FedSpend
AZ_FY2008_J40Perc_TotObl <- AZ_FY2008_TotOblJ40/AZ_FY2008_TotObl
AZ_FY2008_Trend_DF <- data.frame(AZ_FY2008_year, AZ_FY2008_TotObl, AZ_FY2008_FedSpend, AZ_FY2008_TotOblJ40, AZ_FY2008_FedSpendJ40, AZ_FY2008_J40Perc_FedSpend, AZ_FY2008_J40Perc_TotObl)
colnames(AZ_FY2008_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2008_Trend_DF)
write.csv(AZ_FY2008_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2008) ## Remove to save memory
rm(AZ_FY2008_filt_CFDA) ## remove to save memory
rm(FY2008)
gc()


######################### FY2009 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2009_All_Assistance_Full_20250306")
FY2009 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2009_All_Assistance_Full_20250306"), read_csv))
AZ_FY2009 <- subset(FY2009, FY2009$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2009$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2009_filt_CFDA <- subset(AZ_FY2009, as.numeric(AZ_FY2009$cfda_number) %in% Justice40CFDAs) 
AZ_FY2009_year <- 2009
AZ_FY2009_TotObl <- sum(AZ_FY2009$total_obligated_amount)
AZ_FY2009_FedSpend <- sum(AZ_FY2009$federal_action_obligation)
AZ_FY2009_TotOblJ40 <- sum(AZ_FY2009_filt_CFDA$total_obligated_amount)
AZ_FY2009_FedSpendJ40 <- sum(AZ_FY2009_filt_CFDA$federal_action_obligation)
AZ_FY2009_J40Perc_FedSpend <- AZ_FY2009_FedSpendJ40/AZ_FY2009_FedSpend
AZ_FY2009_J40Perc_TotObl <- AZ_FY2009_TotOblJ40/AZ_FY2009_TotObl
AZ_FY2009_Trend_DF <- data.frame(AZ_FY2009_year, AZ_FY2009_TotObl, AZ_FY2009_FedSpend, AZ_FY2009_TotOblJ40, AZ_FY2009_FedSpendJ40, AZ_FY2009_J40Perc_FedSpend, AZ_FY2009_J40Perc_TotObl)
colnames(AZ_FY2009_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2009_Trend_DF)
write.csv(AZ_FY2009_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2009) ## Remove to save memory
rm(AZ_FY2009_filt_CFDA) ## remove to save memory
rm(FY2009)
gc()


######################### FY2010 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2010_All_Assistance_Full_20250306")
FY2010 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2010_All_Assistance_Full_20250306"), read_csv))
AZ_FY2010 <- subset(FY2010, FY2010$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2010$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2010_filt_CFDA <- subset(AZ_FY2010, as.numeric(AZ_FY2010$cfda_number) %in% Justice40CFDAs) 
AZ_FY2010_year <- 2010
AZ_FY2010_TotObl <- sum(AZ_FY2010$total_obligated_amount)
AZ_FY2010_FedSpend <- sum(AZ_FY2010$federal_action_obligation)
AZ_FY2010_TotOblJ40 <- sum(AZ_FY2010_filt_CFDA$total_obligated_amount)
AZ_FY2010_FedSpendJ40 <- sum(AZ_FY2010_filt_CFDA$federal_action_obligation)
AZ_FY2010_J40Perc_FedSpend <- AZ_FY2010_FedSpendJ40/AZ_FY2010_FedSpend
AZ_FY2010_J40Perc_TotObl <- AZ_FY2010_TotOblJ40/AZ_FY2010_TotObl
AZ_FY2010_Trend_DF <- data.frame(AZ_FY2010_year, AZ_FY2010_TotObl, AZ_FY2010_FedSpend, AZ_FY2010_TotOblJ40, AZ_FY2010_FedSpendJ40, AZ_FY2010_J40Perc_FedSpend, AZ_FY2010_J40Perc_TotObl)
colnames(AZ_FY2010_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2010_Trend_DF)
write.csv(AZ_FY2010_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2010) ## Remove to save memory
rm(AZ_FY2010_filt_CFDA) ## remove to save memory
rm(FY2010)
gc()



######################### FY2011 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2011_All_Assistance_Full_20250306")
FY2011 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2011_All_Assistance_Full_20250306"), read_csv))
AZ_FY2011 <- subset(FY2011, FY2011$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2011$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2011_filt_CFDA <- subset(AZ_FY2011, as.numeric(AZ_FY2011$cfda_number) %in% Justice40CFDAs) 
AZ_FY2011_year <- 2011
AZ_FY2011_TotObl <- sum(AZ_FY2011$total_obligated_amount)
AZ_FY2011_FedSpend <- sum(AZ_FY2011$federal_action_obligation)
AZ_FY2011_TotOblJ40 <- sum(AZ_FY2011_filt_CFDA$total_obligated_amount)
AZ_FY2011_FedSpendJ40 <- sum(AZ_FY2011_filt_CFDA$federal_action_obligation)
AZ_FY2011_J40Perc_FedSpend <- AZ_FY2011_FedSpendJ40/AZ_FY2011_FedSpend
AZ_FY2011_J40Perc_TotObl <- AZ_FY2011_TotOblJ40/AZ_FY2011_TotObl
AZ_FY2011_Trend_DF <- data.frame(AZ_FY2011_year, AZ_FY2011_TotObl, AZ_FY2011_FedSpend, AZ_FY2011_TotOblJ40, AZ_FY2011_FedSpendJ40, AZ_FY2011_J40Perc_FedSpend, AZ_FY2011_J40Perc_TotObl)
colnames(AZ_FY2011_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2011_Trend_DF)
write.csv(AZ_FY2011_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2011) ## Remove to save memory
rm(AZ_FY2011_filt_CFDA) ## remove to save memory
rm(FY2011)
gc()


######################### FY2012 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2012_All_Assistance_Full_20250306")
FY2012 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2012_All_Assistance_Full_20250306"), read_csv))
AZ_FY2012 <- subset(FY2012, FY2012$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2012$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2012_filt_CFDA <- subset(AZ_FY2012, as.numeric(AZ_FY2012$cfda_number) %in% Justice40CFDAs) 
AZ_FY2012_year <- 2012
AZ_FY2012_TotObl <- sum(AZ_FY2012$total_obligated_amount)
AZ_FY2012_FedSpend <- sum(AZ_FY2012$federal_action_obligation)
AZ_FY2012_TotOblJ40 <- sum(AZ_FY2012_filt_CFDA$total_obligated_amount)
AZ_FY2012_FedSpendJ40 <- sum(AZ_FY2012_filt_CFDA$federal_action_obligation)
AZ_FY2012_J40Perc_FedSpend <- AZ_FY2012_FedSpendJ40/AZ_FY2012_FedSpend
AZ_FY2012_J40Perc_TotObl <- AZ_FY2012_TotOblJ40/AZ_FY2012_TotObl
AZ_FY2012_Trend_DF <- data.frame(AZ_FY2012_year, AZ_FY2012_TotObl, AZ_FY2012_FedSpend, AZ_FY2012_TotOblJ40, AZ_FY2012_FedSpendJ40, AZ_FY2012_J40Perc_FedSpend, AZ_FY2012_J40Perc_TotObl)
colnames(AZ_FY2012_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2012_Trend_DF)
write.csv(AZ_FY2012_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2012) ## Remove to save memory
rm(AZ_FY2012_filt_CFDA) ## remove to save memory
rm(FY2012)
gc()



######################### FY2013 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2013_All_Assistance_Full_20250306")
FY2013 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2013_All_Assistance_Full_20250306"), read_csv))
AZ_FY2013 <- subset(FY2013, FY2013$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2013$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2013_filt_CFDA <- subset(AZ_FY2013, as.numeric(AZ_FY2013$cfda_number) %in% Justice40CFDAs) 
AZ_FY2013_year <- 2013
AZ_FY2013_TotObl <- sum(AZ_FY2013$total_obligated_amount)
AZ_FY2013_FedSpend <- sum(AZ_FY2013$federal_action_obligation)
AZ_FY2013_TotOblJ40 <- sum(AZ_FY2013_filt_CFDA$total_obligated_amount)
AZ_FY2013_FedSpendJ40 <- sum(AZ_FY2013_filt_CFDA$federal_action_obligation)
AZ_FY2013_J40Perc_FedSpend <- AZ_FY2013_FedSpendJ40/AZ_FY2013_FedSpend
AZ_FY2013_J40Perc_TotObl <- AZ_FY2013_TotOblJ40/AZ_FY2013_TotObl
AZ_FY2013_Trend_DF <- data.frame(AZ_FY2013_year, AZ_FY2013_TotObl, AZ_FY2013_FedSpend, AZ_FY2013_TotOblJ40, AZ_FY2013_FedSpendJ40, AZ_FY2013_J40Perc_FedSpend, AZ_FY2013_J40Perc_TotObl)
colnames(AZ_FY2013_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2013_Trend_DF)
write.csv(AZ_FY2013_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2013) ## Remove to save memory
rm(AZ_FY2013_filt_CFDA) ## remove to save memory
rm(FY2013)
gc()



######################### FY2014 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2014_All_Assistance_Full_20250306")
FY2014 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2014_All_Assistance_Full_20250306"), read_csv))
AZ_FY2014 <- subset(FY2014, FY2014$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2014$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2014_filt_CFDA <- subset(AZ_FY2014, as.numeric(AZ_FY2014$cfda_number) %in% Justice40CFDAs) 
AZ_FY2014_year <- 2014
AZ_FY2014_TotObl <- sum(AZ_FY2014$total_obligated_amount)
AZ_FY2014_FedSpend <- sum(AZ_FY2014$federal_action_obligation)
AZ_FY2014_TotOblJ40 <- sum(AZ_FY2014_filt_CFDA$total_obligated_amount)
AZ_FY2014_FedSpendJ40 <- sum(AZ_FY2014_filt_CFDA$federal_action_obligation)
AZ_FY2014_J40Perc_FedSpend <- AZ_FY2014_FedSpendJ40/AZ_FY2014_FedSpend
AZ_FY2014_J40Perc_TotObl <- AZ_FY2014_TotOblJ40/AZ_FY2014_TotObl
AZ_FY2014_Trend_DF <- data.frame(AZ_FY2014_year, AZ_FY2014_TotObl, AZ_FY2014_FedSpend, AZ_FY2014_TotOblJ40, AZ_FY2014_FedSpendJ40, AZ_FY2014_J40Perc_FedSpend, AZ_FY2014_J40Perc_TotObl)
colnames(AZ_FY2014_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2014_Trend_DF)
write.csv(AZ_FY2014_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2014) ## Remove to save memory
rm(AZ_FY2014_filt_CFDA) ## remove to save memory
rm(FY2014)
gc()



######################### FY2015 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2015_All_Assistance_Full_20250306")
FY2015 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2015_All_Assistance_Full_20250306"), read_csv))
AZ_FY2015 <- subset(FY2015, FY2015$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2015$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2015_filt_CFDA <- subset(AZ_FY2015, as.numeric(AZ_FY2015$cfda_number) %in% Justice40CFDAs) 
AZ_FY2015_year <- 2015
AZ_FY2015_TotObl <- sum(AZ_FY2015$total_obligated_amount)
AZ_FY2015_FedSpend <- sum(AZ_FY2015$federal_action_obligation)
AZ_FY2015_TotOblJ40 <- sum(AZ_FY2015_filt_CFDA$total_obligated_amount)
AZ_FY2015_FedSpendJ40 <- sum(AZ_FY2015_filt_CFDA$federal_action_obligation)
AZ_FY2015_J40Perc_FedSpend <- AZ_FY2015_FedSpendJ40/AZ_FY2015_FedSpend
AZ_FY2015_J40Perc_TotObl <- AZ_FY2015_TotOblJ40/AZ_FY2015_TotObl
AZ_FY2015_Trend_DF <- data.frame(AZ_FY2015_year, AZ_FY2015_TotObl, AZ_FY2015_FedSpend, AZ_FY2015_TotOblJ40, AZ_FY2015_FedSpendJ40, AZ_FY2015_J40Perc_FedSpend, AZ_FY2015_J40Perc_TotObl)
colnames(AZ_FY2015_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2015_Trend_DF)
write.csv(AZ_FY2015_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2015) ## Remove to save memory
rm(AZ_FY2015_filt_CFDA) ## remove to save memory
rm(FY2015)
gc()




######################### FY2016 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2016_All_Assistance_Full_20250306")
FY2016 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2016_All_Assistance_Full_20250306"), read_csv))
AZ_FY2016 <- subset(FY2016, FY2016$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2016$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2016_filt_CFDA <- subset(AZ_FY2016, as.numeric(AZ_FY2016$cfda_number) %in% Justice40CFDAs) 
AZ_FY2016_year <- 2016
AZ_FY2016_TotObl <- sum(AZ_FY2016$total_obligated_amount)
AZ_FY2016_FedSpend <- sum(AZ_FY2016$federal_action_obligation)
AZ_FY2016_TotOblJ40 <- sum(AZ_FY2016_filt_CFDA$total_obligated_amount)
AZ_FY2016_FedSpendJ40 <- sum(AZ_FY2016_filt_CFDA$federal_action_obligation)
AZ_FY2016_J40Perc_FedSpend <- AZ_FY2016_FedSpendJ40/AZ_FY2016_FedSpend
AZ_FY2016_J40Perc_TotObl <- AZ_FY2016_TotOblJ40/AZ_FY2016_TotObl
AZ_FY2016_Trend_DF <- data.frame(AZ_FY2016_year, AZ_FY2016_TotObl, AZ_FY2016_FedSpend, AZ_FY2016_TotOblJ40, AZ_FY2016_FedSpendJ40, AZ_FY2016_J40Perc_FedSpend, AZ_FY2016_J40Perc_TotObl)
colnames(AZ_FY2016_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2016_Trend_DF)
write.csv(AZ_FY2016_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2016) ## Remove to save memory
rm(AZ_FY2016_filt_CFDA) ## remove to save memory
rm(FY2016)
gc()



######################### FY2017 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2017_All_Assistance_Full_20250306")
FY2017 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2017_All_Assistance_Full_20250306"), read_csv))
AZ_FY2017 <- subset(FY2017, FY2017$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2017$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2017_filt_CFDA <- subset(AZ_FY2017, as.numeric(AZ_FY2017$cfda_number) %in% Justice40CFDAs) 
AZ_FY2017_year <- 2017
AZ_FY2017_TotObl <- sum(AZ_FY2017$total_obligated_amount)
AZ_FY2017_FedSpend <- sum(AZ_FY2017$federal_action_obligation)
AZ_FY2017_TotOblJ40 <- sum(AZ_FY2017_filt_CFDA$total_obligated_amount)
AZ_FY2017_FedSpendJ40 <- sum(AZ_FY2017_filt_CFDA$federal_action_obligation)
AZ_FY2017_J40Perc_FedSpend <- AZ_FY2017_FedSpendJ40/AZ_FY2017_FedSpend
AZ_FY2017_J40Perc_TotObl <- AZ_FY2017_TotOblJ40/AZ_FY2017_TotObl
AZ_FY2017_Trend_DF <- data.frame(AZ_FY2017_year, AZ_FY2017_TotObl, AZ_FY2017_FedSpend, AZ_FY2017_TotOblJ40, AZ_FY2017_FedSpendJ40, AZ_FY2017_J40Perc_FedSpend, AZ_FY2017_J40Perc_TotObl)
colnames(AZ_FY2017_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2017_Trend_DF)
write.csv(AZ_FY2017_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2017) ## Remove to save memory
rm(AZ_FY2017_filt_CFDA) ## remove to save memory
rm(FY2017)
gc()




######################### FY2018 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2018_All_Assistance_Full_20250306")
FY2018 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2018_All_Assistance_Full_20250306"), read_csv))
AZ_FY2018 <- subset(FY2018, FY2018$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2018$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2018_filt_CFDA <- subset(AZ_FY2018, as.numeric(AZ_FY2018$cfda_number) %in% Justice40CFDAs) 
AZ_FY2018_year <- 2018
AZ_FY2018_TotObl <- sum(AZ_FY2018$total_obligated_amount)
AZ_FY2018_FedSpend <- sum(AZ_FY2018$federal_action_obligation)
AZ_FY2018_TotOblJ40 <- sum(AZ_FY2018_filt_CFDA$total_obligated_amount)
AZ_FY2018_FedSpendJ40 <- sum(AZ_FY2018_filt_CFDA$federal_action_obligation)
AZ_FY2018_J40Perc_FedSpend <- AZ_FY2018_FedSpendJ40/AZ_FY2018_FedSpend
AZ_FY2018_J40Perc_TotObl <- AZ_FY2018_TotOblJ40/AZ_FY2018_TotObl
AZ_FY2018_Trend_DF <- data.frame(AZ_FY2018_year, AZ_FY2018_TotObl, AZ_FY2018_FedSpend, AZ_FY2018_TotOblJ40, AZ_FY2018_FedSpendJ40, AZ_FY2018_J40Perc_FedSpend, AZ_FY2018_J40Perc_TotObl)
colnames(AZ_FY2018_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2018_Trend_DF)
write.csv(AZ_FY2018_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2018) ## Remove to save memory
rm(AZ_FY2018_filt_CFDA) ## remove to save memory
rm(FY2018)
gc()





######################### FY2019 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2019_All_Assistance_Full_20250306")
FY2019 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2019_All_Assistance_Full_20250306"), read_csv))
AZ_FY2019 <- subset(FY2019, FY2019$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2019$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2019_filt_CFDA <- subset(AZ_FY2019, as.numeric(AZ_FY2019$cfda_number) %in% Justice40CFDAs) 
AZ_FY2019_year <- 2019
AZ_FY2019_TotObl <- sum(AZ_FY2019$total_obligated_amount)
AZ_FY2019_FedSpend <- sum(AZ_FY2019$federal_action_obligation)
AZ_FY2019_TotOblJ40 <- sum(AZ_FY2019_filt_CFDA$total_obligated_amount)
AZ_FY2019_FedSpendJ40 <- sum(AZ_FY2019_filt_CFDA$federal_action_obligation)
AZ_FY2019_J40Perc_FedSpend <- AZ_FY2019_FedSpendJ40/AZ_FY2019_FedSpend
AZ_FY2019_J40Perc_TotObl <- AZ_FY2019_TotOblJ40/AZ_FY2019_TotObl
AZ_FY2019_Trend_DF <- data.frame(AZ_FY2019_year, AZ_FY2019_TotObl, AZ_FY2019_FedSpend, AZ_FY2019_TotOblJ40, AZ_FY2019_FedSpendJ40, AZ_FY2019_J40Perc_FedSpend, AZ_FY2019_J40Perc_TotObl)
colnames(AZ_FY2019_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2019_Trend_DF)
write.csv(AZ_FY2019_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2019) ## Remove to save memory
rm(AZ_FY2019_filt_CFDA) ## remove to save memory
rm(FY2019)
gc()

#### Intermediate Write to CSV since 2020 is causing issues ########
write.csv(AZ_TrendDF, file = "D:/R_Analysis_IntermediateFiles/Arizona/arizonaj40_total_thru2019.csv", row.names=FALSE)


######################### FY2020 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2020_All_Assistance_Full_20250306")
FY2020 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2020_All_Assistance_Full_20250306"), read_csv))
AZ_FY2020 <- subset(FY2020, FY2020$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2020$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2020_filt_CFDA <- subset(AZ_FY2020, as.numeric(AZ_FY2020$cfda_number) %in% Justice40CFDAs) 
AZ_FY2020_year <- 2020
AZ_FY2020_TotObl <- sum(AZ_FY2020$total_obligated_amount)
AZ_FY2020_FedSpend <- sum(AZ_FY2020$federal_action_obligation)
AZ_FY2020_TotOblJ40 <- sum(AZ_FY2020_filt_CFDA$total_obligated_amount)
AZ_FY2020_FedSpendJ40 <- sum(AZ_FY2020_filt_CFDA$federal_action_obligation)
AZ_FY2020_J40Perc_FedSpend <- AZ_FY2020_FedSpendJ40/AZ_FY2020_FedSpend
AZ_FY2020_J40Perc_TotObl <- AZ_FY2020_TotOblJ40/AZ_FY2020_TotObl
AZ_FY2020_Trend_DF <- data.frame(AZ_FY2020_year, AZ_FY2020_TotObl, AZ_FY2020_FedSpend, AZ_FY2020_TotOblJ40, AZ_FY2020_FedSpendJ40, AZ_FY2020_J40Perc_FedSpend, AZ_FY2020_J40Perc_TotObl)
colnames(AZ_FY2020_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2020_Trend_DF)
write.csv(AZ_FY2020_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2020) ## Remove to save memory
rm(AZ_FY2020_filt_CFDA) ## remove to save memory
rm(FY2020)
gc()




######################### FY2021 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2021_All_Assistance_Full_20250306")
FY2021 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2021_All_Assistance_Full_20250306"), read_csv))
AZ_FY2021 <- subset(FY2021, FY2021$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2021$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2021_filt_CFDA <- subset(AZ_FY2021, as.numeric(AZ_FY2021$cfda_number) %in% Justice40CFDAs) 
AZ_FY2021_year <- 2021
AZ_FY2021_TotObl <- sum(AZ_FY2021$total_obligated_amount)
AZ_FY2021_FedSpend <- sum(AZ_FY2021$federal_action_obligation)
AZ_FY2021_TotOblJ40 <- sum(AZ_FY2021_filt_CFDA$total_obligated_amount)
AZ_FY2021_FedSpendJ40 <- sum(AZ_FY2021_filt_CFDA$federal_action_obligation)
AZ_FY2021_J40Perc_FedSpend <- AZ_FY2021_FedSpendJ40/AZ_FY2021_FedSpend
AZ_FY2021_J40Perc_TotObl <- AZ_FY2021_TotOblJ40/AZ_FY2021_TotObl
AZ_FY2021_Trend_DF <- data.frame(AZ_FY2021_year, AZ_FY2021_TotObl, AZ_FY2021_FedSpend, AZ_FY2021_TotOblJ40, AZ_FY2021_FedSpendJ40, AZ_FY2021_J40Perc_FedSpend, AZ_FY2021_J40Perc_TotObl)
colnames(AZ_FY2021_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2021_Trend_DF)
write.csv(AZ_FY2021_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2021) ## Remove to save memory
rm(AZ_FY2021_filt_CFDA) ## remove to save memory
rm(FY2021)
gc()



######################### FY2022 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2022_All_Assistance_Full_20250306")
FY2022 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2022_All_Assistance_Full_20250306"), read_csv))
AZ_FY2022 <- subset(FY2022, FY2022$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2022$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2022_filt_CFDA <- subset(AZ_FY2022, as.numeric(AZ_FY2022$cfda_number) %in% Justice40CFDAs) 
AZ_FY2022_year <- 2022
AZ_FY2022_TotObl <- sum(AZ_FY2022$total_obligated_amount)
AZ_FY2022_FedSpend <- sum(AZ_FY2022$federal_action_obligation)
AZ_FY2022_TotOblJ40 <- sum(AZ_FY2022_filt_CFDA$total_obligated_amount)
AZ_FY2022_FedSpendJ40 <- sum(AZ_FY2022_filt_CFDA$federal_action_obligation)
AZ_FY2022_J40Perc_FedSpend <- AZ_FY2022_FedSpendJ40/AZ_FY2022_FedSpend
AZ_FY2022_J40Perc_TotObl <- AZ_FY2022_TotOblJ40/AZ_FY2022_TotObl
AZ_FY2022_Trend_DF <- data.frame(AZ_FY2022_year, AZ_FY2022_TotObl, AZ_FY2022_FedSpend, AZ_FY2022_TotOblJ40, AZ_FY2022_FedSpendJ40, AZ_FY2022_J40Perc_FedSpend, AZ_FY2022_J40Perc_TotObl)
colnames(AZ_FY2022_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2022_Trend_DF)
write.csv(AZ_FY2022_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2022) ## Remove to save memory
rm(AZ_FY2022_filt_CFDA) ## remove to save memory
rm(FY2022)
gc()



######################### FY2023 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2023_All_Assistance_Full_20250306")
FY2023 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2023_All_Assistance_Full_20250306"), read_csv))
AZ_FY2023 <- subset(FY2023, FY2023$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2023$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2023_filt_CFDA <- subset(AZ_FY2023, as.numeric(AZ_FY2023$cfda_number) %in% Justice40CFDAs) 
AZ_FY2023_year <- 2023
AZ_FY2023_TotObl <- sum(AZ_FY2023$total_obligated_amount)
AZ_FY2023_FedSpend <- sum(AZ_FY2023$federal_action_obligation)
AZ_FY2023_TotOblJ40 <- sum(AZ_FY2023_filt_CFDA$total_obligated_amount)
AZ_FY2023_FedSpendJ40 <- sum(AZ_FY2023_filt_CFDA$federal_action_obligation)
AZ_FY2023_J40Perc_FedSpend <- AZ_FY2023_FedSpendJ40/AZ_FY2023_FedSpend
AZ_FY2023_J40Perc_TotObl <- AZ_FY2023_TotOblJ40/AZ_FY2023_TotObl
AZ_FY2023_Trend_DF <- data.frame(AZ_FY2023_year, AZ_FY2023_TotObl, AZ_FY2023_FedSpend, AZ_FY2023_TotOblJ40, AZ_FY2023_FedSpendJ40, AZ_FY2023_J40Perc_FedSpend, AZ_FY2023_J40Perc_TotObl)
colnames(AZ_FY2023_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2023_Trend_DF)
write.csv(AZ_FY2023_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2023) ## Remove to save memory
rm(AZ_FY2023_filt_CFDA) ## remove to save memory
rm(FY2023)
gc()



######################### FY2024 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2024_All_Assistance_Full_20250306")
FY2024 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2024_All_Assistance_Full_20250306"), read_csv))
AZ_FY2024 <- subset(FY2024, FY2024$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2024$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2024_filt_CFDA <- subset(AZ_FY2024, as.numeric(AZ_FY2024$cfda_number) %in% Justice40CFDAs) 
AZ_FY2024_year <- 2024
AZ_FY2024_TotObl <- sum(AZ_FY2024$total_obligated_amount)
AZ_FY2024_FedSpend <- sum(AZ_FY2024$federal_action_obligation)
AZ_FY2024_TotOblJ40 <- sum(AZ_FY2024_filt_CFDA$total_obligated_amount)
AZ_FY2024_FedSpendJ40 <- sum(AZ_FY2024_filt_CFDA$federal_action_obligation)
AZ_FY2024_J40Perc_FedSpend <- AZ_FY2024_FedSpendJ40/AZ_FY2024_FedSpend
AZ_FY2024_J40Perc_TotObl <- AZ_FY2024_TotOblJ40/AZ_FY2024_TotObl
AZ_FY2024_Trend_DF <- data.frame(AZ_FY2024_year, AZ_FY2024_TotObl, AZ_FY2024_FedSpend, AZ_FY2024_TotOblJ40, AZ_FY2024_FedSpendJ40, AZ_FY2024_J40Perc_FedSpend, AZ_FY2024_J40Perc_TotObl)
colnames(AZ_FY2024_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2024_Trend_DF)
write.csv(AZ_FY2024_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2024) ## Remove to save memory
rm(AZ_FY2024_filt_CFDA) ## remove to save memory
rm(FY2024)
gc()



######################### FY2025 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250306/FY2025_All_Assistance_Full_20250306")
FY2025 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250306/FY2025_All_Assistance_Full_20250306"), read_csv))
AZ_FY2025 <- subset(FY2025, FY2025$primary_place_of_performance_state_name == "AZ" | AZ_FY2009$primary_place_of_performance_state_name == "ARIZONA")
typeof(AZ_FY2025$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
AZ_FY2025_filt_CFDA <- subset(AZ_FY2025, as.numeric(AZ_FY2025$cfda_number) %in% Justice40CFDAs) 
AZ_FY2025_year <- 2025
AZ_FY2025_TotObl <- sum(AZ_FY2025$total_obligated_amount)
AZ_FY2025_FedSpend <- sum(AZ_FY2025$federal_action_obligation)
AZ_FY2025_TotOblJ40 <- sum(AZ_FY2025_filt_CFDA$total_obligated_amount)
AZ_FY2025_FedSpendJ40 <- sum(AZ_FY2025_filt_CFDA$federal_action_obligation)
AZ_FY2025_J40Perc_FedSpend <- AZ_FY2025_FedSpendJ40/AZ_FY2025_FedSpend
AZ_FY2025_J40Perc_TotObl <- AZ_FY2025_TotOblJ40/AZ_FY2025_TotObl
AZ_FY2025_Trend_DF <- data.frame(AZ_FY2025_year, AZ_FY2025_TotObl, AZ_FY2025_FedSpend, AZ_FY2025_TotOblJ40, AZ_FY2025_FedSpendJ40, AZ_FY2025_J40Perc_FedSpend, AZ_FY2025_J40Perc_TotObl)
colnames(AZ_FY2025_Trend_DF) <- headers ## make column headers match
AZ_TrendDF <- rbind(AZ_TrendDF, AZ_FY2025_Trend_DF)
write.csv(AZ_FY2025_filt_CFDA, file = "D:/J40_Filtered_Datasets/Arizona/FY2008_Arizona_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
rm(AZ_FY2025) ## Remove to save memory
rm(AZ_FY2025_filt_CFDA) ## remove to save memory
rm(FY2025)
gc()

### Writing to CSV to save for graphing ###
write.csv(AZ_TrendDF, file = "D:/R_Analysis_IntermediateFiles/Arizona/arizonaj40_total.csv", row.names=FALSE)


### Read in from CSV to save time rather than recreating entire dataset #######
AZTotals_J40 <- read_csv("D:/R_Analysis_IntermediateFiles/Arizona/arizonaj40_total.csv")

## Line plot of total spending federal action vs total obligated ##
metric_comp_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=Total_Obligated, color="Total Obligated"), size=2) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=Total_FedSpend, color="Federal Action Obligated"), size=2)+
  scale_y_continuous(labels = unit_format(unit = " Trillion", prefix = "$", sep="", scale = 1e-12)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending by Field Used") + 
  scale_color_manual(name="", values=c("Total Obligated"="green", "Federal Action Obligated"="red")) + 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
metric_comp_plot

## Just Federal Action Obligation ###
FedSpend_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=Total_FedSpend, color="Federal Obligation"), size=2)+
  scale_y_continuous(limits = c(0,150000000000),labels = unit_format(unit = " Billion", prefix = "$", sep="", scale = 1e-9)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending by FY") + 
  scale_color_manual(name="", values=c("Federal Obligation"="red")) + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
FedSpend_plot

## J40 and total Federal Action Obligation #########
j40_tot_fedaction_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=FedSpend_J40, color="Energy and Environment"), size=2) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=Total_FedSpend, color="Total"), size=2)+
  scale_y_continuous(labels = unit_format(unit = " Billion", prefix = "$", sep="", scale = 1e-9)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending (Energy and Environment and Total)") + 
  scale_color_manual(name="", values=c("Energy and Environment"="blue", "Total"="red")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
j40_tot_fedaction_plot


## Just Federal Action Obligation ###
J40_FedSpend_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=FedSpend_J40, color="E&E Federal Obligation"), size=2)+
  scale_y_continuous(limits = c(0,5000000000),labels = unit_format(unit = " Billion", prefix = "$", sep="", scale = 1e-9)) + 
  ylab("Energy and Environment Federal Spending") + 
  ggtitle("Energy and Environment Federal Spending by FY") + 
  scale_color_manual(name="", values=c("E&E Federal Obligation"="blue")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
J40_FedSpend_plot



## J40  Total Obligation ###
J40_TotObl_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=Total_Obligated_J40, color="E&E Federal Obligation"), size=2)+
  scale_y_continuous(limits = c(0,50000000000),labels = unit_format(unit = " Billion", prefix = "$", sep="", scale = 1e-9)) + 
  ylab("Energy and Environment Federal Spending") + 
  ggtitle("Energy and Environment Federal Spending by FY") + 
  scale_color_manual(name="", values=c("E&E Federal Obligation"="blue")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
J40_TotObl_plot

## Percent of Federal Action Obligation Spent on J40 ##
J40_FedSpend_Perc_plot <- 
  ggplot(AZTotals_J40, aes(x = Year)) + 
  geom_line(data=AZTotals_J40, aes(x=Year, y=J40_Perc_FedSpend, color="E&E Percentage"), size=2)+
  scale_y_continuous(limits = c(0,0.1),labels = scales::percent) + 
  ylab("Energy and Environment Federal Spending Percentage") + 
  ggtitle("Energy and Environment Federal Spending by FY") + 
  scale_color_manual(name="", values=c("E&E Percentage"="turquoise")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(AZTotals_J40$Year), breaks = AZTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
J40_FedSpend_Perc_plot


