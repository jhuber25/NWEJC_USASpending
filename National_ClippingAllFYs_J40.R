############ Libraries #####################################
library(readr)
library(dplyr)

## Initializing dataframe for Total Fed Spend, Total J40 Spend, Percentage of Total Spend
National_TrendDF <- data.frame(matrix(ncol=6, nrow=0))
headers <- c('Year','Total_Obligated','Total_FedSpend','Total_Obligated_J40', 'FedSpend_J40', 'J40_Perc_FedSpend', 'J40_Perc_TotObl')
colnames(National_TrendDF) <- headers




######################### FY2008 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2008_All_Assistance_Full_20250406")
FY2008 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2008_All_Assistance_Full_20250406"), read_csv))
typeof(FY2008$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2008_filt_CFDA <- subset(FY2008, as.numeric(FY2008$cfda_number) %in% Justice40CFDAs & trimws(FY2008$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2008_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2008_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2008_year <- 2008
FY2008_TotObl <- sum(FY2008$total_obligated_amount)
FY2008_FedSpend <- sum(FY2008$federal_action_obligation)
FY2008_TotOblJ40 <- sum(FY2008_filt_CFDA$total_obligated_amount)
FY2008_FedSpendJ40 <- sum(FY2008_filt_CFDA$federal_action_obligation)
FY2008_J40Perc_FedSpend <- FY2008_FedSpendJ40/FY2008_FedSpend
FY2008_J40Perc_TotObl <- FY2008_TotOblJ40/FY2008_TotObl
FY2008_NationalTrend_DF <- data.frame(FY2008_year, FY2008_TotObl, FY2008_FedSpend, FY2008_TotOblJ40, FY2008_FedSpendJ40, FY2008_J40Perc_FedSpend, FY2008_J40Perc_TotObl)
colnames(FY2008_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2008_NationalTrend_DF)
rm(FY2008) ## Remove to save memory
rm(FY2008_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2009 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2009_All_Assistance_Full_20250406")
FY2009 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2009_All_Assistance_Full_20250406"), read_csv))
typeof(FY2009$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2009_filt_CFDA <- subset(FY2009, as.numeric(FY2009$cfda_number) %in% Justice40CFDAs & trimws(FY2009$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2009_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2009_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2009_year <- 2009
FY2009_TotObl <- sum(FY2009$total_obligated_amount)
FY2009_FedSpend <- sum(FY2009$federal_action_obligation)
FY2009_TotOblJ40 <- sum(FY2009_filt_CFDA$total_obligated_amount)
FY2009_FedSpendJ40 <- sum(FY2009_filt_CFDA$federal_action_obligation)
FY2009_J40Perc_FedSpend <- FY2009_FedSpendJ40/FY2009_FedSpend
FY2009_J40Perc_TotObl <- FY2009_TotOblJ40/FY2009_TotObl
FY2009_NationalTrend_DF <- data.frame(FY2009_year, FY2009_TotObl, FY2009_FedSpend, FY2009_TotOblJ40, FY2009_FedSpendJ40, FY2009_J40Perc_FedSpend, FY2009_J40Perc_TotObl)
colnames(FY2009_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2009_NationalTrend_DF)
rm(FY2009) ## Remove to save memory
rm(FY2009_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2010 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2010_All_Assistance_Full_20250406")
FY2010 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2010_All_Assistance_Full_20250406"), read_csv))
typeof(FY2010$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2010_filt_CFDA <- subset(FY2010, as.numeric(FY2010$cfda_number) %in% Justice40CFDAs & trimws(FY2010$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2010_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2010_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2010_year <- 2010
FY2010_TotObl <- sum(FY2010$total_obligated_amount)
FY2010_FedSpend <- sum(FY2010$federal_action_obligation)
FY2010_TotOblJ40 <- sum(FY2010_filt_CFDA$total_obligated_amount)
FY2010_FedSpendJ40 <- sum(FY2010_filt_CFDA$federal_action_obligation)
FY2010_J40Perc_FedSpend <- FY2010_FedSpendJ40/FY2010_FedSpend
FY2010_J40Perc_TotObl <- FY2010_TotOblJ40/FY2010_TotObl
FY2010_NationalTrend_DF <- data.frame(FY2010_year, FY2010_TotObl, FY2010_FedSpend, FY2010_TotOblJ40, FY2010_FedSpendJ40, FY2010_J40Perc_FedSpend, FY2010_J40Perc_TotObl)
colnames(FY2010_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2010_NationalTrend_DF)
rm(FY2010) ## Remove to save memory
rm(FY2010_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)





######################### FY2011 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2011_All_Assistance_Full_20250406")
FY2011 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2011_All_Assistance_Full_20250406"), read_csv))
typeof(FY2011$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2011_filt_CFDA <- subset(FY2011, as.numeric(FY2011$cfda_number) %in% Justice40CFDAs & trimws(FY2011$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2011_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2011_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2011_year <- 2011
FY2011_TotObl <- sum(FY2011$total_obligated_amount)
FY2011_FedSpend <- sum(FY2011$federal_action_obligation)
FY2011_TotOblJ40 <- sum(FY2011_filt_CFDA$total_obligated_amount)
FY2011_FedSpendJ40 <- sum(FY2011_filt_CFDA$federal_action_obligation)
FY2011_J40Perc_FedSpend <- FY2011_FedSpendJ40/FY2011_FedSpend
FY2011_J40Perc_TotObl <- FY2011_TotOblJ40/FY2011_TotObl
FY2011_NationalTrend_DF <- data.frame(FY2011_year, FY2011_TotObl, FY2011_FedSpend, FY2011_TotOblJ40, FY2011_FedSpendJ40, FY2011_J40Perc_FedSpend, FY2011_J40Perc_TotObl)
colnames(FY2011_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2011_NationalTrend_DF)
rm(FY2011) ## Remove to save memory
rm(FY2011_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)




######################### FY2012 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2012_All_Assistance_Full_20250406")
FY2012 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2012_All_Assistance_Full_20250406"), read_csv))
typeof(FY2012$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2012_filt_CFDA <- subset(FY2012, as.numeric(FY2012$cfda_number) %in% Justice40CFDAs & trimws(FY2012$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2012_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2012_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2012_year <- 2012
FY2012_TotObl <- sum(FY2012$total_obligated_amount)
FY2012_FedSpend <- sum(FY2012$federal_action_obligation)
FY2012_TotOblJ40 <- sum(FY2012_filt_CFDA$total_obligated_amount)
FY2012_FedSpendJ40 <- sum(FY2012_filt_CFDA$federal_action_obligation)
FY2012_J40Perc_FedSpend <- FY2012_FedSpendJ40/FY2012_FedSpend
FY2012_J40Perc_TotObl <- FY2012_TotOblJ40/FY2012_TotObl
FY2012_NationalTrend_DF <- data.frame(FY2012_year, FY2012_TotObl, FY2012_FedSpend, FY2012_TotOblJ40, FY2012_FedSpendJ40, FY2012_J40Perc_FedSpend, FY2012_J40Perc_TotObl)
colnames(FY2012_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2012_NationalTrend_DF)
rm(FY2012) ## Remove to save memory
rm(FY2012_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2013 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2013_All_Assistance_Full_20250406")
FY2013 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2013_All_Assistance_Full_20250406"), read_csv))
typeof(FY2013$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2013_filt_CFDA <- subset(FY2013, as.numeric(FY2013$cfda_number) %in% Justice40CFDAs & trimws(FY2013$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2013_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2013_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2013_year <- 2013
FY2013_TotObl <- sum(FY2013$total_obligated_amount)
FY2013_FedSpend <- sum(FY2013$federal_action_obligation)
FY2013_TotOblJ40 <- sum(FY2013_filt_CFDA$total_obligated_amount)
FY2013_FedSpendJ40 <- sum(FY2013_filt_CFDA$federal_action_obligation)
FY2013_J40Perc_FedSpend <- FY2013_FedSpendJ40/FY2013_FedSpend
FY2013_J40Perc_TotObl <- FY2013_TotOblJ40/FY2013_TotObl
FY2013_NationalTrend_DF <- data.frame(FY2013_year, FY2013_TotObl, FY2013_FedSpend, FY2013_TotOblJ40, FY2013_FedSpendJ40, FY2013_J40Perc_FedSpend, FY2013_J40Perc_TotObl)
colnames(FY2013_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2013_NationalTrend_DF)
rm(FY2013) ## Remove to save memory
rm(FY2013_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2014 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2014_All_Assistance_Full_20250406")
FY2014 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2014_All_Assistance_Full_20250406"), read_csv))
typeof(FY2014$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2014_filt_CFDA <- subset(FY2014, as.numeric(FY2014$cfda_number) %in% Justice40CFDAs & trimws(FY2014$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2014_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2014_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2014_year <- 2014
FY2014_TotObl <- sum(FY2014$total_obligated_amount)
FY2014_FedSpend <- sum(FY2014$federal_action_obligation)
FY2014_TotOblJ40 <- sum(FY2014_filt_CFDA$total_obligated_amount)
FY2014_FedSpendJ40 <- sum(FY2014_filt_CFDA$federal_action_obligation)
FY2014_J40Perc_FedSpend <- FY2014_FedSpendJ40/FY2014_FedSpend
FY2014_J40Perc_TotObl <- FY2014_TotOblJ40/FY2014_TotObl
FY2014_NationalTrend_DF <- data.frame(FY2014_year, FY2014_TotObl, FY2014_FedSpend, FY2014_TotOblJ40, FY2014_FedSpendJ40, FY2014_J40Perc_FedSpend, FY2014_J40Perc_TotObl)
colnames(FY2014_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2014_NationalTrend_DF)
rm(FY2014) ## Remove to save memory
rm(FY2014_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2015 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2015_All_Assistance_Full_20250406")
FY2015 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2015_All_Assistance_Full_20250406"), read_csv))
typeof(FY2015$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2015_filt_CFDA <- subset(FY2015, as.numeric(FY2015$cfda_number) %in% Justice40CFDAs & trimws(FY2015$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2015_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2015_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2015_year <- 2015
FY2015_TotObl <- sum(FY2015$total_obligated_amount)
FY2015_FedSpend <- sum(FY2015$federal_action_obligation)
FY2015_TotOblJ40 <- sum(FY2015_filt_CFDA$total_obligated_amount)
FY2015_FedSpendJ40 <- sum(FY2015_filt_CFDA$federal_action_obligation)
FY2015_J40Perc_FedSpend <- FY2015_FedSpendJ40/FY2015_FedSpend
FY2015_J40Perc_TotObl <- FY2015_TotOblJ40/FY2015_TotObl
FY2015_NationalTrend_DF <- data.frame(FY2015_year, FY2015_TotObl, FY2015_FedSpend, FY2015_TotOblJ40, FY2015_FedSpendJ40, FY2015_J40Perc_FedSpend, FY2015_J40Perc_TotObl)
colnames(FY2015_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2015_NationalTrend_DF)
rm(FY2015) ## Remove to save memory
rm(FY2015_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2016 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2016_All_Assistance_Full_20250406")
FY2016 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2016_All_Assistance_Full_20250406"), read_csv))
typeof(FY2016$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2016_filt_CFDA <- subset(FY2016, as.numeric(FY2016$cfda_number) %in% Justice40CFDAs & trimws(FY2016$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2016_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2016_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2016_year <- 2016
FY2016_TotObl <- sum(FY2016$total_obligated_amount)
FY2016_FedSpend <- sum(FY2016$federal_action_obligation)
FY2016_TotOblJ40 <- sum(FY2016_filt_CFDA$total_obligated_amount)
FY2016_FedSpendJ40 <- sum(FY2016_filt_CFDA$federal_action_obligation)
FY2016_J40Perc_FedSpend <- FY2016_FedSpendJ40/FY2016_FedSpend
FY2016_J40Perc_TotObl <- FY2016_TotOblJ40/FY2016_TotObl
FY2016_NationalTrend_DF <- data.frame(FY2016_year, FY2016_TotObl, FY2016_FedSpend, FY2016_TotOblJ40, FY2016_FedSpendJ40, FY2016_J40Perc_FedSpend, FY2016_J40Perc_TotObl)
colnames(FY2016_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2016_NationalTrend_DF)
rm(FY2016) ## Remove to save memory
rm(FY2016_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2017 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2017_All_Assistance_Full_20250406")
FY2017 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2017_All_Assistance_Full_20250406"), read_csv))
typeof(FY2017$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2017_filt_CFDA <- subset(FY2017, as.numeric(FY2017$cfda_number) %in% Justice40CFDAs & trimws(FY2017$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2017_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2017_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2017_year <- 2017
FY2017_TotObl <- sum(FY2017$total_obligated_amount)
FY2017_FedSpend <- sum(FY2017$federal_action_obligation)
FY2017_TotOblJ40 <- sum(FY2017_filt_CFDA$total_obligated_amount)
FY2017_FedSpendJ40 <- sum(FY2017_filt_CFDA$federal_action_obligation)
FY2017_J40Perc_FedSpend <- FY2017_FedSpendJ40/FY2017_FedSpend
FY2017_J40Perc_TotObl <- FY2017_TotOblJ40/FY2017_TotObl
FY2017_NationalTrend_DF <- data.frame(FY2017_year, FY2017_TotObl, FY2017_FedSpend, FY2017_TotOblJ40, FY2017_FedSpendJ40, FY2017_J40Perc_FedSpend, FY2017_J40Perc_TotObl)
colnames(FY2017_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2017_NationalTrend_DF)
rm(FY2017) ## Remove to save memory
rm(FY2017_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2018 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2018_All_Assistance_Full_20250406")
FY2018 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2018_All_Assistance_Full_20250406"), read_csv))
typeof(FY2018$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2018_filt_CFDA <- subset(FY2018, as.numeric(FY2018$cfda_number) %in% Justice40CFDAs & trimws(FY2018$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2018_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2018_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2018_year <- 2018
FY2018_TotObl <- sum(FY2018$total_obligated_amount)
FY2018_FedSpend <- sum(FY2018$federal_action_obligation)
FY2018_TotOblJ40 <- sum(FY2018_filt_CFDA$total_obligated_amount)
FY2018_FedSpendJ40 <- sum(FY2018_filt_CFDA$federal_action_obligation)
FY2018_J40Perc_FedSpend <- FY2018_FedSpendJ40/FY2018_FedSpend
FY2018_J40Perc_TotObl <- FY2018_TotOblJ40/FY2018_TotObl
FY2018_NationalTrend_DF <- data.frame(FY2018_year, FY2018_TotObl, FY2018_FedSpend, FY2018_TotOblJ40, FY2018_FedSpendJ40, FY2018_J40Perc_FedSpend, FY2018_J40Perc_TotObl)
colnames(FY2018_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2018_NationalTrend_DF)
rm(FY2018) ## Remove to save memory
rm(FY2018_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2019 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2019_All_Assistance_Full_20250406")
FY2019 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2019_All_Assistance_Full_20250406"), read_csv))
typeof(FY2019$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2019_filt_CFDA <- subset(FY2019, as.numeric(FY2019$cfda_number) %in% Justice40CFDAs & trimws(FY2019$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2019_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2019_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2019_year <- 2019
FY2019_TotObl <- sum(FY2019$total_obligated_amount)
FY2019_FedSpend <- sum(FY2019$federal_action_obligation)
FY2019_TotOblJ40 <- sum(FY2019_filt_CFDA$total_obligated_amount)
FY2019_FedSpendJ40 <- sum(FY2019_filt_CFDA$federal_action_obligation)
FY2019_J40Perc_FedSpend <- FY2019_FedSpendJ40/FY2019_FedSpend
FY2019_J40Perc_TotObl <- FY2019_TotOblJ40/FY2019_TotObl
FY2019_NationalTrend_DF <- data.frame(FY2019_year, FY2019_TotObl, FY2019_FedSpend, FY2019_TotOblJ40, FY2019_FedSpendJ40, FY2019_J40Perc_FedSpend, FY2019_J40Perc_TotObl)
colnames(FY2019_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2019_NationalTrend_DF)
rm(FY2019) ## Remove to save memory
rm(FY2019_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)



######################### FY2020 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2020_All_Assistance_Full_20250406")
FY2020 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2020_All_Assistance_Full_20250406"), read_csv))
typeof(FY2020$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2020_filt_CFDA <- subset(FY2020, as.numeric(FY2020$cfda_number) %in% Justice40CFDAs & trimws(FY2020$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2020_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2020_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2020_year <- 2020
FY2020_TotObl <- sum(FY2020$total_obligated_amount)
FY2020_FedSpend <- sum(FY2020$federal_action_obligation)
FY2020_TotOblJ40 <- sum(FY2020_filt_CFDA$total_obligated_amount)
FY2020_FedSpendJ40 <- sum(FY2020_filt_CFDA$federal_action_obligation)
FY2020_J40Perc_FedSpend <- FY2020_FedSpendJ40/FY2020_FedSpend
FY2020_J40Perc_TotObl <- FY2020_TotOblJ40/FY2020_TotObl
FY2020_NationalTrend_DF <- data.frame(FY2020_year, FY2020_TotObl, FY2020_FedSpend, FY2020_TotOblJ40, FY2020_FedSpendJ40, FY2020_J40Perc_FedSpend, FY2020_J40Perc_TotObl)
colnames(FY2020_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2020_NationalTrend_DF)
rm(FY2020) ## Remove to save memory
rm(FY2020_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2021 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2021_All_Assistance_Full_20250406")
FY2021 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2021_All_Assistance_Full_20250406"), read_csv))
typeof(FY2021$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2021_filt_CFDA <- subset(FY2021, as.numeric(FY2021$cfda_number) %in% Justice40CFDAs & trimws(FY2021$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2021_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2021_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2021_year <- 2021
FY2021_TotObl <- sum(FY2021$total_obligated_amount)
FY2021_FedSpend <- sum(FY2021$federal_action_obligation)
FY2021_TotOblJ40 <- sum(FY2021_filt_CFDA$total_obligated_amount)
FY2021_FedSpendJ40 <- sum(FY2021_filt_CFDA$federal_action_obligation)
FY2021_J40Perc_FedSpend <- FY2021_FedSpendJ40/FY2021_FedSpend
FY2021_J40Perc_TotObl <- FY2021_TotOblJ40/FY2021_TotObl
FY2021_NationalTrend_DF <- data.frame(FY2021_year, FY2021_TotObl, FY2021_FedSpend, FY2021_TotOblJ40, FY2021_FedSpendJ40, FY2021_J40Perc_FedSpend, FY2021_J40Perc_TotObl)
colnames(FY2021_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2021_NationalTrend_DF)
rm(FY2021) ## Remove to save memory
rm(FY2021_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)




######################### FY2022 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2022_All_Assistance_Full_20250406")
FY2022 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2022_All_Assistance_Full_20250406"), read_csv))
typeof(FY2022$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2022_filt_CFDA <- subset(FY2022, as.numeric(FY2022$cfda_number) %in% Justice40CFDAs & trimws(FY2022$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2022_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2022_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2022_year <- 2022
FY2022_TotObl <- sum(FY2022$total_obligated_amount)
FY2022_FedSpend <- sum(FY2022$federal_action_obligation)
FY2022_TotOblJ40 <- sum(FY2022_filt_CFDA$total_obligated_amount)
FY2022_FedSpendJ40 <- sum(FY2022_filt_CFDA$federal_action_obligation)
FY2022_J40Perc_FedSpend <- FY2022_FedSpendJ40/FY2022_FedSpend
FY2022_J40Perc_TotObl <- FY2022_TotOblJ40/FY2022_TotObl
FY2022_NationalTrend_DF <- data.frame(FY2022_year, FY2022_TotObl, FY2022_FedSpend, FY2022_TotOblJ40, FY2022_FedSpendJ40, FY2022_J40Perc_FedSpend, FY2022_J40Perc_TotObl)
colnames(FY2022_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2022_NationalTrend_DF)
rm(FY2022) ## Remove to save memory
rm(FY2022_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2023 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2023_All_Assistance_Full_20250406")
FY2023 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2023_All_Assistance_Full_20250406"), read_csv))
typeof(FY2023$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2023_filt_CFDA <- subset(FY2023, as.numeric(FY2023$cfda_number) %in% Justice40CFDAs & trimws(FY2023$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2023_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2023_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2023_year <- 2023
FY2023_TotObl <- sum(FY2023$total_obligated_amount)
FY2023_FedSpend <- sum(FY2023$federal_action_obligation)
FY2023_TotOblJ40 <- sum(FY2023_filt_CFDA$total_obligated_amount)
FY2023_FedSpendJ40 <- sum(FY2023_filt_CFDA$federal_action_obligation)
FY2023_J40Perc_FedSpend <- FY2023_FedSpendJ40/FY2023_FedSpend
FY2023_J40Perc_TotObl <- FY2023_TotOblJ40/FY2023_TotObl
FY2023_NationalTrend_DF <- data.frame(FY2023_year, FY2023_TotObl, FY2023_FedSpend, FY2023_TotOblJ40, FY2023_FedSpendJ40, FY2023_J40Perc_FedSpend, FY2023_J40Perc_TotObl)
colnames(FY2023_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2023_NationalTrend_DF)
rm(FY2023) ## Remove to save memory
rm(FY2023_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2024 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2024_All_Assistance_Full_20250406")
FY2024 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2024_All_Assistance_Full_20250406"), read_csv))
typeof(FY2024$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2024_filt_CFDA <- subset(FY2024, as.numeric(FY2024$cfda_number) %in% Justice40CFDAs & trimws(FY2024$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2024_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2024_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2024_year <- 2024
FY2024_TotObl <- sum(FY2024$total_obligated_amount)
FY2024_FedSpend <- sum(FY2024$federal_action_obligation)
FY2024_TotOblJ40 <- sum(FY2024_filt_CFDA$total_obligated_amount)
FY2024_FedSpendJ40 <- sum(FY2024_filt_CFDA$federal_action_obligation)
FY2024_J40Perc_FedSpend <- FY2024_FedSpendJ40/FY2024_FedSpend
FY2024_J40Perc_TotObl <- FY2024_TotOblJ40/FY2024_TotObl
FY2024_NationalTrend_DF <- data.frame(FY2024_year, FY2024_TotObl, FY2024_FedSpend, FY2024_TotOblJ40, FY2024_FedSpendJ40, FY2024_J40Perc_FedSpend, FY2024_J40Perc_TotObl)
colnames(FY2024_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2024_NationalTrend_DF)
rm(FY2024) ## Remove to save memory
rm(FY2024_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()
### Writing to CSV to intermediate progress  ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


######################### FY2025 Block ################################################
setwd("D:/USASpending_AwardDataArchive_Download_20250406/FY2025_All_Assistance_Full_20250406")
FY2025 <- do.call(rbind,
                  lapply(list.files(path = "D:/USASpending_AwardDataArchive_Download_20250406/FY2025_All_Assistance_Full_20250406"), read_csv))
typeof(FY2025$cfda_number)  ## stored in this CSV as character vector
Justice40_ProgramList <- read_csv("D:/CFDA_Sheet/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double, so need to convert character to double
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
Justice40AgenciesList <- read_csv("D:/CFDA_Sheet/Justice40_CoveredAgencies.csv")
Justice40Agencies <- Justice40AgenciesList$Agency
FY2025_filt_CFDA <- subset(FY2025, as.numeric(FY2025$cfda_number) %in% Justice40CFDAs & trimws(FY2025$awarding_agency_name) %in% Justice40Agencies) 
write.csv(FY2025_filt_CFDA, file = "D:/J40_Filtered_Datasets/National/FY2025_National_FilteredJ40.csv", row.names = FALSE) ## this is to export the combined_filtered
FY2025_year <- 2025
FY2025_TotObl <- sum(FY2025$total_obligated_amount)
FY2025_FedSpend <- sum(FY2025$federal_action_obligation)
FY2025_TotOblJ40 <- sum(FY2025_filt_CFDA$total_obligated_amount)
FY2025_FedSpendJ40 <- sum(FY2025_filt_CFDA$federal_action_obligation)
FY2025_J40Perc_FedSpend <- FY2025_FedSpendJ40/FY2025_FedSpend
FY2025_J40Perc_TotObl <- FY2025_TotOblJ40/FY2025_TotObl
FY2025_NationalTrend_DF <- data.frame(FY2025_year, FY2025_TotObl, FY2025_FedSpend, FY2025_TotOblJ40, FY2025_FedSpendJ40, FY2025_J40Perc_FedSpend, FY2025_J40Perc_TotObl)
colnames(FY2025_NationalTrend_DF) <- headers ## make column headers match
National_TrendDF <- rbind(National_TrendDF, FY2025_NationalTrend_DF)
rm(FY2025) ## Remove to save memory
rm(FY2025_filt_CFDA) ## remove to save memory
rm(Justice40_ProgramList)
gc()


### Writing to CSV to save for graphing ###
write.csv(National_TrendDF, file = "D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv", row.names=FALSE)


### See J40 trend graph script for next steps #############
