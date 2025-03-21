### Making Individual Spreadsheets to Join to GIS Data ###
## Run first two blocks before running any other section ##

### Set up and import files ##
## install.packages('tidyverse')
## install.packages('plotly')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

FY2021_filtered <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2021_combined_filtered/FY2021_combined_filtered.csv")
FY2022_filtered <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2022_combined_filtered/FY2022_combined_filtered.csv")
FY2023_filtered <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2023_combined_filtered/FY2023_combined_filtered.csv")
FY2024_filtered <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2024_combined_filtered/FY2024_combined_filtered.csv")


## Making new field of State-County for aggregation ##

FY2021_filtered$State_County <- paste(FY2021_filtered$primary_place_of_performance_state_name, FY2021_filtered$primary_place_of_performance_county_name, sep= "-")
FY2022_filtered$State_County <- paste(FY2022_filtered$primary_place_of_performance_state_name, FY2022_filtered$primary_place_of_performance_county_name, sep= "-")
FY2023_filtered$State_County <- paste(FY2023_filtered$primary_place_of_performance_state_name, FY2023_filtered$primary_place_of_performance_county_name, sep= "-")
FY2024_filtered$State_County <- paste(FY2024_filtered$primary_place_of_performance_state_name, FY2024_filtered$primary_place_of_performance_county_name, sep= "-")


## Aggregating all federal spending by county ##

FY2021_byCounty <- FY2021_filtered
FY2022_byCounty <- FY2022_filtered
FY2023_byCounty <- FY2023_filtered
FY2024_byCounty <- FY2024_filtered



FY2021_byCounty <- FY2021_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_byCounty <- FY2022_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_byCounty <- FY2023_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_byCounty <- FY2024_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))





## Writing County Aggregated Spending to CSV for ArcGIS ##

write.csv(FY2021_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyCounty.csv", row.names = FALSE)
write.csv(FY2022_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyCounty.csv", row.names = FALSE)
write.csv(FY2023_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyCounty.csv", row.names = FALSE)
write.csv(FY2024_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyCounty.csv", row.names = FALSE)

## Histogram of each by county
hist(FY2021_byCounty$total_J40_fed_spending)



### Total J40 Spending by FY Run to See Number ####
## IMPORTANT: WHY ARE THERE NEGATIVE VALUES? ###

J40Spending_FY2021 <- sum(FY2021_filtered$total_obligated_amount)
J40Spending_FY2022 <- sum(FY2022_filtered$total_obligated_amount)
J40Spending_FY2023 <- sum(FY2023_filtered$total_obligated_amount)
J40Spending_FY2024 <- sum(FY2024_filtered$total_obligated_amount)


## Making Line Chart of the four years of total J40 spending ##
## Making a data frame with the spending totals ##

tot_J40_spending <- c(J40Spending_FY2021, J40Spending_FY2022, J40Spending_FY2023, J40Spending_FY2024)
fiscal_yr <- c(2021, 2022, 2023, 2024)
tot_j40_spend_byyr <- data.frame(fiscal_yr, tot_J40_spending)

## Making the linechart in ggplot2 850 x 500 ##
options(scipen = 999)

## with dots, 0 to max scale
total_linechart_wdots_maxscale <- plotly::ggplotly(ggplot(data=tot_j40_spend_byyr, aes(x=fiscal_yr, y=tot_J40_spending, group=1)) + 
  geom_line(color="#6495ED", linewidth=1)+scale_y_continuous(limits = c(0,50000000000), labels = scales::dollar) + ggtitle("Total Justice 40 Applicable Spending by Fiscal Year") +
  ylab("Total Spending (USD)") + xlab("Fiscal Year") + geom_point(color="#6495ED", size = 2) + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x=element_text(vjust=-1)) + theme(axis.title.y=element_text(vjust=3)))
total_linechart_wdots_maxscale

## without dots, 0 to max scale
total_linechart_nodot_max <- plotly::ggplotly(ggplot(data=tot_j40_spend_byyr, aes(x=fiscal_yr, y=tot_J40_spending, group=1)) + 
  geom_line(color="#6495ED", linewidth=1)+scale_y_continuous(limits = c(0,50000000000), labels = scales::dollar) + ggtitle("Total Justice 40 Applicable Spending by Fiscal Year") +
  ylab("Total Spending (USD)") + xlab("Fiscal Year") + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x=element_text(vjust=-1)) + theme(axis.title.y=element_text(vjust=3)))
total_linechart_nodot_max

## with dots, 30 to 40 scale
total_linechart_wdots_maxscale <- plotly::ggplotly(ggplot(data=tot_j40_spend_byyr, aes(x=fiscal_yr, y=tot_J40_spending, group=1)) + 
  geom_line(color="#6495ED", linewidth=1.5)+scale_y_continuous(limits = c(35000000000,40000000000), labels = scales::dollar) + ggtitle("Total Justice 40 Applicable Spending by Fiscal Year") +
  ylab("Total Spending (USD)") + xlab("Fiscal Year") + geom_point(color="#6495ED", size = 2) + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x=element_text(vjust=-1)) + theme(axis.title.y=element_text(vjust=3)))
total_linechart_wdots_maxscale

## without dots, 30 to 40 scale
total_linechart_nodot_cut <- plotly::ggplotly(ggplot(data=tot_j40_spend_byyr, aes(x=fiscal_yr, y=tot_J40_spending, group=1)) + 
  geom_line(color="#6495ED", linewidth=1.5)+scale_y_continuous(limits = c(30000000000,40000000000), labels = scales::dollar) + ggtitle("Total Justice 40 Applicable Spending by Fiscal Year") +
  ylab("Total Spending (USD)") + xlab("Fiscal Year") + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x=element_text(vjust=-1)) + theme(axis.title.y=element_text(vjust=3)))
total_linechart_nodot_cut








## Aggregating all federal spending by program ##

FY2021_byProgram <- FY2021_filtered
FY2022_byProgram <- FY2022_filtered
FY2023_byProgram <- FY2023_filtered
FY2024_byProgram <- FY2024_filtered



FY2021_byProgram <- FY2021_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_byProgram <- FY2022_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_byProgram <- FY2023_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_byProgram <- FY2024_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))



## Writing Program Aggregated Spending to CSV don't need for arcgis, just for report ##

write.csv(FY2021_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyProgram.csv", row.names = FALSE)
write.csv(FY2022_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyProgram.csv", row.names = FALSE)
write.csv(FY2023_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyProgram.csv", row.names = FALSE)
write.csv(FY2024_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyProgram.csv", row.names = FALSE)


### Aggregating all federal spending by agency ####
FY2021_byAgency <- FY2021_filtered
FY2022_byAgency <- FY2022_filtered
FY2023_byAgency <- FY2023_filtered
FY2024_byAgency <- FY2024_filtered



FY2021_byAgency <- FY2021_byAgency %>%
  group_by(awarding_agency_name) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_byAgency <- FY2022_byAgency %>%
  group_by(awarding_agency_name) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_byAgency <- FY2023_byAgency %>%
  group_by(awarding_agency_name) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_byAgency <- FY2024_byAgency %>%
  group_by(awarding_agency_name) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing Agency Aggregated Spending to CSV don't need for arcgis, just for report ##

write.csv(FY2021_byAgency, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyAgency.csv", row.names = FALSE)
write.csv(FY2022_byAgency, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyAgency.csv", row.names = FALSE)
write.csv(FY2023_byAgency, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyAgency.csv", row.names = FALSE)
write.csv(FY2024_byAgency, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyAgency.csv", row.names = FALSE)


### Making new subset of tribal funding, starting with federally recognized tribes

FY2021_filtered_FedTribs <- subset(FY2021_filtered, FY2021_filtered$business_types_description == "INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (FEDERALLY-RECOGNIZED)")
FY2022_filtered_FedTribs <- subset(FY2022_filtered, FY2022_filtered$business_types_description == "INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (FEDERALLY-RECOGNIZED)")
FY2023_filtered_FedTribs <- subset(FY2023_filtered, FY2023_filtered$business_types_description == "INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (FEDERALLY-RECOGNIZED)")
FY2024_filtered_FedTribs <- subset(FY2024_filtered, FY2024_filtered$business_types_description == "INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (FEDERALLY-RECOGNIZED)")

## Making new field of State-County for aggregation Tribal dataset ##

FY2021_filtered_FedTribs$State_County <- paste(FY2021_filtered_FedTribs$primary_place_of_performance_state_name, FY2021_filtered_FedTribs$primary_place_of_performance_county_name, sep= "-")
FY2022_filtered_FedTribs$State_County <- paste(FY2022_filtered_FedTribs$primary_place_of_performance_state_name, FY2022_filtered_FedTribs$primary_place_of_performance_county_name, sep= "-")
FY2023_filtered_FedTribs$State_County <- paste(FY2023_filtered_FedTribs$primary_place_of_performance_state_name, FY2023_filtered_FedTribs$primary_place_of_performance_county_name, sep= "-")
FY2024_filtered_FedTribs$State_County <- paste(FY2024_filtered_FedTribs$primary_place_of_performance_state_name, FY2024_filtered_FedTribs$primary_place_of_performance_county_name, sep= "-")

## Aggregating all federal Tribal spending by county ##

FY2021_FedTribs_byCounty <- FY2021_filtered_FedTribs
FY2022_FedTribs_byCounty <- FY2022_filtered_FedTribs
FY2023_FedTribs_byCounty <- FY2023_filtered_FedTribs
FY2024_FedTribs_byCounty <- FY2024_filtered_FedTribs



FY2021_FedTribs_byCounty <- FY2021_FedTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_FedTribs_byCounty <- FY2022_FedTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_FedTribs_byCounty <- FY2023_FedTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_FedTribs_byCounty <- FY2024_FedTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing County Aggregated Tribal Spending to CSV for ArcGIS ##

write.csv(FY2021_FedTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyCounty_FedTribs.csv", row.names = FALSE)
write.csv(FY2022_FedTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyCounty_FedTribs.csv", row.names = FALSE)
write.csv(FY2023_FedTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyCounty_FedTribs.csv", row.names = FALSE)
write.csv(FY2024_FedTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyCounty_FedTribs.csv", row.names = FALSE)




### Making new subset, all tribal funding
## Making a list of the business types from Sophie ###
Tribal_BusinessTypes_dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Tribal_Business_Types.csv")
Tribal_BusinessTypes <- Tribal_BusinessTypes_dataset$Business_Type
Tribal_BusinessTypes


## Subsetting 
FY2021_filtered_AllTribs <- subset(FY2021_filtered, FY2021_filtered$business_types_description %in% Tribal_BusinessTypes)
FY2022_filtered_AllTribs <- subset(FY2022_filtered, FY2022_filtered$business_types_description %in% Tribal_BusinessTypes)
FY2023_filtered_AllTribs <- subset(FY2023_filtered, FY2023_filtered$business_types_description %in% Tribal_BusinessTypes)
FY2024_filtered_AllTribs <- subset(FY2024_filtered, FY2024_filtered$business_types_description %in% Tribal_BusinessTypes)


## Making new field of State-County for aggregation Tribal dataset ##

FY2021_filtered_AllTribs$State_County <- paste(FY2021_filtered_AllTribs$primary_place_of_performance_state_name, FY2021_filtered_AllTribs$primary_place_of_performance_county_name, sep= "-")
FY2022_filtered_AllTribs$State_County <- paste(FY2022_filtered_AllTribs$primary_place_of_performance_state_name, FY2022_filtered_AllTribs$primary_place_of_performance_county_name, sep= "-")
FY2023_filtered_AllTribs$State_County <- paste(FY2023_filtered_AllTribs$primary_place_of_performance_state_name, FY2023_filtered_AllTribs$primary_place_of_performance_county_name, sep= "-")
FY2024_filtered_AllTribs$State_County <- paste(FY2024_filtered_AllTribs$primary_place_of_performance_state_name, FY2024_filtered_AllTribs$primary_place_of_performance_county_name, sep= "-")

## Aggregating all federal Tribal spending by county ##

FY2021_AllTribs_byCounty <- FY2021_filtered_AllTribs
FY2022_AllTribs_byCounty <- FY2022_filtered_AllTribs
FY2023_AllTribs_byCounty <- FY2023_filtered_AllTribs
FY2024_AllTribs_byCounty <- FY2024_filtered_AllTribs



FY2021_AllTribs_byCounty <- FY2021_AllTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_AllTribs_byCounty <- FY2022_AllTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_AllTribs_byCounty <- FY2023_AllTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_AllTribs_byCounty <- FY2024_AllTribs_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing County Aggregated Tribal Spending to CSV for ArcGIS ##

write.csv(FY2021_AllTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyCounty_AllTribs.csv", row.names = FALSE)
write.csv(FY2022_AllTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyCounty_AllTribs.csv", row.names = FALSE)
write.csv(FY2023_AllTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyCounty_AllTribs.csv", row.names = FALSE)
write.csv(FY2024_AllTribs_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyCounty_AllTribs.csv", row.names = FALSE)


## Loan Programs by County


FY2021_Loans_byCounty <- FY2021_filtered
FY2022_Loans_byCounty <- FY2022_filtered
FY2023_Loans_byCounty <- FY2023_filtered
FY2024_Loans_byCounty <- FY2024_filtered



FY2021_Loans_byCounty <- FY2021_Loans_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2022_Loans_byCounty <- FY2022_Loans_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2023_Loans_byCounty <- FY2023_Loans_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2024_Loans_byCounty <- FY2024_Loans_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))


## Loan Programs by Program
FY2021_Loans_byProgram <- FY2021_filtered
FY2022_Loans_byProgram <- FY2022_filtered
FY2023_Loans_byProgram <- FY2023_filtered
FY2024_Loans_byProgram <- FY2024_filtered



FY2021_Loans_byProgram <- FY2021_Loans_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2022_Loans_byProgram <- FY2022_Loans_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2023_Loans_byProgram <- FY2023_Loans_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

FY2024_Loans_byProgram <- FY2024_Loans_byProgram %>%
  group_by(cfda_number) %>%
  summarize(total_J40_fed_loan = sum(face_value_of_loan))

## Writing County Aggregated Loan Spending to CSV for ArcGIS ##

write.csv(FY2021_Loans_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_Loans_byProgram.csv", row.names = FALSE)
write.csv(FY2022_Loans_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_Loans_byProgram.csv", row.names = FALSE)
write.csv(FY2023_Loans_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_Loans_byProgram.csv", row.names = FALSE)
write.csv(FY2024_Loans_byProgram, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_Loans_byProgram.csv", row.names = FALSE)


## Making Rural Counties Subset
Rural_Dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Rural_Counties_List.csv")
RuralCounties <- Rural_Dataset$State_County
FY2021_filt_rural <- subset(FY2021_filtered, FY2021_filtered$State_County %in% RuralCounties) 
FY2022_filt_rural <- subset(FY2022_filtered, FY2022_filtered$State_County %in% RuralCounties) 
FY2023_filt_rural <- subset(FY2023_filtered, FY2023_filtered$State_County %in% RuralCounties) 
FY2024_filt_rural <- subset(FY2024_filtered, FY2024_filtered$State_County %in% RuralCounties) 

### Rural Totals by Year
RurTot_J40Spending_FY2021 <- sum(FY2021_filt_rural$total_obligated_amount)
RurTot_J40Spending_FY2022 <- sum(FY2022_filt_rural$total_obligated_amount)
RurTot_J40Spending_FY2023 <- sum(FY2023_filt_rural$total_obligated_amount)
RurTot_J40Spending_FY2024 <- sum(FY2024_filt_rural$total_obligated_amount)

## Exporting Full Rural Datasets
write.csv(FY2021_filt_rural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rur_J40Spending_FY2021.csv", row.names = FALSE)
write.csv(FY2022_filt_rural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rur_J40Spending_FY2022.csv", row.names = FALSE)
write.csv(FY2023_filt_rural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rur_J40Spending_FY2023.csv", row.names = FALSE)
write.csv(FY2024_filt_rural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rur_J40Spending_FY2024.csv", row.names = FALSE)

## Summing Rural by County
Rural_FY2021_byCounty <- FY2021_filt_rural
Rural_FY2022_byCounty <- FY2022_filt_rural
Rural_FY2023_byCounty <- FY2023_filt_rural
Rural_FY2024_byCounty <- FY2024_filt_rural



Rural_FY2021_byCounty <- Rural_FY2021_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Rural_FY2022_byCounty <- Rural_FY2022_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Rural_FY2023_byCounty <- Rural_FY2023_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Rural_FY2024_byCounty <- Rural_FY2024_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing Rural County Aggregated Spending to CSV for ArcGIS ##

write.csv(Rural_FY2021_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rural_FY2021_byCounty.csv", row.names = FALSE)
write.csv(Rural_FY2022_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rural_FY2022_byCounty.csv", row.names = FALSE)
write.csv(Rural_FY2023_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rural_FY2023_byCounty.csv", row.names = FALSE)
write.csv(Rural_FY2024_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Rural_FY2024_byCounty.csv", row.names = FALSE)






## Making Non Rural Counties Subset
FY2021_filt_nonrural <- subset(FY2021_filtered, (! FY2021_filtered$State_County %in% RuralCounties)) 
FY2022_filt_nonrural <- subset(FY2022_filtered, (! FY2022_filtered$State_County %in% RuralCounties)) 
FY2023_filt_nonrural <- subset(FY2023_filtered, (! FY2023_filtered$State_County %in% RuralCounties)) 
FY2024_filt_nonrural <- subset(FY2024_filtered, (! FY2024_filtered$State_County %in% RuralCounties)) 

### NonRural Totals by Year
nonruralTot_J40Spending_FY2021 <- sum(FY2021_filt_nonrural$total_obligated_amount)
nonruralTot_J40Spending_FY2022 <- sum(FY2022_filt_nonrural$total_obligated_amount)
nonruralTot_J40Spending_FY2023 <- sum(FY2023_filt_nonrural$total_obligated_amount)
nonruralTot_J40Spending_FY2024 <- sum(FY2024_filt_nonrural$total_obligated_amount)

## Exporting Full NonRural Datasets
write.csv(FY2021_filt_nonrural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/nonrural_J40Spending_FY2021.csv", row.names = FALSE)
write.csv(FY2022_filt_nonrural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/nonrural_J40Spending_FY2022.csv", row.names = FALSE)
write.csv(FY2023_filt_nonrural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/nonrural_J40Spending_FY2023.csv", row.names = FALSE)
write.csv(FY2024_filt_nonrural, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/nonrural_J40Spending_FY2024.csv", row.names = FALSE)

## Summing NonRural by County
NonRural_FY2021_byCounty <- FY2021_filt_nonrural
NonRural_FY2022_byCounty <- FY2022_filt_nonrural
NonRural_FY2023_byCounty <- FY2023_filt_nonrural
NonRural_FY2024_byCounty <- FY2024_filt_nonrural



NonRural_FY2021_byCounty <- NonRural_FY2021_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

NonRural_FY2022_byCounty <- NonRural_FY2022_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

NonRural_FY2023_byCounty <- NonRural_FY2023_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

NonRural_FY2024_byCounty <- NonRural_FY2024_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing Nonrural County Aggregated Spending to CSV for ArcGIS ##

write.csv(NonRural_FY2021_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/NonRural_FY2021_byCounty.csv", row.names = FALSE)
write.csv(NonRural_FY2022_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/NonRural_FY2022_byCounty.csv", row.names = FALSE)
write.csv(NonRural_FY2023_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/NonRural_FY2023_byCounty.csv", row.names = FALSE)
write.csv(NonRural_FY2024_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/NonRural_FY2024_byCounty.csv", row.names = FALSE)





## Making Congressional District Table for Fig 5
FY2021_byCD <- FY2021_filtered
FY2022_byCD <- FY2022_filtered
FY2023_byCD <- FY2023_filtered
FY2024_byCD <- FY2024_filtered



FY2021_byCD <- FY2021_byCD %>%
  group_by(prime_award_transaction_place_of_performance_cd_current) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_byCD <- FY2022_byCD %>%
  group_by(prime_award_transaction_place_of_performance_cd_current) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_byCD <- FY2023_byCD %>%
  group_by(prime_award_transaction_place_of_performance_cd_current) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_byCD <- FY2024_byCD %>%
  group_by(prime_award_transaction_place_of_performance_cd_current) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))


## Writing CD Aggregated Spending to CSV for ArcGIS ##

write.csv(FY2021_byCD, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2021_J40spendingbyCD.csv", row.names = FALSE)
write.csv(FY2022_byCD, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2022_J40spendingbyCD.csv", row.names = FALSE)
write.csv(FY2023_byCD, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2023_J40spendingbyCD.csv", row.names = FALSE)
write.csv(FY2024_byCD, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/FY2024_J40spendingbyCD.csv", row.names = FALSE)



### Distressed Counties Spending ####
## Decided not to use this route, keeping for documentation###
## Prepping EDA Distressed List ###
EDACounties_Dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/EDA_Distressed_forR.csv")
DistressedCounties_Dataset <- subset(EDACounties_Dataset, EDACounties_Dataset$Distressed == "Yes")
DistressedCounties <- DistressedCounties_Dataset$State_County


### Prepping Query for ArcGIS ###
DistressedCounty_String <- paste(DistressedCounties, sep = '', collapse = '","')
DistressedCounty_String

### Filtering FY21-24 for Distressed Counties Only ###
FY2021_filt_distressed <- subset(FY2021_filtered, FY2021_filtered$State_County %in% DistressedCounties) 
FY2022_filt_distressed <- subset(FY2022_filtered, FY2022_filtered$State_County %in% DistressedCounties) 
FY2023_filt_distressed <- subset(FY2023_filtered, FY2023_filtered$State_County %in% DistressedCounties) 
FY2024_filt_distressed <- subset(FY2024_filtered, FY2024_filtered$State_County %in% DistressedCounties) 

## Exporting Full Distressed Datasets
write.csv(FY2021_filt_distressed, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Dist_J40Spending_FY2021.csv", row.names = FALSE)
write.csv(FY2022_filt_distressed, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Dist_J40Spending_FY2022.csv", row.names = FALSE)
write.csv(FY2023_filt_distressed, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Dist_J40Spending_FY2023.csv", row.names = FALSE)
write.csv(FY2024_filt_distressed, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Dist_J40Spending_FY2024.csv", row.names = FALSE)


## Summing Distressed by County
Distressed_FY2021_byCounty <- FY2021_filt_distressed
Distressed_FY2022_byCounty <- FY2022_filt_distressed
Distressed_FY2023_byCounty <- FY2023_filt_distressed
Distressed_FY2024_byCounty <- FY2024_filt_distressed



Distressed_FY2021_byCounty <- Distressed_FY2021_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Distressed_FY2022_byCounty <- Distressed_FY2022_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Distressed_FY2023_byCounty <- Distressed_FY2023_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

Distressed_FY2024_byCounty <- Distressed_FY2024_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

## Writing Distressed County Aggregated Spending to CSV for ArcGIS ##

write.csv(Distressed_FY2021_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Distressed_FY2021_byCounty.csv", row.names = FALSE)
write.csv(Distressed_FY2022_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Distressed_FY2022_byCounty.csv", row.names = FALSE)
write.csv(Distressed_FY2023_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Distressed_FY2023_byCounty.csv", row.names = FALSE)
write.csv(Distressed_FY2024_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/report_csvs/Distressed_FY2024_byCounty.csv", row.names = FALSE)


### Filtering the Combined FYs spending data to only counties with SVI > 0.75 ###
FullSpending_combinedFYs <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/Distressed_Analysis/J40spendingbyCounty_wPerCap_allFYs_Averages_forSVI.csv")
SVI_Distressed <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/Distressed_Analysis/SVI_Distressed_Counties_ONLY.csv")
FullSpending_CombinedFYs_SVI <- merge(FullSpending_combinedFYs, SVI_Distressed, by = "State_County")
write.csv(FullSpending_CombinedFYs_SVI, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/Distressed_Analysis/SVI_Distressed_allFYs_wPerCaps.csv", row.names = FALSE)


### Filtering for DOT CFDAs Only #####
## Method 1 doesn't match Sophie's numbers###
#FY2021_DOT <- subset(FY2021_filtered, FY2021_filtered$cfda_number >= 20 & FY2021_filtered$cfda_number < 21 ) 
#sum(FY2021_DOT$total_obligated_amount)

## Method 2 does match Sophie's numbers!
FY2021_DOT <- subset(FY2021_filtered, FY2021_filtered$awarding_agency_name == 'Department of Transportation' ) 
FY2022_DOT <- subset(FY2022_filtered, FY2022_filtered$awarding_agency_name == 'Department of Transportation' ) 
FY2023_DOT <- subset(FY2023_filtered, FY2023_filtered$awarding_agency_name == 'Department of Transportation' ) 
FY2024_DOT <- subset(FY2024_filtered, FY2024_filtered$awarding_agency_name == 'Department of Transportation' ) 

sum(FY2021_DOT$total_obligated_amount)
## Aggregating DOT by county ###

FY2021_DOT$State_County <- paste(FY2021_DOT$primary_place_of_performance_state_name, FY2021_DOT$primary_place_of_performance_county_name, sep= "-")
FY2022_DOT$State_County <- paste(FY2022_DOT$primary_place_of_performance_state_name, FY2022_DOT$primary_place_of_performance_county_name, sep= "-")
FY2023_DOT$State_County <- paste(FY2023_DOT$primary_place_of_performance_state_name, FY2023_DOT$primary_place_of_performance_county_name, sep= "-")
FY2024_DOT$State_County <- paste(FY2024_DOT$primary_place_of_performance_state_name, FY2024_DOT$primary_place_of_performance_county_name, sep= "-")


## Aggregating all federal spending by county ##

FY2021_DOT_byCounty <- FY2021_DOT
FY2022_DOT_byCounty <- FY2022_DOT
FY2023_DOT_byCounty <- FY2023_DOT
FY2024_DOT_byCounty <- FY2024_DOT



FY2021_DOT_byCounty <- FY2021_DOT_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2022_DOT_byCounty <- FY2022_DOT_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2023_DOT_byCounty <- FY2023_DOT_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

FY2024_DOT_byCounty <- FY2024_DOT_byCounty %>%
  group_by(State_County) %>%
  summarize(total_J40_fed_spending = sum(total_obligated_amount))

write.csv(FY2021_DOT_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2021_DOT_byCounty_v2.csv", row.names = FALSE)
write.csv(FY2022_DOT_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2022_DOT_byCounty_v2.csv", row.names = FALSE)
write.csv(FY2023_DOT_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2023_DOT_byCounty_v2.csv", row.names = FALSE)
write.csv(FY2024_DOT_byCounty, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2024_DOT_byCounty_v2.csv", row.names = FALSE)

## DOT Spending in Rural Counties ###
Rural_Dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Rural_Counties_List.csv")
RuralCounties <- Rural_Dataset$State_County
FY2021_DOT_Rural <- subset(FY2021_DOT, FY2021_DOT$State_County %in% RuralCounties) 
FY2022_DOT_Rural <- subset(FY2022_DOT, FY2022_DOT$State_County %in% RuralCounties) 
FY2023_DOT_Rural <- subset(FY2023_DOT, FY2023_DOT$State_County %in% RuralCounties) 
FY2024_DOT_Rural <- subset(FY2024_DOT, FY2024_DOT$State_County %in% RuralCounties) 

### Rural Totals by Year
RurTot_DOT_FY2021 <- sum(FY2021_DOT_Rural$total_obligated_amount)
RurTot_DOT_FY2022 <- sum(FY2022_DOT_Rural$total_obligated_amount)
RurTot_DOT_FY2023 <- sum(FY2023_DOT_Rural$total_obligated_amount)
RurTot_DOT_FY2024 <- sum(FY2024_DOT_Rural$total_obligated_amount)


## Tribal DOT Spending ######
## Making a list of the business types from Sophie ###
Tribal_BusinessTypes_dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Tribal_Business_Types.csv")
Tribal_BusinessTypes <- Tribal_BusinessTypes_dataset$Business_Type
Tribal_BusinessTypes


## Subsetting 
FY2021_DOT_Trib <- subset(FY2021_DOT, FY2021_DOT$business_types_description %in% Tribal_BusinessTypes)
FY2022_DOT_Trib <- subset(FY2022_DOT, FY2022_DOT$business_types_description %in% Tribal_BusinessTypes)
FY2023_DOT_Trib <- subset(FY2023_DOT, FY2023_DOT$business_types_description %in% Tribal_BusinessTypes)
FY2024_DOT_Trib <- subset(FY2024_DOT, FY2024_DOT$business_types_description %in% Tribal_BusinessTypes)

# Totals ###
TribTot_DOT_FY2021 <- sum(FY2021_DOT_Trib$total_obligated_amount)
TribTot_DOT_FY2022 <- sum(FY2022_DOT_Trib$total_obligated_amount)
TribTot_DOT_FY2023 <- sum(FY2023_DOT_Trib$total_obligated_amount)
TribTot_DOT_FY2024 <- sum(FY2024_DOT_Trib$total_obligated_amount)

## Exporting Raw DOT Files ###
write.csv(FY2021_DOT, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2021_DOT.csv", row.names = FALSE)
write.csv(FY2022_DOT, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2022_DOT.csv", row.names = FALSE)
write.csv(FY2023_DOT, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2023_DOT.csv", row.names = FALSE)
write.csv(FY2024_DOT, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/DOT_Only/FY2024_DOT.csv", row.names = FALSE)


### Filtering for EPA,totals match Sophie's  ###
FY2021_EPA <- subset(FY2021_filtered, FY2021_filtered$awarding_agency_name == 'Environmental Protection Agency' ) 
FY2022_EPA <- subset(FY2022_filtered, FY2022_filtered$awarding_agency_name == 'Environmental Protection Agency' ) 
FY2023_EPA <- subset(FY2023_filtered, FY2023_filtered$awarding_agency_name == 'Environmental Protection Agency' ) 
FY2024_EPA <- subset(FY2024_filtered, FY2024_filtered$awarding_agency_name == 'Environmental Protection Agency' ) 


## Adding State County Field
FY2021_EPA$State_County <- paste(FY2021_EPA$primary_place_of_performance_state_name, FY2021_EPA$primary_place_of_performance_county_name, sep= "-")
FY2022_EPA$State_County <- paste(FY2022_EPA$primary_place_of_performance_state_name, FY2022_EPA$primary_place_of_performance_county_name, sep= "-")
FY2023_EPA$State_County <- paste(FY2023_EPA$primary_place_of_performance_state_name, FY2023_EPA$primary_place_of_performance_county_name, sep= "-")
FY2024_EPA$State_County <- paste(FY2024_EPA$primary_place_of_performance_state_name, FY2024_EPA$primary_place_of_performance_county_name, sep= "-")



## Exporting Raw EPA Files ####
write.csv(FY2021_EPA, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/EPA_Only/FY2021_EPA.csv", row.names = FALSE)
write.csv(FY2022_EPA, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/EPA_Only/FY2022_EPA.csv", row.names = FALSE)
write.csv(FY2023_EPA, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/EPA_Only/FY2023_EPA.csv", row.names = FALSE)
write.csv(FY2024_EPA, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/EPA_Only/FY2024_EPA.csv", row.names = FALSE)

### Filtering for Tribal Spending in EPA ####
## Making a list of the business types from Sophie ###
Tribal_BusinessTypes_dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Tribal_Business_Types.csv")
Tribal_BusinessTypes <- Tribal_BusinessTypes_dataset$Business_Type
Tribal_BusinessTypes


## Subsetting 
FY2021_EPA_Trib <- subset(FY2021_EPA, FY2021_EPA$business_types_description %in% Tribal_BusinessTypes)
FY2022_EPA_Trib <- subset(FY2022_EPA, FY2022_EPA$business_types_description %in% Tribal_BusinessTypes)
FY2023_EPA_Trib <- subset(FY2023_EPA, FY2023_EPA$business_types_description %in% Tribal_BusinessTypes)
FY2024_EPA_Trib <- subset(FY2024_EPA, FY2024_EPA$business_types_description %in% Tribal_BusinessTypes)

# Totals ###
TribTot_EPA_FY2021 <- sum(FY2021_EPA_Trib$total_obligated_amount)
TribTot_EPA_FY2022 <- sum(FY2022_EPA_Trib$total_obligated_amount)
TribTot_EPA_FY2023 <- sum(FY2023_EPA_Trib$total_obligated_amount)
TribTot_EPA_FY2024 <- sum(FY2024_EPA_Trib$total_obligated_amount)
Trib_EPA_allFYs <- sum(TribTot_EPA_FY2021,TribTot_EPA_FY2022,TribTot_EPA_FY2023,TribTot_EPA_FY2024 )

## EPA Spending in Rural Counties, will repeat in pivot tables ###
Rural_Dataset <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/gis_maps_files/csvs_20241203/Rural_Counties_List.csv")
RuralCounties <- Rural_Dataset$State_County
FY2021_EPA_Rural <- subset(FY2021_EPA, FY2021_EPA$State_County %in% RuralCounties) 
FY2022_EPA_Rural <- subset(FY2022_EPA, FY2022_EPA$State_County %in% RuralCounties) 
FY2023_EPA_Rural <- subset(FY2023_EPA, FY2023_EPA$State_County %in% RuralCounties) 
FY2024_EPA_Rural <- subset(FY2024_EPA, FY2024_EPA$State_County %in% RuralCounties) 

### Rural Totals by Year, will repeat in pivot tables ###
RurTot_EPA_FY2021 <- sum(FY2021_EPA_Rural$total_obligated_amount)
RurTot_EPA_FY2022 <- sum(FY2022_EPA_Rural$total_obligated_amount)
RurTot_EPA_FY2023 <- sum(FY2023_EPA_Rural$total_obligated_amount)
RurTot_EPA_FY2024 <- sum(FY2024_EPA_Rural$total_obligated_amount)
sum(RurTot_EPA_FY2021, RurTot_EPA_FY2022, RurTot_EPA_FY2023, RurTot_EPA_FY2024)
