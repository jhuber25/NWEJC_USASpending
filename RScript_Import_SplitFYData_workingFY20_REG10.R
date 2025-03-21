library(readr)
setwd("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2021")
FY2021 <- do.call(rbind,
          lapply(list.files(path = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2021"), read_csv))
head(FY2021)

## Adding Filter Step, try 1
typeof(FY2021$cfda_number)  ## stored in CSVs as double, need to align with Sophie's excel sheet

Justice40_ProgramList <- read_csv("C:/Users/justi/Documents/NWEJC/Justice40_Tracking/Justice40_Combined_ConfirmedUnconfirmedCFDAs.csv")
typeof(Justice40_ProgramList$CFDA) # CSV read in as double also, so no need for conversion
Justice40CFDAs <- Justice40_ProgramList$CFDA # list of CFDAs, use for the subsetting statement
## statement <- FY2021$primary_place_of_performance_state_name %in% c("OREGON", "IDAHO", "WASHINGTON", "ALASKA") & FY2021$cfda_number %in% Justice40CFDAs ## still haven't made this work, no need to run this line
FY2021_filt_RegionCFDA <- subset(FY2021, FY2021$primary_place_of_performance_state_name %in% c("OREGON", "IDAHO", "WASHINGTON", "ALASKA") & FY2021$cfda_number %in% Justice40CFDAs) 
write.csv(FY2021_filt_RegionCFDA, file = "C:/Users/justi/Documents/NWEJC/Justice40_Tracking/FY2021_combined_filtered/FY2021_combined_filtered.csv", row.names = FALSE) ## this is to export the combined_filtered



###### Making list of agencies to loop through

agencies <- unique(FY2021_filt_RegionCFDA$awarding_agency_name)
agencies_ordered <- sort(agencies)
agencies_ordered




##### Making subsets with list of subsets, export with for loop #####

FY2021_filtered_list <- list()
for (agency in agencies) {
  new_name <- paste("fy2021_filtered", agency, sep="_")
  df <- subset(FY2021_filt_RegionCFDA, FY2021_filt_RegionCFDA$awarding_agency_name == agency)
  FY2021_filtered_list[[agency]] <- df
  
  
  
}




##### For Loop that exports all to separate CSVs this one works! Agency names are included
for (i in seq_along(FY2021_filtered_list)) {
  filename <- paste0("fy2021_filtered_", names(FY2021_filtered_list[i]), ".csv")
  write.csv(FY2021_filtered_list[[i]], file = filename, row.names = FALSE)
  
  
}


