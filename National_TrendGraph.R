############ Libraries #####################################
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

### Pick up here in main J40 script, attempting to consolidate ###

### Read in from CSV to save time rather than recreating entire dataset #######
NationalTotals_J40 <- read_csv("D:/R_Analysis_IntermediateFiles/National/nationalj40_total.csv")

## Line plot of total spending federal action vs total obligated ##
metric_comp_plot <- 
  ggplot(NationalTotals_J40, aes(x = Year)) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=Total_Obligated, color="Total Obligated"), size=2) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=Total_FedSpend, color="Federal Action Obligated"), size=2)+
  scale_y_continuous(labels = unit_format(unit = " Trillion", prefix = "$", sep="", scale = 1e-12)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending by Field Used") + 
  scale_color_manual(name="", values=c("Total Obligated"="green", "Federal Action Obligated"="red")) + 
  scale_x_continuous("Year", labels = as.character(NationalTotals_J40$Year), breaks = NationalTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
metric_comp_plot

## Just Federal Action Obligation ###
FedSpend_plot <- 
  ggplot(NationalTotals_J40, aes(x = Year)) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=Total_FedSpend, color="Federal Obligation"), size=2)+
  scale_y_continuous(limits = c(0,5000000000000),labels = unit_format(unit = " Trillion", prefix = "$", sep="", scale = 1e-12)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending by FY") + 
  scale_color_manual(name="", values=c("Federal Obligation"="red")) + theme(plot.title = element_text(hjust = 0.5))+ 
  scale_x_continuous("Year", labels = as.character(NationalTotals_J40$Year), breaks = NationalTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
FedSpend_plot

## J40 and total Federal Action Obligation #########
j40_tot_fedaction_plot <- 
  ggplot(NationalTotals_J40, aes(x = Year)) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=FedSpend_J40, color="Energy & Environment"), size=2) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=Total_FedSpend, color="Total"), size=2)+
  scale_y_continuous(labels = unit_format(unit = " Trillion", prefix = "$", sep="", scale = 1e-12)) + 
  ylab("Federal Spending") + 
  ggtitle("Federal Spending (Energy & Environment and Total)") + 
  scale_color_manual(name="", values=c("Energy & Environment"="blue", "Total"="red")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(NationalTotals_J40$Year), breaks = NationalTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
j40_tot_fedaction_plot


## Just Federal Action Obligation ###
J40_FedSpend_plot <- 
  ggplot(NationalTotals_J40, aes(x = Year)) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=FedSpend_J40, color="E&E Federal Obligation"), size=2)+
  scale_y_continuous(limits = c(0,300000000000),labels = unit_format(unit = " Billion", prefix = "$", sep="", scale = 1e-9)) + 
  ylab("Energy & Environment Federal Spending") + 
  ggtitle("Energy & Environment Federal Spending by FY") + 
  scale_color_manual(name="", values=c("E&E Federal Obligation"="blue")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(NationalTotals_J40$Year), breaks = NationalTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
J40_FedSpend_plot

## Percent of Federal Action Obligation Spent on J40 ##
J40_FedSpend_Perc_plot <- 
  ggplot(NationalTotals_J40, aes(x = Year)) + 
  geom_line(data=NationalTotals_J40, aes(x=Year, y=J40_Perc_FedSpend, color="E&E Percentage"), size=2)+
  scale_y_continuous(limits = c(0,0.1),labels = scales::percent) + 
  ylab("Energy & Environment Federal Spending Percentage") + 
  ggtitle("Energy & Environment Federal Spending by FY") + 
  scale_color_manual(name="", values=c("E&E Percentage"="turquoise")) + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = as.character(NationalTotals_J40$Year), breaks = NationalTotals_J40$Year) + theme(axis.text.x=element_text(angle = 45, hjust = 1))
J40_FedSpend_Perc_plot


