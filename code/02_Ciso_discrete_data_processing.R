##==============================================================================
## Project: CO2 isotopes
## Script to process discrete sample data from diel sampling
## Code author: J.R. Blaszczak
##==============================================================================

## Load packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "readxl"), require, character.only=T)

##############
## IMPORT
##############
setwd("../data/discrete_data_raw")

## Import QAQC'd Picarro data
dat <- ldply(list.files(pattern = "*_QAQC.csv"), function(filename) {
  d <- read.csv(filename)
  d$file <- filename
  return(d)
})
names(dat)
## subset to relevant columns
iso <- dat[,c("SampleID","CO2","delCO2","CH4","delCH4","WaterTemp_C","Baro_inHg")]

## Import alkalinity and pH data
A_pH <- read.csv("Diel_pH_Alk_mean_reps.csv", header=T) ## mean of triplicate values for Oak
## convert from mg L-1 CaCO3 to mol m-3 CaCO3
A_pH$Alk_mol_m3 <- A_pH$Alk_mgLCaCO3/(60.008*1000)

## Merge with iso data
iso <- left_join(iso, A_pH, by="SampleID")


###########################################
## Separate SampleID to create DateTime
##########################################
sapply(iso, class)
iso$SampleID <- as.character(iso$SampleID)

iso <- iso %>% 
  separate(SampleID, c("Site","Date","Time"))

iso$Date <- revalue(iso$Date, c("822" = "2018-08-22","823"="2018-08-23","830" = "2018-08-30","831"="2018-08-31",
                                "926" = "2018-09-26","927"="2018-09-27","928"="2018-09-28"))
iso$Time <- as.numeric(iso$Time)
iso$Time <- ifelse(iso$Time < 1000, yes= paste(substring(iso$Time, 1, 1), substring(iso$Time, 2, 3), sep = ":"),
                   no = ifelse(iso$Time >= 1000, yes=paste(substring(iso$Time, 1, 2), substring(iso$Time, 3, 4), sep = ":"), no=NA))
iso$Time <- revalue(iso$Time, c("0:"="0:00"))

iso$SampleDateTime <- lubridate::ymd_hm(paste(iso$Date,iso$Time))
iso$SampleDateTime <- as.POSIXct(as.character(iso$SampleDateTime), format="%Y-%m-%d %H:%M:%S",tz = "UTC")

iso <- iso[order(iso$Site, iso$Date, iso$Time),]


#################
## Export
#################
write.csv(iso, "../Discrete_data_allsites.csv")

















