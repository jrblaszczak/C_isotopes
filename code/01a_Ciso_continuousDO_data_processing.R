##==============================================================================
## Project: CO2 isotopes
## Script to process continuous sensor data from diel sampling
## Code author: J.R. Blaszczak
##==============================================================================

## Load packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "readxl"), require, character.only=T)

setwd("../data/continuous_data_raw")

## Import DO data
DOdat <- ldply(list.files(pattern = "*_DOcompiled.csv"), function(filename) {
  d <- read.csv(filename)
  d$file <- filename
  return(d)
})

names(DOdat)

DOdat <- DOdat[,c("Time","Temp","DO","file")]
colnames(DOdat) <- c("DateTime","temp","oxy","file")

sDO <- split(DOdat, DOdat$file)
names(sDO) <- c("Blaine","Beaver","OakLolomai","OakWillow")

sDO <- lapply(sDO, function(x) return(x[,c("DateTime","temp","oxy")]))
dDO <- ldply(sDO, data.frame)
dDO$DateTime <- as.POSIXct(as.character(dDO$DateTime), format = "%Y-%m-%d %H:%M:%S")

## Round DateTime to the nearest 5 minute interval to help with integration
dDO$DateTime <- floor_date(dDO$DateTime, "5 minutes")

## Visualize
ggplot(dDO, aes(DateTime, oxy))+geom_line()+
  facet_wrap(~.id)

## Save compiled file
write.csv(dDO, "../DO_continuous_data.csv")



