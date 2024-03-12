## MIMS Diel Post-processing

library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

#############
## IMPORT
############
#MIMS <- read.csv("2018_09_03_BeaverDiel_MIMSprocessed.csv", header=T)
MIMS <- read.csv("2018_10_02_OakDiel_MIMSprocessed.csv", header=T)

## Convert Sampletime to POSIXct
head(MIMS$Sampletime)
#MIMS$Sampletime <- as.POSIXct(as.character(MIMS$Sampletime), format="%m/%d/%y %H:%M")
MIMS$Sampletime <- as.POSIXct(as.character(MIMS$Sampletime), format="%Y-%m-%d %H:%M:%S", tz="US/Arizona")

## Visualize
ggplot(MIMS, aes(Sampletime, nconc, color=Location))+geom_point()
ggplot(MIMS, aes(Sampletime, O2conc, color=Location))+geom_point()
ggplot(MIMS, aes(Sampletime, O2arcalc, color=Location))+geom_point()

## Take the mean of all measurements at the same time point
MIMS %>%
  group_by(Location,Sampletime) %>%
  summarise(O2Ar = mean(O2arcalc), Nconc = mean(nconc), O2conc = mean(O2conc))
