##==============================================================================
## Project: CO2 isotopes
## Script to convert data from the Picarro
## Code authors: J.R. Blaszczak, R.O. Hall, Jr.
##==============================================================================

# Picarro data handling attempt. Based on: https://github.com/bpbond/R-data-picarro/blob/master/scripts/picarro.R

## Import packages
lapply(c("plyr","dplyr","readr","ggplot2","cowplot","reshape2",
         "xts","dygraphs","lubridate","tidyverse"), require, character.only=T)

## Prior to running the rest of the code
## 1) Set the wd to folder location of the raw Picarro dat files and meta data
## 2) Specify meta file name
meta_file_name <- "2018_09_28_OakCreek.txt"
## 3) Specify start time of Picarro run (using Picarro time)
picarro_start_time <- "2018-09-28 19:30:00"
## 4) Specify notes file name
notes_file_name <- "2018_09_28_OakCreek_notes.txt"

###########
## IMPORT
###########

## Import meta data for each day:  In this case metadata consists of a sample identifier, and time stamp, where time stamp is when the sample syring hits about 15mL and it 
## looks like the trace is leveling off for that sample.  This metadata can and should have other stuff in it, such as sample location, collection time,
## pH, volume of sample, volume of equlibration gas (typically 70mL), temp of equilibration
meta <- read.table(meta_file_name, sep=",",header=T)
meta$DATETIME <- as.POSIXct(as.character(meta$ResetTime), format="%Y-%m-%d %H:%M:%S")
meta$samplenum<-seq(1:length(meta$ResetTime))

## Import the raw data
filelist <-list.files( pattern="dat", full.names = TRUE)  ##makes a vector of that day's files
rawdata <- list()  #empty list
#below loops over all the files making a list containg a tibble for each file and stuffing it into a list
for(f in filelist) {
  cat("Reading", f, "\n") #no idea why we need this
  read_table(f) %>%
    select(DATE, TIME, ALARM_STATUS, MPVPosition, `12CO2_dry`, Delta_Raw_iCO2, HP_12CH4, `HP_Delta_iCH4_Raw`) -> rawdata[[f]] #add whichever things we want here
}
rawdata <- bind_rows(rawdata)

## Fix datetime
rawdata$DATETIME <- lubridate::ymd_hms(paste(rawdata$DATE, rawdata$TIME))
rawdata$DATETIME <- as.POSIXct(as.character(rawdata$DATETIME), format="%Y-%m-%d %H:%M:%S")

# Get rid of unneeded fields
rawdata$DATE <- rawdata$TIME <- NULL

##average data for each second where we have a measure.  Not optional
rawdata<- rawdata %>% group_by(as.numeric(rawdata$DATETIME)) %>% summarise_all(funs(mean))

##get rid of all the early in the day data to make a smaller file.  This step is optional
rawdata <- subset(rawdata, rawdata$DATETIME > picarro_start_time)

##if ok, then name the rawdat file for that day here
dat<-rawdata
colnames(dat)[4:7] <- c("CO2","Delta_iCO2","CH4","Delta_iCH4")

#Plot it. Look ok?
plot(rawdata$DATETIME,rawdata$`12CO2_dry`)
plot(rawdata$DATETIME,rawdata$Delta_Raw_iCO2)
plot(rawdata$DATETIME,rawdata$HP_12CH4)
plot(rawdata$DATETIME,rawdata$`HP_Delta_iCH4_Raw`)


#############
## SUBSET
#############
## Subset the main datafile based on sample time + some amount of time, 
## 40 seconds appears correct, but always check this time.  
## To short and we lost data.  Too long and we average off the plateau
meta$ENDTIME<-meta$DATETIME+30

subdat<-list()
for(i in 1:length(meta$samplenum)){
  
  subdat[[i]]<-subset(dat, DATETIME>meta$DATETIME[i]  & DATETIME< meta$ENDTIME[i])
  subdat[[i]]$samplenum <-i
}
subdat <- bind_rows(subdat)

## Plot All Together
subdat_melt <- melt(subdat[,4:9], id.vars=c("DATETIME","samplenum"))
ggplot(subdat_melt, aes(DATETIME, value, color=samplenum))+
  geom_point()+
  facet_wrap(~variable, ncol=2, scales = "free")

######################
## QA/QC every sample
######################
## Look at time series traces plotted over original data
## and decide on new Reset time if necessary

vis <- function(d, subd, v){
  
  d <- d[,c("DATETIME",v)]
  subd <- subd[,c("DATETIME",v,"samplenum")]
  comb <- merge(d, subd, by="DATETIME", all.x=T)
  colnames(comb) <- c("DateTime","All","Sub","samplenum")
  comb.xts <- as.xts(comb, order.by=comb$DateTime)
  
  dygraph(comb.xts[,c("All","Sub","samplenum")]) %>%
    dyAxis("y2", label = "samplenum", independentTicks = TRUE)%>%
    dySeries("Sub", fillGraph = T)%>%
    dySeries("samplenum", axis=('y2'))%>%
    dyRangeSelector()
}

## 13C-CO2
vis(dat, subdat, "Delta_iCO2")

## CO2
vis(dat, subdat, "CO2")

## CH4
vis(dat, subdat, "CH4")

## 13C-CH4
vis(dat, subdat, "Delta_iCH4")


#####################################################
## Adjust overall start time and individual sites
####################################################

## This section is sample run specific
## Save this section of code in text file for future reference

## Oak 2018-09-28

## Samples that need later start times
meta[which(meta$samplenum %in% c(4,6,9,11,14,16,70)),]$DATETIME <- meta[which(meta$samplenum %in% c(4,6,9,11,14,16,70)),]$DATETIME + 5

## Samples that need earlier start times
meta[which(meta$samplenum %in% c(49,83)),]$DATETIME <- meta[which(meta$samplenum %in% c(49,83)),]$DATETIME - 5

## Averaging could last 5 seconds longer
meta$ENDTIME<-meta$DATETIME+35

subdat_rev<-list()
for(i in 1:length(meta$samplenum)){
  
  subdat_rev[[i]]<-subset(dat, DATETIME>meta$DATETIME[i]  & DATETIME< meta$ENDTIME[i])
  subdat_rev[[i]]$samplenum <-i
}
subdat_rev <- bind_rows(subdat_rev)


## 13C-CO2
vis(dat, subdat_rev, "Delta_iCO2")
## CO2
vis(dat, subdat_rev, "CO2")
## CH4
vis(dat, subdat_rev, "CH4")
## 13C-CH4
vis(dat, subdat, "Delta_iCH4")

## Get rid of samples #10, 50, 51
subdat_rev <- subdat_rev[-which(subdat_rev$samplenum %in% c(10,50,51)),]


## After QA/QC Summarize per sample
subdatsum<- subdat_rev %>% group_by(samplenum) %>% summarise(CO2=mean(CO2), delCO2=mean(Delta_iCO2), 
                                                         CH4=mean(CH4), delCH4=mean(Delta_iCH4) )


###########################################
##merge with meta, and give it a new name
#############################################
#Thinking about the metafile, here is what should also go in
#water_temp, equil_temp, pH, ANC, air_vol (it won't always be 70), baro_press (absolute, mm Hg)
#not the  kH_CO2 because that is a derived value that we will calc each time

sum_file<- full_join(meta,subdatsum)  #there you go, joining by samplenum

## Remove standards
sum_file <- na.omit(sum_file)

## Remove any problematic syringe lines based on notes that got through QA/QC
View(sum_file)
#sum_file <- sum_file[-which(sum_file$Syringe %in% c(28,95)),]


## Plot
sum_file_melt <- melt(sum_file[,c("CO2","delCO2","CH4","delCH4","DATETIME","samplenum")], id.vars=c("DATETIME","samplenum"))
ggplot(sum_file_melt, aes(DATETIME, value, color=samplenum))+
  geom_point()+
  facet_wrap(~variable, ncol=2, scales = "free")


## merge again with notes file to get syringe specific information
notes <- read.table(notes_file_name, sep=",",header=T)
notes <- notes %>% 
  tidyr::separate(Syringes, c("SyringeA","SyringeB","SyringeC"))


sumA <- merge(sum_file, notes, by.x="Syringe", by.y = "SyringeA")
sumB <- merge(sum_file, notes, by.x="Syringe", by.y = "SyringeB")
sumC <- merge(sum_file, notes, by.x="Syringe", by.y = "SyringeC")
sum_file <- rbind(sumA[,1:17], sumB[,1:17], sumC[,1:17])
## Fix names
names(sum_file)
sum_file <- sum_file[,c(1:4,9:17)]
colnames(sum_file)[9] <- "SampleID"










############
## Export
###########
write.csv(sum_file, "2018_09_28_OakDiel_QAQC.csv")

