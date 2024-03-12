## Processing Beaver Creek Partial Diel

library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(reshape2)

##########
## Import
##########
## Set right wd
df <- read.csv("2018_08_10_BeaverPartialDiel.csv", header=T)
names(df)
df$DATETIME <- as.POSIXct(as.character(df$DATETIME), format="%Y-%m-%d %H:%M:%S")

## Visualize
df_melt <- melt(df[,c("CO2","delCO2","CH4","delCH4","DATETIME","samplenum")], id.vars=c("DATETIME","samplenum"))
ggplot(df_melt, aes(DATETIME, value, color=samplenum))+
  geom_point()+
  facet_wrap(~variable, ncol=2, scales = "free")

## Separate SampleID
sapply(df, class)
df$SampleID <- as.character(df$SampleID)

df <- df %>% 
  separate(SampleID, c("Site","Date","Time"))

df$Date <- revalue(df$Date, c("810" = "2018-08-10","89"="2018-08-09"))
df$Time <- as.numeric(df$Time)
df$Time <- ifelse(df$Time < 1000, yes= paste(substring(df$Time, 1, 1), substring(df$Time, 2, 3), sep = ":"),
                  no = ifelse(df$Time >= 1000, yes=paste(substring(df$Time, 1, 2), substring(df$Time, 3, 4), sep = ":"), no=NA))
df$Time <- revalue(df$Time, c("0:"="0:00"))

df$SampleDateTime <- lubridate::ymd_hm(paste(df$Date,df$Time))
df$SampleDateTime <- as.POSIXct(as.character(df$SampleDateTime), format="%Y-%m-%d %H:%M:%S",tz = "UTC")

## Write csv for future work
names(df)
df_export <- df[,c(2,12:31)]
#write.csv(df_export, "2018_08_10_BeaverPartialDiel_Processed.csv")


#########################
## Summarize and Merge
#######################
df_melt2 <- melt(df[,c("CO2","delCO2","CH4","delCH4","SampleDateTime","Site")], id.vars=c("SampleDateTime","Site"))
ggplot(df_melt2, aes(SampleDateTime, value, color=Site))+
  geom_point()+
  facet_wrap(~variable, ncol=2, scales = "free")

df_mean <- dcast(df_melt2, SampleDateTime + Site ~ variable, fun.aggregate = mean)
df_sd <- dcast(df_melt2, SampleDateTime + Site ~ variable, fun.aggregate = sd)

df_mean_melt <- melt(df_mean[,c("CO2","delCO2","CH4","delCH4","SampleDateTime","Site")], id.vars=c("SampleDateTime","Site"))
df_sd_melt <- melt(df_sd[,c("CO2","delCO2","CH4","delCH4","SampleDateTime","Site")], id.vars=c("SampleDateTime","Site"))

df_mean_sd <- merge(df_mean_melt, df_sd_melt, by=c("SampleDateTime","Site","variable"))

colnames(df_mean_sd)[4:5] <- c("Mean","SD")

#################
## Visualize
################

ggplot(df_mean_sd, aes(SampleDateTime, Mean, color=Site))+
  geom_point(size=3)+
  geom_errorbar(aes(ymax=Mean-SD, ymin=Mean+SD), width=.2, position=position_dodge(.9))+
  facet_wrap(~variable, ncol=2, scales = "free")+
  theme(axis.title = element_blank(),
        strip.background = element_rect(fill = "white", color="black",linetype = "solid"),
        panel.border = element_rect(color = "black", linetype = "solid"),
        legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values = c("BeaverDwn" = "red3"))+
  scale_x_datetime(breaks = df_mean_sd$SampleDateTime)



#############################
## Visualize Relationships
#############################
names(df)

df_melt3 <- melt(df[,c(12:18,30,31)], id.vars=c("SampleDateTime"))
ggplot(df_melt3, aes(SampleDateTime, value))+
  geom_point()+
  facet_wrap(~variable, ncol=2, scales = "free")



ggplot(df, aes(SampleDateTime, WaterTemp_C))+
  geom_point(color="red2", size=3)

ggplot(df, aes(SampleDateTime, pH))+
  geom_point(color="blue", size=3)

ggplot(df, aes(SampleDateTime, Baro_inHg))+
  geom_point(color="blue", size=3)









