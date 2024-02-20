##==============================================================================
## Project: CO2 isotopes
## Script to estimate discharge using dilution gauging
## Code author: J.R. Blaszczak
##==============================================================================

## Explanation based off: Moore, R.D. 2005. Salt Injection Using Salt in Solution.
## "Streamline Watershed Management Bulletin". Volume 8 (2)

## Two assumptions:
## (1) All the injected mass is recovered downstream
## (2) Tracer is completely mixed in the channel

## The time required for the peak of the wave to move past an observation point 
## depends inversely on the mean velocity of streamflow
## The duration of the salt wave depends on the amount of longitudinal dispersion,
## which depends on how variable velocities are across the stream

## At any time (t) while tracer is passing in the salt wave,
## the discharge of the tracer solution is: q(t) = Q*RC(t)
## Where Q is stream discharge (L/s), and RC(t) is the relative
## concentration of the tracer solution (L/L) at t

## Integrate over the salt wave to get discharge (Q):
## Q = V/integral(RC(t))dt

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#############################
## Import & Visualize Data ##
#############################

# Iteratively import
dat <- read.csv("../data/discharge_data_raw/2018_09_28_Cond_Pink_LolomaiOakCreek.csv", skip=1, header=T)
#dat <- read.csv("2018_09_28_Cond_Yellow_WillowPtOakCreek.csv", skip=1, header=T)
#dat <- read.csv("2018_08_28_Cond_Yellow_Blaine.csv", skip=1, header=T)
#dat <- read.csv("2018_09_25_Cond_Pink_Beaver.csv", skip=1, header=T)

names(dat)

# Subset
dat <- dat[,c(2:4)]
colnames(dat) <- c("DateTime","Cond","TempC")

# Convert DateTime
dat$DateTime <- as.POSIXct(as.character(dat$DateTime), format="%m/%d/%y %H:%M:%S")
head(dat)
sapply(dat, class)

# Visualize
ggplot(dat, aes(DateTime, Cond))+geom_point()

## Subset based on plot
dat <- subset(dat, DateTime >= as.POSIXct("2018-09-28 07:15:00") & DateTime <= as.POSIXct("2018-09-28 09:00:00")) # Lolomai
#dat <- subset(dat, DateTime >= as.POSIXct("2018-09-28 09:00:00") & DateTime <= as.POSIXct("2018-09-28 10:14:00")) # Willow
#dat <- subset(dat, DateTime >= as.POSIXct("2018-08-23 07:00:00") & DateTime <= as.POSIXct("2018-08-23 9:00:00")) # Blaine
#dat <- subset(dat, DateTime >= as.POSIXct("2018-08-31 07:00:00") & DateTime <= as.POSIXct("2018-08-31 11:00:00")) # Beaver

# for Blaine & Beaver
#dat <- dat[1:100,] #Blaine
#dat <- dat[1:200,] #Beaver


################
## Estimate Q ##
################
## Equation
Qint<-function(time,cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

## (1) Determine the background conductivity
## Select area before or after the salt wave which is constant
## for at least 30 minutes and take the average

sub_bg <- subset(dat, DateTime >= as.POSIXct("2018-09-28 08:30:00") & DateTime <= as.POSIXct("2018-09-28 09:00:00")) #Lolomai
#sub_bg <- subset(dat, DateTime >= as.POSIXct("2018-09-28 09:00:00") & DateTime <= as.POSIXct("2018-09-28 09:30:00")) #Willow
#sub_bg <- subset(dat, DateTime >= as.POSIXct("2018-08-23 07:00:00") & DateTime <= as.POSIXct("2018-08-23 07:30:00")) #Blaine
#sub_bg <- subset(dat, DateTime >= as.POSIXct("2018-08-31 07:30:00") & DateTime <= as.POSIXct("2018-08-31 08:00:00")) #Beaver

bg_cond <- mean(sub_bg$Cond)

## (2) Estimate conductivity slug based on mass of Cl added
## 1 g salt in 1 L of water gives cond=2100 uS / cm
Oak_Lolomai_Clmass <- 3000 # Oak @ Lolomai: 3.00 kg
Oak_Willow_Clmass <- 7210 # Oak @ Willow: 7.21 kg
Blaine_Clmass <- 1209 # Blaine: 1209 g
Beaver_Clmass <- 2084 # Beaver: 2084 g

Cond_mass <- 2100*Oak_Lolomai_Clmass ## *replace depending on calculation

## Calculate Q!
## Units = L/sec
Q <- Qint(as.numeric(dat$DateTime), dat$Cond, bg_cond, Cond_mass)
Q

## Summary
## Oak @ Lolomai: Q = 491.2901 L/sec (0.49 cms)
## Oak @ Willow: Q = 724.7055 L/sec (0.72 cms)
## Blaine: Q = 33.9 L/sec (0.034 cms)
## Beaver: Q = 107.102 L/sec (0.107 cms)

#######################
## Estimate Velocity ##
#######################

inj_time <- as.POSIXct("2018-09-28 07:23:00") #Lolomai
#inj_time <- as.POSIXct("2018-09-28 09:34:00") #Willow
#inj_time <- as.POSIXct("2018-08-23 07:23:00") #Blaine
#inj_time <- as.POSIXct("2018-08-31 07:23:00") # Beaver

peak_time <- dat[which.max(dat$Cond),]$DateTime

time_diff_sec <- as.numeric(peak_time - inj_time)*60

## Velocity = distance in meters/time in seconds

## Distance upstream
Oak_Lolomai_dist_upstream <- 107.5 # Oak @ Lolomai: 107.5 m upstream
Oak_Willow_dist_upstream <- 150 # Oak @ Willow: 150 m upstream
Blaine_dist_upstream <- 50 # Blaine: 50 m upstream
Beaver_dist_upstream <- 217 # Beaver: 217 m upstream

v <- Oak_Lolomai_dist_upstream/time_diff_sec ## *replace depending on calculation
v

## Summary
## Oak @ Lolomai: 0.346 m/s
## Oak @ Willow: 0.199 m/s
## Blaine: 0.069 m/s
## Beaver: 0.066 m/s

#############################
## Estimate mean depth (z) ##
#############################
## effective depth (z) can be estimated using the following equation
##
## z = Q/(w*v)
## where z is effective depth (m)
## Q is discharge (m^3/sec)
## w is average width (m)
## v is velocity (m/sec)

## Enter average width measurement in m
# Oak @ Lolomai: 8.6
# Oak @ Willow: 11.8
# Blaine: 2.05
# Beaver: 8.92  (from upstream of cattle crossing -- need better)

w <- 8.6

## Calculate effective depth
z <- (Q/1000)/(w*v)
z

## Summary
## Oak @ Lolomai: 0.17 m
## Oak @ Willow: 0.31 m
## Blaine: 0.24 m
## Beaver: 0.18 m (based on estimated width)









