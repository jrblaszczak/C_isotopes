
############################################################################################################################################################################
# Processing raw MIMS data to calculate concentrations & ratios
# Hilary Madinger and Bob Hall
# 16 July 2016

# Execute the functions in MIMS_gas_functions.R before this code. 
# MIMS_gas_functions.R calculates gas concentrations at saturation, while correcting for water density, temperature, and barometric presssure, which are used in the functions below. 
# Using gas saturations, two water bath calibrations (at different temperatures), and the data copied and pasted from the MIMS, this code converts mass signals into concentrations/ratios. 
############################################################################################################################################################################


###########################################
# DATA IMPORT AND MANAGEMENT
###########################################	
source("MIMS_gas_functions.R")

# I have had problems with .csv files removing decimal places for values from the MIMS. One way to fix this is to use the readxl package to read in an excel file instead of a .csv file.
install.packages("readxl", dependencies = T)
library(readxl)

# Call and name the MIMS data. 
MIMSdata<-read_excel("2018_09_03_BeaverDiel_MIMS.xlsx")

# The needed columns for this code include: 
# X28, X32, X40, N2.Ar, O2.Ar = columns from the MIMS output. These can come from MIMS_datasheet_mean_example.R too.  
# Temp = waterbath or water sample temperature in Celcius
# Pressure = air pressure when sample was collected or when running samples in mmHg (barometric pressure conversions also in MIMS_gas_functions.R)
# Sampletype = datasheet column distinguishing data from calibrating the MIMS and samples (Calibrate/Samp)
# Calibnum = datasheet column where each calibration period has a seperate, sequential number (1, 2, 3, ect. )
# Calibtype = datasheet column distinguishing colder and warmer calibration temperatures (Low/High)
# Useful metadata may include sample ID, treatment type, sample location, time and date of sample collection, ect. 
# There is also a comments column for including any irregularities or observations while running the MIMS


###########################################
# MIMScalc FUNCTION to convert raw numbers to ratios/concentrations
###########################################	

# MIMScalc function first calculates raw ratios again for decimal accuracy and calculates saturation values for future use in the function. 
# Next, the function divides the data file in to the calibration data and sample data (Calibrate and Samp)
# In a loop, it calculates a linear regression for each group of samples using the pre and post calibration numbers for that sample. 
# The samples are then fit to the line for each regression. 
# At the end of the function, the data is all put into an output datafile that is saved to the computer. 

MIMScalc<-function(MIMSdata){
  nar<- (MIMSdata$N2.Ar)     
  MIMSdata$nar<-nar
  O2ar<-(MIMSdata$O2.Ar)     
  MIMSdata$O2ar<-O2ar
  watdensv<-watdens(MIMSdata$Temp)
  MIMSdata$watdensv<-watdensv
  osat1v<-osat1(MIMSdata$Temp, MIMSdata$Pressure)
  MIMSdata$osat1v<-osat1v
  nsatv<-nsat(MIMSdata$Temp, MIMSdata$Pressure)
  MIMSdata$nsatv<-nsatv
  arsatv<-arsat(MIMSdata$Temp, MIMSdata$Pressure)
  MIMSdata$arsatv<-arsatv
  narsat<- (MIMSdata$nsatv / MIMSdata$arsatv)
  MIMSdata$narsat<-narsat
  O2arsat<- (MIMSdata$osat1v / MIMSdata$arsatv)
  MIMSdata$O2arsat<-O2arsat
  
  #subsampling
  calibMIMSdata<-MIMSdata[MIMSdata$Sampletype=="Calibrate",]
  samplesMIMSdata<-MIMSdata[MIMSdata$Sampletype=="Samp",]

  
## For each ratio, I use a linear model from the calibration data to convert the MIMS data (nar) into a corrected ratio (narcalc). 
## The loop calculates the values for samples inbetween each calibration interval. 
    
  #linear model for nar
  lmresult<-matrix(nrow=max(as.numeric(calibMIMSdata$Calibnum))-1,ncol=2)
  for (i in 1:(max(as.numeric(calibMIMSdata$Calibnum))-1)) {
    calibs<-calibMIMSdata[as.numeric(calibMIMSdata$Calibnum)==i | as.numeric(calibMIMSdata$Calibnum) == i+1,]
    output<-lm(calibs$narsat~calibs$nar)
    lmresult[i,]<-coef(output)
  }
  
  samplesMIMSdata$coef1<-lmresult[as.numeric(samplesMIMSdata$Sampnum),1]
  samplesMIMSdata$coef2<-lmresult[as.numeric(samplesMIMSdata$Sampnum),2]
  
  #calc concentration for nar
  samplesMIMSdata$narcalc<-samplesMIMSdata$coef1+samplesMIMSdata$coef2*samplesMIMSdata$nar
  
## Using the corrected ratio, I have another linear model to calculate the argon concentration (arconc)
  
  #linear model for ar
  lmresultar<-matrix(nrow=max(as.numeric(calibMIMSdata$Calibnum))-1,ncol=2)
  for (i in 1:(max(as.numeric(calibMIMSdata$Calibnum))-1)) {
    calibs<-calibMIMSdata[as.numeric(calibMIMSdata$Calibnum)==i | as.numeric(calibMIMSdata$Calibnum) == i+1,]
    output<-lm(calibs$arsatv~as.numeric(calibs$X40))
    lmresultar[i,]<-coef(output)
  }
  
  samplesMIMSdata$arcoef1<-lmresultar[as.numeric(samplesMIMSdata$Sampnum),1]
  samplesMIMSdata$arcoef2<-lmresultar[as.numeric(samplesMIMSdata$Sampnum),2]
  
  #calc concentration for ar
  samplesMIMSdata$arconc<-samplesMIMSdata$arcoef1+samplesMIMSdata$arcoef2*as.numeric(samplesMIMSdata$X40)
  
  #calc n by proportion with nar and ar
  samplesMIMSdata$nconc<-samplesMIMSdata$nar*samplesMIMSdata$arconc
  
## Argon and Oxygen have nearly identical Schmidt numbers so the saturation values for argon and oxygen do not vary by temperature compared to one another. 
## As a result, we only need information from one temperature. In this case I am using the warmer temperature for calculations. 
## The calculation pattern is otherwise similar to calculating nitrogen values. 
  
  #subset just the high temperature data for O2 concentration calculation
  HighO2<-calibMIMSdata[calibMIMSdata$Calibtype=="High",]
  
  #X32 current O2 concentration calculation
  O2result<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
  for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
    calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
    output<-mean(calibs$X32)
    O2result[i,]<-(output)
  }
  
  #O2 saturation calculation
  O2satresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
  for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
    calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
    output<-mean(calibs$osat1v)
    O2satresult[i,]<-(output)
  }
  
  O2calibresult<-O2satresult/O2result 
  
  samplesMIMSdata$O2conc<-(O2calibresult[as.numeric(samplesMIMSdata$Sampnum),1])*samplesMIMSdata$X32
    
#calculating the O2/Ar with only one calculated value to reduce the error 
#O2ar current calculation
O2arresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
  calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
  output<-mean(calibs$O2ar)
  O2arresult[i,]<-(output)
}

#O2ar saturation calculation
O2arsatresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
  calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
  output<-mean(calibs$O2arsat)
  O2arsatresult[i,]<-(output)
}

O2arcalibresult<-O2arsatresult/O2arresult 
samplesMIMSdata$O2arcalc<-(O2arcalibresult[as.numeric(samplesMIMSdata$Sampnum),1])*samplesMIMSdata$O2ar

  
  outdf<-samplesMIMSdata
  outdf
}

###########################################
# EXPORT DATAFILE
###########################################

MIMS_r_outdf <-MIMScalc(MIMSdata)   #Name the file made in the MIMScalc function anything you would like.


write.csv(MIMS_r_outdf, "2018_09_03_BeaverDiel_MIMSprocessed.csv")  #Save the file to your computer or dropbox.

## The new datafile is only data from samples, no calibration numbers. 
## You can delete the new first column without a header (it's just a count of the samples). 

## The resulting narcalc and O2arcalc columns are the ratios you then use for calculations. Nar and O2Ar are unprocessed numbers. 
## The arconc, nconc, and O2conc are the now calibrated concentration values. 
## The ratios are more accurate than concentrations calculated from the MIMS samples because the machine is better at measureing ratios of masses than individual masses. 


