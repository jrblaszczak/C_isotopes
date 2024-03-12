


#########################
## CALCULATE CO2 IN H20 (in the post-processing files now)
#########################
#I (Bob) think this is correct

CO2calc2<-function(x) {
  x$kH_CO2<-29* exp( 2400 * ((1/(x$equiltemp+273.15)) - 1/(298.15)) ) #temp correction for Henry's law
  x$CO2_mol_water<- x$CO2_vol_air  / x$kH_CO2
  x$CO2_mol_air<- x$CO2_vol_air/ ((x$equiltemp+273.15)*0.08026/x$pressure)
  
  x$CO2_conc<-(x$CO2_mol_water*x$volwater + x$CO2_mol_air*x$volair)/100 # µmol/L or mmol/m3
  return(x)
  
}


## Need to set certain columns to desired values if all the same
## If each sample is different, simple set the column to a column specified in the metadata file
names(sum_file)

sum_file$CO2_vol_air <- sum_file$CO2
sum_file$equiltemp <- sum_file$WaterTemp_C
sum_file$volair <- sum_file$air_mL
sum_file$volwater <- sum_file$H2O_mL
sum_file$pressure <- 0.9

## Run function on datafile
sum_file <- CO2calc2(sum_file)
names(sum_file)

ggplot(sum_file, aes(SampleID, CO2_conc))+
  geom_point(size=3)+
  theme(axis.text.x=element_text(angle=45, hjust = 1))
