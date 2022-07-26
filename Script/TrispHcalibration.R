## Tris Calibration of pH using an Orion Multiparameter probe measuring mV, temperature, and salinity

rm(list=ls())

library(tidyverse)
library(seacarb)
library(broom)

## bring in pH calibration files and raw data files
pHcalib<-read_csv("Data/Experiment_Data/Mesocosm_Tank_pH/TrispHcalibration.csv")
pHData<-read_csv("Data/Experiment_Data/Mesocosm_Tank_pH/pH_mesocosm.csv")

#mVTris is milivolts of the tris calibration
#TTris is temperature of the tris calibration 
#there needs to be at least three points of Tris values at different temperature

#fit a linear model between temperature and mV
todaypHcalib<-pHcalib %>%
  filter(date=='20220902')

mVTris_t<-lm(mVTris~TTris, data =todaypHcalib)
print(summary(mVTris_t))
#make sure they are significant and R^2 is around 0.99

#plot temperature vs millivolt -- make sure it looks like a straight line
pHTrisplot<-ggplot(data =todaypHcalib, aes(x=TTris, y=mVTris))+
  geom_line() +
  theme_classic()
pHTrisplot

## take the mV calibration files by each date and use them to calculate pH
pHSlope<-pHcalib %>%
  nest_by(date)%>%
  mutate(fitpH = list(lm(mVTris~TTris, data = data))) %>% # linear regression of mV and temp of the tris
  summarise(broom::tidy(fitpH)) %>% # make the output tidy
  select(date, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
  left_join(.,pHData) %>% # join with the pH sample data
  mutate(mVTris = TempInSitu*TTris + `(Intercept)`) %>%# calculate the mV of the tris at temperature in which the pH of samples were measured
  drop_na() %>%
  mutate(pH = pH(Ex=mV,Etris=mVTris,S=Salinity_lab,T=TempInSitu)) %>% # calculate pH of the samples using the pH seacarb function
  #mutate(pH_insitu = pHinsi(pH = pH, ALK = TA_Raw, Tinsi = TempInSitu, Tlab = Temp, S = Salinity_lab_Silbiger)) %>%
  select(date, SampleID,Salinity_lab,pH, TempInSitu) ## need to calculate pH insi then it is done

# or if the above doesn't work
#pHSlope<-pHcalib %>%
#  group_by(date)%>%
#  do(fitpH = lm(mVTris~TTris, data = .))%>% # linear regression of mV and temp of the tris
#  tidy(fitpH) %>% # make the output tidy
#  select(date, term, estimate) %>%
#  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
#  left_join(.,pHData) %>% # join with the pH sample data
#  mutate(mVTris = TempInSitu*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
#  drop_na() %>%
#  mutate(pH = pH(Ex=mV,Etris=mVTris,S=Salinity_lab,T=TempInSitu)) %>% # calculate pH of the samples using the pH seacarb function
#  #mutate(pH_insitu = pHinsi(pH = pH, ALK = TA_Raw, Tinsi = TempInSitu, Tlab = Temp, S = Salinity_lab_Silbiger)) %>%
#  select(date, SampleID,Salinity_lab,pH, TempInSitu) ## need to calculate pH insi then it is done

View(pHSlope)

## write the data
write_csv(pHSlope, paste0('Data/Experiment_Data/Mesocosm_Tank_pH/pHTotal.csv'))


#consider creating another datasheet that has the range, ave, standard error for each of the pH groups