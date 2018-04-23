Car2014=read.csv("Accident 2014.csv")
Car2015=read.csv("Accident 2015.csv")
Car2016=read.csv("Accident 2016.csv")

library(dplyr)
library(ggplot2)

head(Car2014)

#Sex of casualty against casualty class and number of car accidents, 
#only considering casualty class 1 - drivers and riders. 
Car2014 %>% filter(Casualty.Class==1,Sex.of.Casualty==1) %>% summarise(MaleAccidents=n())
Car2014 %>% filter(Casualty.Class==1,Sex.of.Casualty==2) %>% summarise(FemaleAccidents=n())

Car2015 %>% filter(Casualty.Class==1,Sex.of.Casualty==1) %>% summarise(MaleAccidents=n())
Car2015 %>% filter(Casualty.Class==1,Sex.of.Casualty==2) %>% summarise(FemaleAccidents=n())

Car2016 %>% filter(Casualty.Class==1,Sex.of.Casualty==1) %>% summarise(MaleAccidents=n())
Car2016 %>% filter(Casualty.Class==1,Sex.of.Casualty==2) %>% summarise(FemaleAccidents=n())

MaleAccidents = c(247,223,203)
FemaleAccidents =c(104,112,111)
Year=c(2014,2015,2016)
SexOfAccidents = data.frame(Year,MaleAccidents,FemaleAccidents)
SexOfAccidents

# Casualty class against the number of accidents.

Car2014 %>% filter(Casualty.Class==1) %>% summarise(DriverCasualty=n())
Car2014 %>% filter(Casualty.Class==2) %>% summarise(PassengerCasualty=n())
Car2014 %>% filter(Casualty.Class==3) %>% summarise(PedestrianCasualty=n())

Car2015 %>% filter(Casualty.Class==1) %>% summarise(DriverCasualty=n())
Car2015 %>% filter(Casualty.Class==2) %>% summarise(PassengerCasualty=n())
Car2015 %>% filter(Casualty.Class==3) %>% summarise(PedestrianCasualty=n())

Car2016 %>% filter(Casualty.Class==1) %>% summarise(DriverCasualty=n())
Car2016 %>% filter(Casualty.Class==2) %>% summarise(PassengerCasualty=n())
Car2016 %>% filter(Casualty.Class==3) %>% summarise(PedestrianCasualty=n())

DriverCasualty=c(351,335,314)
PassengerCasualty=c(175,119,138)
PedestrianCasualty=c(97,102,103)
ClassOfCasualty = data.frame(Year,DriverCasualty,PassengerCasualty,PedestrianCasualty)
ClassOfCasualty

# Vehicle type against the Casualty Severity and the number of accidents.

Car2014 %>% filter(Casualty.Severity ==1) %>% select(Type.of.Vehicle)
Car2015 %>% filter(Casualty.Severity ==1) %>% select(Type.of.Vehicle)
Car2016 %>% filter(Casualty.Severity ==1) %>% select(Type.of.Vehicle)


Car2014 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==9))

Car2015 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==9))
Car2015 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==90))

Car2016 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==1))
Car2016 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==5))
Car2016 %>% filter(Casualty.Severity ==1) %>% summarise(sum(Type.of.Vehicle==9))

PedalCycleDeath=c(0,0,2)
MotorcycleOver500ccDeath=c(0,0,2)
CarDeath=c(6,5,5)
OtherVehicleDeath=c(0,1,0)

TypeOfVehicleDeath=data.frame(Year,PedalCycleDeath,MotorcycleOver500ccDeath,CarDeath,OtherVehicleDeath)
TypeOfVehicleDeath
