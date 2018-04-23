accident14 = read.csv('Accident 2014.csv')
accident15 = read.csv('Accident 2015.csv')
accident16 = read.csv('Accident 2016.csv')
str(accident14)

head(accident14)

library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)

###############################################################################
accident=rbind(accident14,accident15,accident16)
accident$Accident.Date=dmy(accident$Accident.Date)
accident$Year=year(accident$Accident.Date)
accident$Month=month(accident$Accident.Date)

accident$Year=as.factor(accident$Year)
accident$Month=as.factor(accident$Month)

# Summary Table: Accidents and Injuries in 2014-2016
accident %>% filter(Year==2014) %>% summarise(n_distinct(Reference.Number))
accident %>% filter(Year==2015) %>% summarise(n_distinct(Reference.Number))
accident %>% filter(Year==2016) %>% summarise(n_distinct(Reference.Number))
accident %>% filter(Year==2014) %>% summarise(n())
accident %>% filter(Year==2015) %>% summarise(n())
accident %>% filter(Year==2016) %>% summarise(n())

Year=c("2014","2015","2016")
Accidents=c(427,410,407)
Injuries=c(623,556,555)
table=data.frame(Year,Accidents,Injuries)
table

# Injuries each month for 2014-2016
ggplot(accident,aes(x=Month,fill=Year))+geom_bar(position="dodge",color="black",width=0.5)+
  ggtitle("Injuires Each Month for 2014-2016")+ylab("Injuries")

# Injuries in each hour
names(accident)[6]=paste("Time.24hr")
accident$Time.24hr=str_pad(accident$Time.24hr,width = 4,side=c("left"),pad="0")
accident$Hour=substr(accident$Time.24hr,start=1,stop=2)
ggplot(accident,aes(x=Hour,fill=..count..))+geom_bar()+ggtitle("Injuries in Hours")+
    ylab("Injuries")+xlab("Hour")+scale_fill_gradient("Injuries",low='green',high='red')
 
