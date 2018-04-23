accident14=read.csv("Accident 2014.CSV")
accident15=read.csv("Accident 2015.CSV")
accident16=read.csv("Accident 2016.CSV")
head(accident15)
library(lubridate)
library(dplyr)
library(ggplot2)
accident=rbind(accident14,accident15,accident16)

accident$Accident.Date=dmy(accident$Accident.Date)
accident$year=year(accident$Accident.Date)
accident$month=month(accident$Accident.Date)

accident$year=as.factor(accident$year)
accident$month=as.factor(accident$month)
accident$X1st.Road.Class...No=as.numeric(accident$X1st.Road.Class...No)
accident$Casualty.Severity=as.numeric(accident$Casualty.Severity)
class(accident$year)


accident_unique=accident%>%distinct(Reference.Number,.keep_all=T)
#roadclass and accident numbers 
ggplot(accident_unique,aes(x=X1st.Road.Class,fill=year))+geom_bar(position="dodge",color="black",width=0.5)+xlab("Road class") +
  ylab("Accident Count") +ggtitle("Roadclass and accident numbers in 2014-2016")

#top 5 road number (use this )

Countroadnumber=accident_unique%>%group_by(X1st.Road.Class...No)%>%summarise(countnumber=n())
top10roadnumber=Countroadnumber%>%arrange(desc(countnumber))%>%slice(1:10)
ggplot(top10roadnumber,aes(x=as.factor(X1st.Road.Class...No),y=countnumber))+geom_bar(stat="identity",position="dodge",color="white",fill="darkred",width=0.5)+xlab("Road class No")+
  ylab("Accident Count") +ggtitle("Top 10 Road NO in 2014-2016")+scale_y_continuous(breaks=c(0,100,200,300,400,500))


#roadsuface and accident numbers
ggplot(accident_unique,aes(x= Road.Surface))+geom_histogram(binwidth = 0.5,color="white",fill="darkred")+xlab("Road Surface") +
  ylab("Accident Count") +ggtitle("Road Surface impact on accident number 2014-2016")

ggplot(accident,aes(x= Road.Surface,y=Casualty.Severity))+geom_histogram(stat="identity")


