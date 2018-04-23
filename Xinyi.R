accident2014=read.csv("Accident 2014.csv")
accident2015=read.csv("Accident 2015.csv")
accident2016=read.csv("Accident 2016.csv")
accident=rbind(accident2014,accident2015,accident2016)
library(ggplot2)
library(dplyr)


###Lighting Conditions 

# Lighting conditions are marked by numbers from 1 to 7. 1 stands for the brightest lighting condition
# and 7 stands for the darkest. 

## Hypothesis 1: The number of car accidents could be higher in darker conditions.
dim(accident)
accident_unique=accident %>% distinct(Reference.Number,.keep_all = T) 
# Select unique Reference Number to avoid repetitive counting.
dim(accident_unique)


ggplot(accident_unique, aes(Lighting.Conditions,color="light red",fill="light red")) + 
  geom_bar()+xlab("Lighting Conditions")+ylab("Number of Accidents")+
  ggtitle("Influence of Lighting Conditions on Number of Accidents")+guides(fill=F,color=F)+
  scale_y_continuous(breaks=seq(0,900,100))+
  scale_x_continuous(breaks=seq(1,7,1))

# We assume that the number of accidents might increase as the lighting condition 
# becoming worse due to the visibility. However, the barplot shows that most of the accidents 
# occurred when there was daylight with street lights present which is the brightest. 
# Therefore, Hypothesis 1 is not true.



## Hypothesis 2: The Casualty Severity could be higher in darker conditions.

mode(accident$Casualty.Severity)
accident$Casualty.Severity= as.factor(accident$Casualty.Severity)
levels(accident$Casualty.Severity) = c("Fatal","Serious","Slight")
ggplot(accident, aes(x=Lighting.Conditions,color=Casualty.Severity,fill=Casualty.Severity)) + 
  geom_histogram(position = "fill")+xlab("Lighting Conditions")+
  ylab("Percentage of Different Severity")+
  ggtitle("Influence of Lighting Conditions on Casualty Severity showed by Percentage")+
  scale_x_continuous(breaks=seq(1,7,1))
# The percentage of different casualty severity actually fluctuates as the lighting conditions getting worse.
# There is no sign that the percentage of fatal or serious casualty went up in darker lighting conditions.
# Therefore, Hypothesis 2 might not be true.


ggplot(accident, aes(x=Lighting.Conditions)) + 
  geom_histogram(position = "dodge")+facet_grid(Casualty.Severity~.,scales = "free")+xlab("Lighting Conditions")+
  ylab("Number of Casualty")+ggtitle("Influence of Lighting Conditions on Casualty Severity showed by Counting")+
  scale_x_continuous(breaks=seq(1,7,1))
# The graph does not show the number of fatal casualty went up when the Lighting Conditions got worse. 

accident$Casualty.Severity= as.numeric(accident$Casualty.Severity)
ggplot(accident, aes(x=Lighting.Conditions,y=Casualty.Severity)) + 
 geom_bar(stat="summary", fun.y="mean", position="dodge")+xlab("Lighting Conditions")+
  ylab("Casualty Severity")+ggtitle("Influence of Lighting Conditions on Casualty Severity showed by Average")+
  scale_y_continuous(breaks=seq(0,3,0.5))+
  scale_x_continuous(breaks=seq(1,7,1))
# The Casualy Severity is rated by numbers from 1 to 3. 1 stands for Fatal, 2 stands for Serious and
# 3 is Slight.The graph shows average values of Casualy Severity. If Hypothesis 2 is true, 
# the average values should go down as the Lighting Conditions varying from 1 to 7. 
# Obviously,  there is no such a trend in the graph so that Hypothesis 2 is false.

### Weather Conditions

## Hypothesis 1: High winds would cause more car accidents.
# As for Weather Conditions, 1 to 3 represents conditions without high winds and 4 to 6 stands for the opposite.

accident_unique %>% filter(Weather.Conditions %in% c(1,2,3)) %>% summarise(n())
# 1158 accidents happened without high winds.
accident_unique %>% filter(Weather.Conditions %in% c(4,5,6)) %>% summarise(n())
# 55 accidents happened with high winds.
# The conclusion is obvious that high winds would not cause more car accidents 
# which means Hypothesis 1 is false.

## Hypothesis 2: Snow, rain would cause more car accidents.

accident_unique %>% filter(Weather.Conditions %in% c(1,2,3)) %>% 
  ggplot(aes(x=Weather.Conditions))+geom_bar(color="light blue",fill="light blue")+
  scale_x_continuous(breaks=seq(1,3,1),label=c("Fine","Raining","Snowing"))+
  labs(x="Weather Conditions",y="Number of Accidents",title="Influence of Weather Conditions without High Winds")
# As we can see, when there was no high winds, raining and snowing did not cause more accidents. 
# Conversely, most of the accidents happened when the weather is fine without high winds.

accident_unique %>% filter(Weather.Conditions %in% c(4,5,6)) %>% 
  ggplot(aes(x=Weather.Conditions))+geom_bar(color="pink",fill="pink")+
  scale_x_continuous(breaks=seq(4,6,1),label=c("Fine","Raining","Snowing"))+
  labs(x="Weather Conditions",y="Number of Accidents",title="Influence of Weather Conditions with High Winds")
# If there was high winds, the number of accidents in raining days did increase by almost four times
# than the fine days. However, snowing days did not show such pattern.
# As a result, we can also conclude that Hypothesis 2 is also false.


### Type of Vehicle

## Hypothesis 1: Motorcycles with higher Cylinder Capacity(cc) would have higher possibility 
## of more and severer car accidents since their engines are more powerful.
accident_unique %>% filter(Type.of.Vehicle %in% c(2,3,4,5)) %>% 
  ggplot(aes(x=Type.of.Vehicle))+geom_bar(color="light yellow",fill="light yellow")+
  scale_x_continuous(breaks=seq(2,5,1),label=c("50cc and under","50-125cc","125-500cc","over 500cc"))+
  labs(x="Type of Vehicle",y="Number of Accidents",title="Influence of Cylinder Capacity")
# We can conclude from the graph that there is no positive linear correlation between Cylinder Capacity
# and the number of accidents. Therefore, Hypothesis 1 is not true.

## Hypothesis 2: Goods vehicles with higher maximum gross weight(MGW) would have higher 
## possibility of severer car accidents since they are heavier.
accident %>% filter(Type.of.Vehicle %in% c(19,20,21)) %>% 
  ggplot(aes(x=Type.of.Vehicle,y=Casualty.Severity)) + 
  geom_bar(stat="summary", fun.y="mean", position="dodge")+xlab("Type of Vehicle")+
  ylab("Casualty Severity")+ggtitle("Influence maximum gross weight of goods vehicles")+
  scale_y_continuous(breaks=seq(0,3,0.5))+
  scale_x_continuous(breaks=seq(19,21,1),label=c("3.5 tonnes mgw and under","3.5-7.5 tonnes mgw","7.5 tonnes mgw and over"))
# As we know, the higher the average is, the severer the slighter the Casualty Severity is.
# The barplot tells us that Goods vehicle with 3.5 tonnes mgw and under would cause more severe car accidents
# than the other two even if the difference is not much. Of course, Hypothesis 2 is false.

## Hypothesis 3: Tram/Light rail or Ridden horse may have the lowest number of accidents. 
accident_unique$Type.of.Vehicle = as.factor(accident_unique$Type.of.Vehicle)
accident_unique %>% ggplot(aes(x=Type.of.Vehicle))+geom_bar(color="orange",fill="orange")+
  labs(x="Type of Vehicle",y="Number of Accidents",title="Car Accidents Caused by Different Types of Vehicle")
# As we can see, vehicle type 9 which refers to Car had much more accidents than the others.

summary(accident_unique$Type.of.Vehicle)
# Vehicle type 10, 16, 17, 18, 22, 23 had one or two accidents in year 2014-2016, which means they were 
# safer comparatively. For vehicle types 10, 16, 17, 18, 22, they are "Minibus (8 or 16 passenger seats)", "Ridden horse",
# "Agricultural vehicle (includes diggers etc.)", "Tram / Light rail" and "Mobility Scooter" respectively.
# For vehicle type 23, there is no record in the file "Guidance", we have no idea of that.


