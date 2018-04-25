Data Analysis on UK Car Accidents & Injuries
================
Kaixiang Huang, Yanqi Huang, Xinyi Hou, Xinhui Peng, Aiwei Li
April 23, 2018

Introduction, Objective and Motivation
--------------------------------------

Road traffic safety has always been a paramount concern in our daily life. It is reported that about 1,250,000 people were killed by traffic-related accidents in 2015 \[1\]. The objective of the project is to analyze the data of car accidents at certain area in UK from 2014 to 2016 and to figure out useful insights from the data graphically. Factors like time period, road conditions, lighting conditions, and sex etc. might be correlated with the frequency of accidents. We attempt to find these correlations and make recommendations for improving road safety. The dataset is published by UK Department for Transportation \[2\] and available from our Github repository \[3\].

Statistical Insights
--------------------

### Locations vs. Accidents & Injuries

``` r
accident14 = read.csv('Accident 2014.csv')
accident15 = read.csv('Accident 2015.csv')
accident16 = read.csv('Accident 2016.csv')
accident_all = rbind(accident14,accident15,accident16)
library(dplyr)
library(rgdal)
library(proj4)
#Accident in 2014
coords14 = cbind(long = as.numeric(as.character(accident14$Grid.Ref..Easting)),
               lat = as.numeric(as.character(accident14$Grid.Ref..Northing)))
GP_SP14 = SpatialPoints(coords14,proj4string = CRS("+init=epsg:27700"))
GP_SP_LL14 = spTransform(GP_SP14,CRS("+init=epsg:4326"))
longlat14=as.data.frame(GP_SP_LL14)

#Accident in 2015
coords15 = cbind(long = as.numeric(as.character(accident15$Grid.Ref..Easting)),
                 lat = as.numeric(as.character(accident15$Grid.Ref..Northing)))
GP_SP15 = SpatialPoints(coords15,proj4string = CRS("+init=epsg:27700"))
GP_SP_LL15 = spTransform(GP_SP15,CRS("+init=epsg:4326"))
longlat15=as.data.frame(GP_SP_LL15)
#Accident in 2016 
coords16 = cbind(long = as.numeric(as.character(accident15$Grid.Ref..Easting)),
                 lat = as.numeric(as.character(accident15$Grid.Ref..Northing)))
GP_SP16 = SpatialPoints(coords16,proj4string = CRS("+init=epsg:27700"))
GP_SP_LL16 = spTransform(GP_SP16,CRS("+init=epsg:4326"))
longlat16=as.data.frame(GP_SP_LL16)
longlat16=head(longlat16,555)
#combine the data for three years 
longlat = rbind(longlat14,longlat15,longlat16)
longlat = mutate(longlat,Road_Surface = accident_all$Road.Surface)
longlat$Road_Surface=as.factor(longlat$Road_Surface)
#Plot UK map 
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(knitr)
opts_knit$set(eval.after = 'fig.cap')
UK_map_overview= get_map(location = c(mean(longlat$long),mean(longlat$lat)), zoom =10,maptype='roadmap', color='bw')
UK_map_Halifax= get_map(location = c(-1.87,53.7270), zoom =14,maptype='hybrid', color='bw')
```

![Figure.1](Report_summary_files/figure-markdown_github/figs-1.png) 

``` r
ggmap(UK_map_Halifax)+geom_point(data=longlat,aes(x=long,y=lat,shape=Road_Surface,color=Road_Surface),size=3)
```

![Figure.2 balalala](Report_summary_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Time vs. Accidents & Injuries

``` r
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


Year=c("2014","2015","2016")
Accidents=c(427,410,407)
Injuries=c(623,556,555)
table=data.frame(Year,Accidents,Injuries)
```

``` r
ggplot(accident,aes(x=Month,fill=Year))+geom_bar(position="dodge",color="black",width=0.5)+
  ggtitle("Injuires Each Month for 2014-2016")+ylab("Injuries")
```

![Figure.3 balalala](Report_summary_files/figure-markdown_github/unnamed-chunk-4-1.png)