accident14 = read.csv('Accident 2014.csv')
accident15 = read.csv('Accident 2015.csv')
accident16 = read.csv('Accident 2016.csv')
accident_all = rbind(accident14,accident15,accident16)
str(accident14)
library(dplyr)
#Install 'rnrfa' package to do the UTM to lat/lon conversion 
install.packages("rgdal")
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

# Overview 
UK_map_overview= get_map(location = c(mean(longlat$long),mean(longlat$lat)), zoom =10,maptype='roadmap', color='bw')
ggmap(UK_map_overview)+ geom_point(data=longlat,aes(x=long,y=lat,shape=Road_Surface,color=Road_Surface),size=2)+
  scale_x_continuous(limits=c(min(longlat$long),max(longlat$long)),expand=c(0,0))+
  scale_y_continuous(limits=c(min(longlat$lat),max(longlat$lat)),expand=c(0,0))

#Take a closer look at Halifax
UK_map_Halifax= get_map(location = c(-1.87,53.7270), zoom =14,maptype='hybrid', color='bw')
ggmap(UK_map_Halifax)+geom_point(data=longlat,aes(x=long,y=lat,shape=as.factor(roadsurface),color=as.factor(roadsurface)),size=3)
  
