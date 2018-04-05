accident14 = read.csv('Accident 2014.csv')
accident15 = read.csv('Accident 2015.csv')
accident16 = read.csv('Accident 2016.csv')
str(accident14)

#Install 'rnrfa' package to do the UTM to lat/lon conversion 
install.packages("rgdal")
library(rgdal)
library(proj4)


coords = cbind(long = as.numeric(as.character(accident14$Grid.Ref..Easting)),
               lat = as.numeric(as.character(accident14$Grid.Ref..Northing)))
GP_SP = SpatialPoints(coords,proj4string = CRS("+init=epsg:27700"))
GP_SP_LL = spTransform(GP_SP,CRS("+init=epsg:4326"))
longlat=as.data.frame(GP_SP_LL)
#Plot UK map 
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
UK_map= get_map(location = c(-1.88742,53.709), zoom =11)
ggmap(UK_map)+ geom_point(data=longlat,aes(x=long,y=lat))
