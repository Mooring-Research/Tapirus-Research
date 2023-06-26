####################
####################
title: "South Covariates"
author: "Lily Bright"
date: "6/1/2022"
output: html_document
####################
####################

rm(list=ls())

#import data
setwd("/Users/lilybright/Documents/PLNU/Summer Research 2022/Official Data 2")
getwd()
dir()

cts0 <- read.csv("CT_Locations_CR_Dataset01_South_Lily.csv")
cts0
head(cts0)

#spatial data
class(cts0$Long) #lat/long recognized as numeric (- symbol denotes west hemi/south hemi)
class(cts0$Lat)

library(maptools)
library(rgdal)
library(raster)
library(sp)

coord <- SpatialPoints(coords = cts0[,c(3,2)],
                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
coord


plot(coord) #plots data on blank space with "x's"
plot(coord, pch = 1) #made plots circles

###Elevation and Human Footprint Impact###
#import raster
elev <- raster("SRTM_CostaRica_1arc.tif")
Hfi<- raster("HFI_CostaRica_EPSG4326.tif")

head(cts)

#Elevation and HFI Covariates
Elev <- extract(elev, coord) 
HFI <- extract(Hfi, coord) 
#cbind(cts0, c(,Elev, HFI))

cts1<-cbind(cts0[,1:3], Elev)
cts2<-cbind(cts1[,1:4], HFI)

#code for chart of relations between 2
plot(coord, pch = 1, col = "red")
plot(Elev, add = T)

plot(coord, pch = 1, col = "red")
plot(HFI, add = T)

write.csv(cts2, file ="Ct_South_ElevHFI.csv")

####Waterrrrr Covariate###
#(didnt end up using because data not significant (all 0s))
#import raster
watercov <- raster("WaterSurf_CostaRica_EPSG4326.tif")
plot(watercov) #stacked in order you listed in, (added map cover)
plot(coord, pch =19, col="red", add =T) #added points (in red)


#crop map with some room
ext<- extent(coord) + c(-0.02, 0.02,-0.02, 0.02 )
watercov2<- crop(watercov, ext)
plot(watercov2)
plot(coord, pch =19, col="black", add =T)

waterdist <- extract(watercov2, coord, buffer=20000, fun=mean) #numbers are very small so might not be a good cov
waterdist

####Primary Forest###
#import raster
primforest <- raster("PrimForest_CostaRica_EPSG4326.tif")
plot(primforest) 
plot(coord, pch =19, col="red", add =T) 

#plot map with coords (too big)
#crop map with some room
ext<- extent(coord) + c(-.02, .02,-.05, .05 )
primforest2<- crop(primforest, ext)
plot(primforest2)
plot(coord, pch =19, col="black", add =T)

primForest <- extract(primforest2, coord) 
ct3<-cbind.data.frame(cts2, primForest)
write.csv(ct3, file ="Site_Covariates.csv")

###Net Productivity###
#import raster
npp <- raster("NPP_CostaRica_1arc.tif")

#Extract NetProd from raster using point vector
NPP <- extract(npp, coord)

#add NPP to Site covaritate documents
ct4<-cbind.data.frame(ct3, NPP)
write.csv(ct4, file ="Site_Covariates.csv")


##Distance to Road and Rivers
#import raster
dtrr <- read.csv("Distances_CR_Dataset01_Lily.csv")

#Extract NetProd from raster using point vector
DTRR <- extract(dtrr, coord)
d.Road<-dtrr$d.Road
d.River<-dtrr$d.River
DTRR<- cbind(d.Road,d.River)

#add NPP to Site covaritate documents
ct5<-cbind.data.frame(ct4, DTRR)
write.csv(ct5, file ="Site_Covariates.csv")
