rm(list=ls())
getwd()
setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")

ctes <- read.csv("AM_Records-Dataset02_EESamuel.csv")
ctes

head(ctes)
summary(ctes)

colnames(ctes)

ctes$species
ctes[,"species"]

unique(ctes$species)

class(ctes)
length(ctes)
head(ctes,500)
head(ctes,0)

colnames(ctes)[3]
colnames(ctes)[2:3]

unique(ctes$station)

unique(ctes$lat)

summary("hour")

class(ctes$lat)



agouti <- "cutia"
agouti
class(agouti)


mammals <- c(

rm(list=ls())
getwd()
setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")

ctes <- read.csv("AM_Records-Dataset02_EESamuel (1).csv")
ctes 
table(ctes$species)
#Renaming from Portuguese to English#
ctes[ctes$species=="cutia","species"]<-"agouti"
ctes[ctes$species=="ariranha","species"]<-"giant_otter"
ctes[ctes$species=="atelocynus","species"]<-"short_eared_dog"
ctes[ctes$species=="bandeira","species"]<-"giant_anteater"
ctes[ctes$species=="canastra","species"]<-"giant_armadillo"
ctes[ctes$species=="cateto","species"]<-"collared_pecary"
ctes[ctes$species=="irara","species"]<-"tayra"
ctes[ctes$species=="jaguatirica","species"]<-"ocelot"
ctes[ctes$species=="marsupial","species"]<-"marsupial"
ctes[ctes$species=="mateiro","species"]<-"red_brocket_deer"
ctes[ctes$species=="mirim","species"]<-"tamandua"
ctes[ctes$species=="mourisco","species"]<-"jaguarundi"
ctes[ctes$species=="macura","species"]<-"opossum"
ctes[ctes$species=="paca","species"]<-"paca"
ctes[ctes$species=="parda","species"]<-"puma"
ctes[ctes$species=="pintada","species"]<-"jaguar"
ctes[ctes$species=="quati","species"]<-"coati"
ctes[ctes$species=="queixada","species"]<-"white_lipped_pecary"
ctes[ctes$species=="roedor","species"]<-"rodent"
ctes[ctes$species=="roxo","species"]<-"brown_brocket_deer"
ctes[ctes$species=="tatu","species"]<-"armadillo"
ctes[ctes$species=="anta","species"]<-"tapir"

#Subsetting, assigning species to name#
agouti <- ctes[ctes$species=="agouti",]
tapir <- ctes[ctes$species=="tapir",]
giant_otter <- ctes[ctes$species=="giant_otter",]
short_eared_dog <- ctes[ctes$species=="short_eared_dog",]
giant_anteater <- ctes[ctes$species=="giant_anteater",]
giant_armadillo <- ctes[ctes$species=="giant_armadillo",]
collared_pecary <- ctes[ctes$species=="collared_pecary",]
tayra <- ctes[ctes$species=="tayra",]
ocelot <- ctes[ctes$species=="ocelot",]
marsupial <- ctes[ctes$species=="marsupial",]
red_brocket_deer <- ctes[ctes$species=="red_brocket_deer",]
tamandua <- ctes[ctes$species=="tamandua",]
jaguarundi <- ctes[ctes$species=="jaguarundi",]
opossum <- ctes[ctes$species=="opussum",]
paca <- ctes[ctes$species=="paca",]
puma <- ctes[ctes$species=="puma",]
jaguar <- ctes[ctes$species=="jaguar",]
coati <- ctes[ctes$species=="coati",]
white_lipped_pecary <- ctes[ctes$species=="white_lipped_pecary",]
rodent <- ctes[ctes$species=="rodent",]
brown_brocket_deer <- ctes[ctes$species=="brown_brocket_deer",]
armadillo <- ctes[ctes$species=="armadillo",]

#Grouping only the mammals, making the table only mammals#
MAM <- rbind(agouti,tapir,giant_otter, short_eared_dog, giant_anteater, giant_armadillo,collared_pecary,tayra,ocelot, marsupial,red_brocket_deer,tamandua, jaguarundi,opossum,paca,puma,jaguar,coati,white_lipped_pecary,rodent,brown_brocket_deer,armadillo)
MAM
class(MAM)
summary(MAM)

head(MAM,100)


jaguar
summary(agouti)
summary(jaguar)
summary(tapir)
summary(armadillo)
summary(ocelot)
summary(brown_brocket deer)
summary(brown_brocket_deer)
summary(puma)
summary(opossum)
summary(tamandua)
summary(rodent)
summary(paca)
summary(coati)
summary(jaguarundi)
summary(tayra)
summary(marsupial)
summary(opossum)
table(MAM$species) #total number of each species captured#

table(MAM$station)
table(ctes$station)
table(MAM$station)
length(table(MAM$station))

#station CTES15 is missing from the mammals only#
--------------------------------------------------------------
######USE THIS, CORRECT######
rm(list=ls())
getwd()
setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")

ctes <- read.csv("AM_Records-Dataset02_EESamuel.csv")
ctes
ctes[ctes$species=="cutia","species"]<-"agouti"
ctes[ctes$species=="ariranha","species"]<-"giant_otter"
ctes[ctes$species=="atelocynus","species"]<-"short_eared_dog"
ctes[ctes$species=="bandeira","species"]<-"giant_anteater"
ctes[ctes$species=="canastra","species"]<-"giant_armadillo"
ctes[ctes$species=="cateto","species"]<-"collared_pecary"
ctes[ctes$species=="irara","species"]<-"tayra"
ctes[ctes$species=="jaguatirica","species"]<-"ocelot"
ctes[ctes$species=="marsupial","species"]<-"marsupial"
ctes[ctes$species=="mateiro","species"]<-"red_brocket_deer"
ctes[ctes$species=="mirim","species"]<-"tamandua"
ctes[ctes$species=="mourisco","species"]<-"jaguarundi"
ctes[ctes$species=="mucura","species"]<-"opossum"
ctes[ctes$species=="paca","species"]<-"paca"
ctes[ctes$species=="parda","species"]<-"puma"
ctes[ctes$species=="pintada","species"]<-"jaguar"
ctes[ctes$species=="quati","species"]<-"coati"
ctes[ctes$species=="queixada","species"]<-"white_lipped_pecary"
ctes[ctes$species=="roedor","species"]<-"rodent"
ctes[ctes$species=="roxo","species"]<-"brown_brocket_deer"
ctes[ctes$species=="tatu","species"]<-"armadillo"
ctes[ctes$species=="anta","species"]<-"tapir"

#Subsetting, assigning species to name#
agouti <- ctes[ctes$species=="agouti",]
tapir <- ctes[ctes$species=="tapir",]
giant_otter <- ctes[ctes$species=="giant_otter",]
short_eared_dog <- ctes[ctes$species=="short_eared_dog",]
giant_anteater <- ctes[ctes$species=="giant_anteater",]
giant_armadillo <- ctes[ctes$species=="giant_armadillo",]
collared_pecary <- ctes[ctes$species=="collared_pecary",]
tayra <- ctes[ctes$species=="tayra",]
ocelot <- ctes[ctes$species=="ocelot",]
marsupial <- ctes[ctes$species=="marsupial",]
red_brocket_deer <- ctes[ctes$species=="red_brocket_deer",]
tamandua <- ctes[ctes$species=="tamandua",]
jaguarundi <- ctes[ctes$species=="jaguarundi",]
opossum <- ctes[ctes$species=="opossum",]
paca <- ctes[ctes$species=="paca",]
puma <- ctes[ctes$species=="puma",]
jaguar <- ctes[ctes$species=="jaguar",]
coati <- ctes[ctes$species=="coati",]
white_lipped_pecary <- ctes[ctes$species=="white_lipped_pecary",]
rodent <- ctes[ctes$species=="rodent",]
brown_brocket_deer <- ctes[ctes$species=="brown_brocket_deer",]
armadillo <- ctes[ctes$species=="armadillo",]

MAM <- rbind(agouti,tapir,giant_otter, short_eared_dog, giant_anteater, giant_armadillo,collared_pecary,tayra,ocelot, marsupial,red_brocket_deer,tamandua, jaguarundi,opossum,paca,puma,jaguar,coati,white_lipped_pecary,rodent,brown_brocket_deer,armadillo)
MAM

table(MAM$species)
summary(opossum)

#Cameras that caught mammals#
table(MAM$station)
#Cameras that caught an animal not necessarily mammals#
table(ctes$station)

library("maptools")

plot(MAM$long,MAM$lat,
	xlab="Longitude",
	ylab="Latitude")
library(raster)
library



coord <- SpatialPoints(coords = MAM[,c("long","lat")],
         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#----PUTTING MAP AND PLOTS TOGETHER# #CONSIDER CHANGIING "COVER" TO ELEV"-----------#	
cover <-raster("s09_w064_1arc_v3.tif")
cover		
plot(cover,
	xlab="Longitude",ylab="Latitude",main="CT Location and  Elevation",add=T)	

plot(coord, pch=18, col= "blue", add=T)
plot(coord)


library(sp)

ext <-extent(coord)+ c(-0.02, +0.02, -0.02, +0.02)
cover2<- crop(cover, ext)
plot(cover2,xlab="Longitude",ylab="Latitude",main="CT Location and  Elevation")
plot(coord, pch=18, col= "blue", add=T)

#EXTRACTING ELEV FROM THE CTS#

elev <- extract(cover2,coord)
elev
elev.d <- cbind(MAM$station,MAM$lat,MAM$long,elev)
unique(elev.d)
elev.d2<-as.data.frame(unique(elev.d))
colnames(elev.d2) <- c("station","latitude","longitude","elevation")
#---------------------------------------------------------------------
library(unmarked)
stat <-read.csv("AM_StationsOn-Dataset02_EESamuel.csv")
stat
head(stat)

par(mfrow=c(1,2))
plot(MAM$long,MAM$lat,
	xlab="Longitude",
	ylab="Latitude",
	col="blue",pch=10,
	main="CT Location and Elevation")
plot(stat$X,stat$Y,
	xlab="Longitude",
	ylab="Latitude",
	col="blue",pch=10,
	main="CT Location and Elevation")
head(stat)
dayworked <- rowSums(stat[,-c(1:6)])
mean(dayworked)
range(dayworked)	
sort(table(MAM$species))

nrow(stat)
ncol(stat)
st.loc<-stat[,c(2,3,4)]
st.loc
rowSums(stat[,c(7:64)])
sum(rowSums(stat[,c(7:64)]))
range(rowSums(stat[,c(7:64)]))
mean(rowSums(stat[,c(7:64)]))

MAM$date #use to start messing with the dates#
mndate <- as.Date(MAM$date, "%m/%d/%Y")

unique(mndate)
sort(mndate)
MAM
mndate
head(mndate)#use to check if the data is outside of the correct time#
tail(mndate)
MAM[201,"date"]<-"1/24/2019" #typo, changed the date from 10 to 19#


mncnt <- as.Date(MAM$date, "%m/%d/%Y")

head(sort(mncnt))
tail(sort(mncnt))

#--------------------------------------------------------------------------------
#Upload Capture History Maker function#
rm(list=ls())#DO NOT USE IT#
getwd()
setwd("

source("Comm_capt_hist_maker_ForEES.txt")

ch_EES<- comm_hist_maker(records= MAM, stations_on = stat, n.collap.days= 7)
str(ch_EES)
ch_EES[[2]]
#-----------------------------------------------------------------
tab.1<-table(MAM$station, MAM$species)
tab.1[tab.1>1]<-1
colSums(tab.1)


table(MAM$species)
table(MAM$station)
write.csv(tab.1, "tab.1_st-sp_EES.csv")
getwd()

#-----------CSVs for Upload and Extraction---------------------------------#
eff <- ch_EES[[3]]
write.csv(eff, "eff_EES_7days.csv")

puma<- ch_EES[[2]][,,"puma"]
puma
write.csv(puma, "puma_EES_7days.csv") #NOT USING PUMA, USE OCELOT INSTEAD#

ocelot <- ch_EES[[2]][,,"ocelot"]
ocelot
write.csv(ocelot, "ocelot_EES_7days.csv")

paca<- ch_EES[[2]][,,"paca"]
write.csv(paca, "paca_EES_7days.csv")

collared_pecary <- ch_EES[[2]][,,"collared_pecary"]
write.csv(collared_pecary, "collared_pecary_EES_7days.csv")

write.csv(st.loc, "st.loc_EES_7days.csv")

#-------Tables Showing Camera Trap Sites----------------------------------
#
order <- data.frame(stat$Estacao, stat$Y, stat$X)
order
dist <- read.csv("Distances_EES_REAL.csv")
head(dist)
frag <- read.csv("FragMetrics_ees.csv")
head(frag)
#--------------------------------------------------------------------------
st.loc

library("maptools")
library(rgdal)
library(sp)
library(raster)
dis <-raster("HFI_EES_EPSG4326.tif")
dis
watr<-raster("WaterSurf_EES_EPSG4326.tif")
watr

coord <- SpatialPoints(coords = st.loc[,c(3,2)],
         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
coord
plot(coord)

#Rasters for Individual Species#
hfp <- raster("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET\\HFI_EES_EPSG4326.tif")
eLEV <- raster("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET\\s09_w064_1arc_v3.tif")
wat <- raster("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET\\WaterSurf_EES_EPSG4326.tif")
fore <- raster("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET\\PrimForest_EES_EPSG4326.tif")
npp <- raster("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET\\NPP_EES_EPSG4326.tif")
road <- dist$d.Road
river <- dist$d.River
edge <- frag$EdgeDens
patch <- frag$PatchDens
core <- frag$DisjCore
ct_hfp <- extract(hfp, coord,buffer=500,fun=mean)
ct_eLEV <- extract(eLEV, coord,buffer=500,fun=mean)
ct_wat <- extract(wat, coord,buffer=500,fun=mean)
ct_fore <- extract(fore, coord, buffer=500, fun=mean)
ct_npp <- extract(npp, coord, buffer=500, fun=mean)

siteCovs<- cbind(st.loc, ct_hfp, ct_eLEV, ct_wat, ct_fore, ct_npp, road, river, edge, patch, core)
par(mfrow=c(2,3))
hist(siteCovs$ct_npp, xlab="Net Primary Productivity", main="")
hist(siteCovs$ct_eLEV, xlab= "Elevation", main="")
hist(siteCovs$ct_hfp,xlab="Human Foort Print", main="")
hist(siteCovs$ct_fore, xlab= "Forest", main="")
hist(siteCovs$road, xlab="Dist to Road", main="")
hist(siteCovs$river, xlab="Dist to River", main="")



write.csv(siteCovs, "siteCovs.csv")
write.csv(road, "Distances_EES.csv")#distance was not raster#

write.csv(MAM, "RecordsTable_EES.csv")
write.csv(stat, "EES_StationsOn_Forest.csv")








acoord <- buffer(coord, width = 5, dissolve = F)
plot(acoord)
plot(hfp)
plot(,add=T)


plot(cr,add=T)



























#----------------------------------------------------------------------------#maybe don't pay attention to this
stations_on$Station<-toupper(stations_on$Station)
records$station<-toupper(records$station)

xx<-colnames(stations_on)[min(grep("20", colnames(stations_on) ))]
first<-as.Date(substr(xx, 2, nchar(xx)), "%m.%d.%Y")
xy<-colnames(stations_on)[max(grep("20", colnames(stations_on) ))]
last<-as.Date(substr(xy, 2, nchar(xy)), "%m.%d.%Y")

stations_on.bin<-stations_on[,min(grep("20", colnames(stations_on) )):max(grep("20", colnames(stations_on) ))]
ndays<-as.numeric(last-first)+1  #number of days
if(ndays != dim(stations_on.bin)[2]) stop("Something went wrong selection columns")

K<- ceiling(ndays/n.collap.days) #number of occasions

beg<-seq(first, last, by=n.collap.days)
en<-seq(first+(n.collap.days-1), last, by=n.collap.days)
if(length(en)<K) {en<-c(en, last)}

eff.coll<-matrix(0, J, K)

occ.l<-(en-beg)+1
  occ<-NULL
  for (k in 1:K){
    occ<-c(occ,rep(k,occ.l[k] ))

for (j in 1:J){
    for (k in 1:K){
      eff.coll[j,k]<-sum(stations_on.bin[j, which(occ==k)])








  








