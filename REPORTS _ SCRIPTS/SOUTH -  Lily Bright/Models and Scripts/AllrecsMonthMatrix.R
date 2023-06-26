rm(list=ls())

setwd("/Users/lilybright/Documents/PLNU/Summer Research 2022/Offic Data")
dir()

dat<- read.csv("NewCom_CR_Dataset01_South.csv")
head(dat)
dim(dat)

#############################################################################
### get independent records table ###########################################

head(dat)

dat$Date
table(dat$Species)
colnames(dat)

#extract only necessary columns, remove records with NA as date
recs<-dat[!is.na(dat$Date),c("Camr_Nm", "Species", "Date", "Time")]
dim(recs)

stations<-unique(recs$Camr_Nm)
nas<- which(is.na(recs$Camr_Nm))
recs[nas,"Camr_Nm"]<- "Lily"
stations<-unique(recs$Camr_Nm)

J<- length(stations)
species<-sort(unique(recs$Species))

recs.all<-recs[1,] #set up structure of independent record table

for (i in 1:length(species)){
  for (j in 1:J){
    sub<- recs[recs$Camr_Nm==stations[j] & recs$Species == species[i],]
    sub<- na.omit(sub)
    if(dim(sub)[1]==0) next
    
    ordsub<- order(strptime( paste(sub$Date, sub$Time), "%m/%d/%Y %H:%M:%S"))            
    sub2<- sub[ordsub,]
    
    dd<-unique(sub2$Date)
    
    dt<-strptime( paste(sub2$Date, sub2$Time), "%m/%d/%Y %H:%M:%S")
    nr<-length(dt)
    
    recs.all<-rbind(recs.all, sub2[1,]) #always add first record
    
    if(dim(sub2)[1]>1) { #if more than one, check for independence
      
      for (k in 2:nr){
        dif<-difftime(dt[k], dt[k-1], units="mins")
        if(dif>(24*60)) {recs.all<-rbind(recs.all,sub2[k,] )}
      }
    }
  }
}
#remove starter row
recs.all<-recs.all[-1,]
recs.all
dim(recs.all)
head(recs.all)
table(recs.all$Species)
write.csv(recs.all, "AllSourthRecs.csv")
####################################################################################
### Creat table with the number of independent records per camera per month/year ####

# Subseting dataset per ct 

stations_recs<- list()
for(c in 1:length(stations)){
  stations_recs[[c]]<-  recs.all[ recs.all$Camr_Nm== stations[c],] 
  names(stations_recs)[c]<- stations[c]
}
str(stations_recs) 
length(stations_recs)
stations_recs[[2]]

m.y<- list()
for(ct in 1:length(stations)){
  date1<-  as.Date(stations_recs[[ct]][,"Date"], "%m/%d/%Y", na.rm=T)#
  y<- data.frame(Year = as.numeric(format(date1, "%Y")),
                 Month= as.numeric(format(date1, "%m")))
  m.y[[ct]]<- table(y[,"Month"],y[,"Year"])
}
m.y
recs[,"Date"]
range(as.Date(recs[,"Date"], "%m/%d/%y", na.rm=T))

t2017<- matrix(NA, length(stations), 12)
rownames(t2017)<- stations
colnames(t2017)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2017",12))

t2018<- matrix(NA, length(stations), 12)
rownames(t2018)<- stations
colnames(t2018)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2018",12))

t2019<- matrix(NA, length(stations), 12)
rownames(t2019)<- stations
colnames(t2019)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2019",12))

t2020<- matrix(NA, length(stations), 12)
rownames(t2020)<- stations
colnames(t2020)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2020",12))

t2021<- matrix(NA, length(stations), 12)
rownames(t2021)<- stations
colnames(t2021)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2021",12))

for(ct in 1:length(stations)){
  for(c in 1:ncol(m.y[[ct]])){
    if(colnames(m.y[[ct]])[c]=="17"){t2017[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="18"){t2018[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="19"){t2019[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="20"){t2020[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="21"){t2021[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
  }
}


d2.CT.on<- cbind(t2017,t2018,t2019,t2020,t2021)
d2.CT.on[d2.CT.on==0]<- NA
d2.CT.on
write.csv(d2.CT.on, "CR_d2.CT.IndRecs.csv")


