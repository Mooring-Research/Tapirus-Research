rm(list=ls())

setwd("C:/Work/Point_Loma/Costa Rica Research/Summer 2022/2. Working with Dataset/Raw Data")
dir()

dat<- read.csv("CR_Dataset03_North - CR_Dataset03_North.csv")
head(dat)
dim(dat)

#############################################################################
### get independent records table ###########################################

head(dat)

dat$Date
table(dat$Species)
colnames(dat)

nas<- which(is.na(dat$Camr_Nm))
dat[nas,"Camr_Nm"]<- "Sarah"

dat1<- cbind(dat, ctid=paste(dat$Site,dat$Camr_Nm))
head(dat1)

#extract only necessary columns, remove records with NA as date
recs<-dat1[!is.na(dat1$Date),c("ctid", "Species", "Date", "Time")]
dim(recs)
head(recs)

#resolving errors in date selection
out<- which(recs$Date=="3/10/2013" & recs$Time=="2:12:56")
out2<- which(recs$Date=="3/10/2013" & recs$Time=="2:10:38")
out3 <- which(recs$Date=="3/8/2015"& recs$Time=="2:08:21")
out4 <- which(recs$Date=="3/8/2015"& recs$Time=="2:33:33")

recs<- recs[-c(out,out2,out3,out4),]

stations<-unique(recs$ctid)
stations
J<- length(stations)
species<-sort(unique(recs$Species))


recs.all<-recs[1,] #set up structure of independent record table

for (i in 1:length(species)){
  for (j in 1:J){
    sub<- recs[recs$ctid==stations[j] & recs$Species == species[i],]
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

tb_camp11 <- recs[recs$ctid=="Savegre Valley Camp11",]
tb_t26<-recs[recs$ctid==stations[108],]
recs.all <- rbind(recs.all,tb_camp11[1,],tb_t26[c(6,7,8),])


#####################################################################################
### Create table with the number of independent records per camera per month/year ####

# Subseting dataset per ct

stations_recs<- list()
for(c in 1:length(stations)){
  stations_recs[[c]]<-  recs.all[ recs.all$ctid== stations[c],]
  names(stations_recs)[c]<- stations[c]
}
str(stations_recs)
length(stations_recs)
stations_recs[[2]]
length(stations)

m.y<- list()
for(ct in 1:length(stations)){
  date1<-  as.Date(stations_recs[[ct]][,"Date"], "%m/%d/%Y", na.rm=T)#
  y<- data.frame(Year = as.numeric(format(date1, "%Y")),
                 Month= as.numeric(format(date1, "%m")))
  m.y[[ct]]<- table(y[,"Month"],y[,"Year"])
}
m.y

range(as.Date(recs[,"Date"], "%m/%d/%Y", na.rm=T))

t2010<- matrix(NA, length(stations), 12)
rownames(t2010)<- stations
colnames(t2010)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2010",12))

t2011<- matrix(NA, length(stations), 12)
rownames(t2011)<- stations
colnames(t2011)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2011",12))

t2012<- matrix(NA, length(stations), 12)
rownames(t2012)<- stations
colnames(t2012)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2012",12))

t2013<- matrix(NA, length(stations), 12)
rownames(t2013)<- stations
colnames(t2013)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2013",12))

t2014<- matrix(NA, length(stations), 12)
rownames(t2014)<- stations
colnames(t2014)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2014",12))

t2015<- matrix(NA, length(stations), 12)
rownames(t2015)<- stations
colnames(t2015)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2015",12))

t2016<- matrix(NA, length(stations), 12)
rownames(t2016)<- stations
colnames(t2016)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2016",12))

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

class(m.y)
ncol(m.y[[70]])

ncol(m.y[[116]])

for(ct in 1:length(stations)){
  for(c in 1:ncol(m.y[[ct]])){
    if(colnames(m.y[[ct]])[c]=="2010"){t2010[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2011"){t2011[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="2012"){t2012[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2013"){t2013[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2014"){t2014[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2015"){t2015[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="2016"){t2016[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2017"){t2017[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2018"){t2018[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2019"){t2019[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2020"){t2020[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
  }
}


#output csv
d2.CT.on<- cbind(t2010,t2011,t2012,t2013,t2014,t2015,t2016,t2017,t2018,t2019,t2020)
d2.CT.on[d2.CT.on==0]<- NA
d2.CT.on
#write.csv(d2.CT.on, "CR_d2.CT.IndRecs3.csv")

