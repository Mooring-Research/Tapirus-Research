###=================================================================================###
### Script to clean CR_Dataset03_North and produce Community Detection Histories    ###
### Summer Research Program - 2022									###
### 06 Jun 2022												###
### Daniel Rocha (Sarah)										###
###=================================================================================###

rm(list=ls())

setwd("C:/Work/Point_Loma/Costa Rica Research/Summer 2022/Working with Dataset/My Dataset")
dir()

dat<- read.csv("CR_Dataset03_North.csv")
head(dat)
dim(dat)

#############################################################################
### get independent records table ###########################################
  
head(dat)

dat[which(is.na(dat$Camr_Nm)),"Camr_Nm"]<- "Sarah"

A<- table(dat$Srvy_Nm, dat$Camr_Nm)
A
A[A>0]<-1
as.data.frame(colSums(A))

stat<- sort(unique(dat$Camr_Nm))

z<- data.frame()
y<- vector()
for(ct in 1:length(stat)){
	x<-dat[dat$Camr_Nm == stat[ct], c("Latitud", "Longitd")]
	for(r in 1:nrow(x)){
		y[r]<- paste(x[r,"Latitud"],x[r,"Longitd"])
	}
	z[ct,1]<- stat[ct]
	z[ct,2]<- length(unique(y))
}

ls.coord[[1]]
cbind(as.data.frame(colSums(A)), z)  #Some cameras have the same name in different Surveys and multiple coordinates.

dat$Date
table(dat$Species)
colnames(dat)
dim(dat)

unique(paste(dat$Srvy_Nm, dat$Camr_Nm))
unique(paste(dat$Site, dat$Camr_Nm))

dat1<- cbind(dat,paste(dat$Site, dat$Camr_Nm))
head(dat1)
colnames(dat1)[6]<- "Camr_Nm1"
colnames(dat1)[19]<- "Camr_Nm"
unique(dat1$Camr_Nm)


#extract only necessary columns, remove records with NA as date
recs<-dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time")]
recs_loc <- dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time", "Latitud", "Longitd")]
dim(recs)
  
  stations<-unique(recs$Camr_Nm)
  #nas<- which(is.na(recs$Camr_Nm))
  #recs[nas,"Camr_Nm"]<- "Emily"
  #stations<-unique(recs$Camr_Nm)

  J<- length(stations)
  species<-sort(unique(recs$Species))
    
  recs.all<-recs[1,] #set up structure of independent record table
  
  for (i in 1:length(species)){
     for (j in 1:J){
      sub<- recs[recs$Camr_Nm==stations[j] & recs$Species == species[i],]
	sub<- na.omit(sub)
	if(dim(sub)[1]==0) next
	
	#OlsonNames(tzdir = NULL)
	ordsub<-order(as.POSIXlt(paste(sub$Date, sub$Time), tz = "America/Costa_Rica", format= "%m/%d/%Y %H:%M:%S"))
      sub2<- sub[ordsub,]

      dd<-unique(sub2$Date)
        
      dt<- as.POSIXlt(paste(sub2$Date, sub2$Time), tz = "America/Costa_Rica", format= "%m/%d/%Y %H:%M:%S")
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



########################################################################################
### I SKIPED THE PART OF THE CODE THAT CREATES THE "NEW GREEN TABLE"
########################################################################################

#################################################################################################

tw<- read.csv("CR_d2.CT.IndRecs4 SARAH.csv")
tw<- tw[,1:5]
tw
sort(tw$Cameraname)==sort(unique(recs.all$Camr_Nm))
head(recs.all)

colnames(recs.all)

recs_tw<- data.frame()
for(ct in 1:nrow(tw)){
	x<- recs.all[recs.all$Camr_Nm==tw$Cameraname[ct],]
	date1<-  as.Date(x[,"Date"], "%m/%d/%Y", na.rm=T)
	for(r in 1:nrow(x)){
		if(tw[ct,"StartYear"] == tw[ct,"EndYear"]){
			if(as.numeric(format(date1[r], "%Y"))== tw[ct,"StartYear"] &
				as.numeric(format(date1[r], "%m"))>= tw[ct,"StartMonth"] &
				as.numeric(format(date1[r], "%m"))<= tw[ct,"EndMonth"])
				{recs_tw<- rbind(recs_tw, x[r,])}}
		if(tw[ct,"StartYear"] != tw[ct,"EndYear"]){
			if(as.numeric(format(date1[r], "%Y"))== tw[ct,"EndYear"] &
				as.numeric(format(date1[r], "%m"))<= tw[ct,"EndMonth"] |
				as.numeric(format(date1[r], "%Y"))== tw[ct,"EndYear"]-1 &
				as.numeric(format(date1[r], "%m"))>= tw[ct,"StartMonth"])
				{recs_tw<- rbind(recs_tw, x[r,])}}
	}
}
dim(recs_tw)
sort(tw$Cameraname)==sort(unique(recs_tw$Camr_Nm))
setdiff(sort(tw$Cameraname),sort(recs_tw$Camr_Nm))
recs.all[recs.all$Camr_Nm== "Tapanti T1-2",]
recs_tw[recs_tw$Camr_Nm== "Tapanti T1-2",]

active<- data.frame(CTname=NA, FirstDay=NA, LastDay=NA)
for(ct in 1:nrow(tw)){
	x<- recs_tw[recs_tw$Camr_Nm==tw$Cameraname[ct],]
	date1<-  as.Date(x[,"Date"], "%m/%d/%Y", na.rm=T)
	active[ct,"CTname"]<- tw$Cameraname[ct]
	active[ct,c("FirstDay", "LastDay")]<- paste(range(date1))
}
active

cbind(active, tw)

ext.date<- range(na.omit(as.Date(c(active[,"FirstDay"], active[,"LastDay"]), "%Y-%m-%d")))
out<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))

ston<- data.frame(matrix(0, nrow = nrow(active), ncol = length(out)))
colnames(ston)<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))
rownames(ston)<- active$CTname

for(ct in 1:nrow(active)){
	if(active[ct,"FirstDay"]== "NA" | active[ct,"LastDay"]== "NA") next
	x<- as.character(seq(as.Date(active[ct,"FirstDay"]),as.Date(active[ct,"LastDay"]), by="days"))
	for(d1 in 1: length(x)){
		for(d2 in 1:length(out)){	
			if(x[d1]==out[d2]){ston[ct, d2]<-1}  
		}
	}
}
head(ston)
str(ston)
dim(ston)
rowSums(ston)
range(rowSums(ston))

colnames(recs.all)<- c( "station", "species", "date", "hour")
head(recs.all)
#write.csv(recs.all, "recs.all_CR_Dataset03_North_07Jun2022.csv", row.names=F)
#write.csv(ston, "ston_CR_Dataset03_North_07Jun2022.csv", row.names=T)

### RECORDS TABLE AND CAMERA TRAP LAYOUT TABLE ARE READY



############################################################################################
### Creating Capture Histories using comm_hist_maker

recs1<- read.csv("recs.all_CR_Dataset03_North_07Jun2022.csv")
head(recs1)
dim(recs1)
ston<- read.csv("ston_CR_Dataset03_North_07Jun2022.csv")
head(ston)
ston<- ston[,-1]
rowSums(ston[,-1])

unique(recs1$station)
recs<- recs1[1,]
for(ct in 1: nrow(ston)){
	for(r in 1:nrow(recs1)){
		if(recs1[r,"station"] == tw$Cameraname[ct]){recs[r,]<- recs1[r,]} else{}
	}
}
recs<- recs[-1,]
nas<- which(is.na(recs$station))
recs<-recs[-nas,]
dim(recs)
dim(recs1)
sort(unique(recs$station)) == sort(unique(ston$Estacao))


records= recs; stations_on = ston; n.collap.days= 7


###=========================================================================================================###
###        	Function to create independent records table, collapsed capture histories and 
###          collapsed record histories
###=========================================================================================================###

comm_hist_maker<-function(records, stations_on, n.collap.days){
  
  ## make sure, station names are always in upper case in both files
  stations_on$Estacao<-toupper(stations_on$Estacao)
  records$station<-toupper(records$station)
  
  ##get first and last date in camera functioning matrix (dates start with "X")
  xx<-colnames(stations_on)[min(grep("20", colnames(stations_on) ))]
  first<-as.Date(substr(xx, 2, nchar(xx)), "%Y.%m.%d")
  xy<-colnames(stations_on)[max(grep("20", colnames(stations_on) ))]
  last<-as.Date(substr(xy, 2, nchar(xy)), "%Y.%m.%d")
  
  ##extract only 0/1 info from station_on
  stations_on.bin<-stations_on[,min(grep("20", colnames(stations_on) )):max(grep("20", colnames(stations_on) ))]
  
  ##based on days to collapse, get start and end date of each occasion
  ndays<-as.numeric(last-first)+1  #number of days
  if(ndays != dim(stations_on.bin)[2]) stop("Something went wrong selection columns")
                                          
  K<- ceiling(ndays/n.collap.days) #number of occasions
  
  beg<-seq(first, last, by=n.collap.days)
  en<-seq(first+(n.collap.days-1), last, by=n.collap.days)
  if(length(en)<K) {en<-c(en, last)} #add end day for last occasion if it's incomplete
  
  
  J<-dim(stations_on)[1] #number of sites
  
  ##calculate collapsed effort
  eff.coll<-matrix(0, J, K)
  
  #create occasion index
  occ.l<-(en-beg)+1
  occ<-NULL
  for (k in 1:K){
    occ<-c(occ,rep(k,occ.l[k] ))
  }

  for (j in 1:J){
    for (k in 1:K){
      eff.coll[j,k]<-sum(stations_on.bin[j, which(occ==k)])
    }
  }
  
  
### get independent records table #####################################################
  
  #extract only necessary columns, remove records with NA as date
  recs<-records[!is.na(records$date),c("station", "species", "date", "hour")]
  if (sum(is.na(pmatch(unique(recs$station), stations_on$Estacao, duplicates.ok = TRUE))) >0) stop("station mismatch")
  
  stations<-stations_on$Estacao
  species<-sort(unique(recs$species))
    
  recs.all<-recs[1,] #set up structure of independent record table
  
  for (i in 1:length(species)){
    
    for (j in 1:J){
      sub2<-recs[recs$station==stations[j] & recs$species == species[i] ,]
      if(dim(sub2)[1]==0) next
      
      dd<-unique(sub2$date)
        
      #dt<-strptime( paste(sub2$date, sub2$hour), "%m/%d/%Y %H:%M:%S")
      dt<- as.POSIXlt(paste(sub2$date, sub2$hour), tz = "America/Costa_Rica", format= "%m/%d/%Y %H:%M:%S")
      
		
	nr<-length(dt)
      
      recs.all<-rbind(recs.all, sub2[1,]) #always add first record
 
      
      if(dim(sub2)[1]>1) { #if more than one, check for independence
        
        for (k in 2:nr){
          dif<-difftime(dt[k], dt[k-1], units="mins")
          if(dif>60) {recs.all<-rbind(recs.all,sub2[k,] )}
        }
      }
    }
  }
  
  #remove starter row
  recs.all<-recs.all[-1,]
  
  
#### make collapsed capture history ###################################################
  
  record.mat<-array(0, c(J, K,length(species)))
  
  for (k in 1:K){
    
    dl<-seq.Date(beg[k], en[k], by=1)
    
    for (i in 1:length(species)){
      for (j in 1:J){
        subb<-recs.all[recs.all$station == stations[j] &
                         recs.all$species==species[i] &
                         as.Date(recs.all$date, "%d/%m/%Y") %in% dl,]
        if(dim(subb)[1]==0) next
        record.mat[j,k,i]<-dim(subb)[1]
      }
    }
  }
  
  ## set to NA where effort == 0
  record.mat[eff.coll==0]<-NA
  
  ## turn number of records into binary
  record.mat2<-record.mat
  record.mat2[record.mat>0]<-1
  
  dimnames(record.mat2)[[3]]<-dimnames(record.mat)[[3]]<-species
  dimnames(record.mat2)[[1]]<-dimnames(record.mat)[[1]]<-stations
  
  f.visit<-apply(eff.coll, 1, function(x){min(which(x>0))})
  l.visit<-apply(eff.coll, 1, function(x){max(which(x>0))})
  k2<- ceiling(max(rowSums(eff.coll))/n.collap.days)+1
  crop_rec.mat<- array(NA,dim = c(length(stations), k2, length(species)))
  crop_rec.mat2<- array(NA,dim = c(length(stations), k2, length(species))) 
  crop_eff.coll<- matrix(NA, length(stations), k2)
  for (j in 1:length(stations)){ 
	r<- (l.visit[j]-f.visit[j])+1
	crop_rec.mat[j,1:r,]<- record.mat[j,f.visit[j]:l.visit[j],]
	crop_rec.mat2[j,1:r,]<- record.mat2[j,f.visit[j]:l.visit[j],]
	crop_eff.coll[j,1:r]<- eff.coll[j,f.visit[j]:l.visit[j]]  
  }

dimnames(crop_rec.mat)[[3]]<-dimnames(crop_rec.mat2)[[3]]<-species
dimnames(crop_rec.mat)[[1]]<-dimnames(crop_rec.mat2)[[1]]<- rownames(crop_eff.coll)<-stations


  return(list("Collapsed record history"=crop_rec.mat, 
             "Collapsed capture history"=crop_rec.mat2, 
             "Occasion effort"=crop_eff.coll,
             "Independent detections"=recs.all))
}
#---------------------------------------------------------------------------------------------###


###----------------------------------------------------------------------------------###
### Running basic occupancy models									 ###
###----------------------------------------------------------------------------------###

ch_cr_north<- comm_hist_maker(records= recs, stations_on = ston, n.collap.days= 7)
str(ch_cr_north)

independent <- nrow(ch_cr_north[[4]])
write.csv(independent, "Collapsed_Records.csv")

puma <- ch_cr_north[[2]][,,"Puma concolor"]
eff<-ch_cr_north[[3]]

library(unmarked)
umf_north<- unmarkedFrameOccu(y=puma, siteCovs=, obsCovs=list(Eff=eff))
summary(umf_north)

# Running Null model

mod0 <- occu(~1~1, umf_north)  # Null Model
summary(mod0)
plogis(11.3)  	# Probability of occupancy
plogis( -4.56)	# Probability of detection

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf_north)  # Null Model
summary(m.psi1.pEff)

############Lat Long Table###############################################################

#use recs as a reference
locations <- recs_loc[,c("Camr_Nm","Latitud","Longitd")]
unique(locations$Camr_Nm)

#new code from Dr. Rocha
stat<- sort(unique(dat$Camr_Nm))
stat
stat[2]

z<- data.frame()
y<- vector()
for(ct in 1:length(stat)){
  x<-dat[dat$Camr_Nm == stat[ct], c("Latitud", "Longitd")]
  for(r in 1:nrow(x)){
    y[r]<- paste(x[r,"Latitud"],x[r,"Longitd"])
  }
  z[ct,1]<- stat[ct]
  z[ct,2]<- length(unique(y))
}


#ls.coord[[1]]
cbind(as.data.frame(colSums(A)), z)  #Some cameras have the same name in different Surveys and multiple coordinates.
