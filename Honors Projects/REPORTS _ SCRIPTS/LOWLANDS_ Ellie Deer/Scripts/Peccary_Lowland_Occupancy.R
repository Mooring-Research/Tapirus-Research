#Occupancy Code to run Costa Rica Lowland Sites
#Written by Dr. Rocha
#Any added comments were made by Ellie Deer

setwd("~/Documents/PLNU Research/Honor's Project-Occupancy/Honor's Proj - Occupancy")
dir()

dat<-read.csv("CR_Dataset04_Lowland.csv")
head(dat)
dim(dat)

##################STEP 1####################
### get independent records table ###########################################

head(dat)

dat$Date
table(dat$Camr_Nm)
colnames(dat)

#extract only necessary columns, remove records with NA as date
recs<-dat[!is.na(dat$Date),c("Camr_Nm", "Species", "Date", "Time")]
dim(recs)

#remove NAs for start date, end date, date, x, and y
dat<- dat[which(!is.na(dat$Cmr_S_D)),]
dat<- dat[which(!is.na(dat$Cmr_E_D)),]
dat<- dat[which(!is.na(dat$Date)),]
dat<- dat[which(!is.na(dat$X)),]
dat<- dat[which(!is.na(dat$Y)),]

stations<-unique(recs$Camr_Nm)
nas<- which(is.na(recs$Camr_Nm))
recs[nas,"Camr_Nm"]<- "Emily"
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
table(recs.all$Camr_Nm)

write.csv(recs.all, "recs.all.csv")

#############################STEP 2############################
### Create table with the number of independent records per camera per month/year ####

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

range(as.Date(recs[,"Date"], "%m/%d/%Y", na.rm=T))

#t2010<- matrix(NA, length(stations), 12)
#rownames(t2010)<- stations
#colnames(t2010)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2010",12))

#t2011<- matrix(NA, length(stations), 12)
#rownames(t2011)<- stations
#colnames(t2011)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2011",12))

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

for(ct in 1:length(stations)){
  for(c in 1:ncol(m.y[[ct]])){
    #if(colnames(m.y[[ct]])[c]=="2010"){t2010[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    #if(colnames(m.y[[ct]])[c]=="2011"){t2011[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="2012"){t2012[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2013"){t2013[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2014"){t2014[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2015"){t2015[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="2016"){t2016[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2017"){t2017[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2018"){t2018[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2019"){t2019[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
  }
}

d2.CT.on<- cbind(t2012,t2013,t2014,t2015,t2016,t2017,t2018,t2019)
d2.CT.on[d2.CT.on==0]<- NA 
d2.CT.on
write.csv(d2.CT.on, "CR_lowland_idp_recss.csv")

##################################STEP3###################################

tw<- read.csv("lwl_indrecs_table.csv") #changed from lwl_indrecs_table.csv to CR_lowland_idp_recss.csv
tw

#had to reformat my years
tw <- within(tw, StartYear[StartYear == 18] <- 2018)
tw <- within(tw, StartYear[StartYear == 17] <- 2017)
tw <- within(tw, StartYear[StartYear == 16] <- 2016)
tw <- within(tw, EndYear[EndYear == 16] <- 2016)
tw <- within(tw, EndYear[EndYear == 17] <- 2017)
tw <- within(tw, EndYear[EndYear == 18] <- 2018)

recs.all <- subset(recs.all, recs.all$Camr_Nm != "Emily")

sort(tw$Cameraname)==sort(unique(recs.all$Camr_Nm))
head(recs.all)

colnames(recs.all)
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


active<- data.frame(CTname=NA, FirstDay=NA, LastDay=NA)
for(ct in 1:nrow(tw)){
  x<- recs_tw[recs_tw$Camr_Nm==tw$Cameraname[ct],]
  date1<-  as.Date(x[,"Date"], "%m/%d/%Y", na.rm=T)
  active[ct,"CTname"]<- tw$Cameraname[ct]
  active[ct,c("FirstDay", "LastDay")]<- paste(range(date1))
}
active

#active1<- active[-c(8, 19,22), ]

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
rowSums(ston)

##############################STEP 4######################################
### Creating Capture Histories using comm_hist_maker

#recs1<- read.csv("recs.all_CR_Dataset03_North_07Jun2022.csv")
recs1 <- recs.all
head(recs1)
dim(recs1)
#ston <- read.csv("ston_CR_Dataset03_North_07Jun2022.csv")
head(ston)
#ston<- ston[,-1]
rowSums(ston[,-1])

ston<-cbind(rownames(ston),ston)
colnames(ston)[1]<-  "Estacao"

unique(recs1$Camr_Nm) #changed station to Camr_Nm, #23 cameras total
recs<- recs1[1,]
for(ct in 1: nrow(ston)){
  for(r in 1:nrow(recs1)){
    if(recs1[r,"Camr_Nm"] == tw$Cameraname[ct]){recs[r,]<- recs1[r,]} else{} #changed "stations" to "Camr_Nm"
  }
}
recs<- recs[-1,]
nas<- which(is.na(recs$Camr_Nm)) #empty!
#recs<-recs[-nas,]
dim(recs)
dim(recs1)

sort(unique(recs$Camr_Nm)) == sort(unique(ston$Estacao)) 
records= recs; stations_on = ston; n.collap.days= 7 

###============================================STEP 5================================================###
###        	Function to create independent records table, collapsed capture histories and 
###          collapsed record histories
###=========================================================================================================###

#rename any columns that need to be renamed for the following function
names(recs.all)[names(recs.all) == "Camr_Nm"] <- "station"
names(records)[names(records) == "Camr_Nm"] <- "station"
names(records)[names(records) == "Species"] <- "species"
names(records)[names(records) == "Date"] <- "date"
names(recs)[names(recs) == "Camr_Nm"] <- "station"
names(recs)[names(recs) == "Species"] <- "species"
names(recs.all)[names(recs.all) == "Species"] <- "species"
names(recs.all)[names(recs.all) == "Date"] <- "date"

comm_hist_maker<-function(records, stations_on, n.collap.days){
  
  ## make sure, station names are always in upper case in both files
  stations_on$Estacao<-toupper(stations_on$Estacao)
  records$station<-toupper(records$station)
  
  ##get first and last date in camera functioning matrix (dates start with "X")
  xx<-colnames(stations_on)[min(grep("20", colnames(stations_on) ))]
  first<-as.Date(substr(xx, 1, nchar(xx)), "%Y-%m-%d") 
  xy<-colnames(stations_on)[max(grep("20", colnames(stations_on) ))]
  last<-as.Date(substr(xy, 1, nchar(xy)), "%Y-%m-%d")
  
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
  anynas <- which(is.na(records$Date))
  recs <-records[!is.na(records$Date),c("station", "species", "Date", "Time")] #error undefined columns
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
      dt<- as.POSIXlt(paste(sub2$Date, sub2$Time), tz = "America/Costa_Rica", format= "%m/%d/%Y %H:%M:%S")
      
      
      nr<-length(dt)
      
      recs.all<-rbind(recs.all, sub2[1,]) #always add first record
      
      
      if(dim(sub2)[1]>1) { #if more than one, check for independence
        
        for (k in 2:nr){
          dif<-difftime(dt[k], dt[k-1], units="mins")
          if(dif>60) {recs.all<-rbind(recs.all,sub2[k,] )} #error
        }
      }
    }
  }
  
  #remove starter row
  recs.all<-recs.all[-1,]
  
  
  #### make collapsed capture history ######################################
  
  record.mat<-array(0, c(J, K,length(species)))
  
  for (k in 1:K){
    
    dl<-seq.Date(beg[k], en[k], by=1)
    
    for (i in 1:length(species)){
      for (j in 1:J){
        subb<-recs.all[recs.all$station == stations[j] &
                         recs.all$species==species[i] &
                         as.Date(recs.all$Date, "%m/%d/%Y") %in% dl,]
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
  dimnames(crop_rec.mat)[[1]]<-dimnames(crop_rec.mat2)[[1]]<-rownames(crop_eff.coll)<-stations
  
  
  return(list("Collapsed record history"=crop_rec.mat, 
              "Collapsed capture history"=crop_rec.mat2, 
              "Occasion effort"=crop_eff.coll,
              "Independent detections"=recs.all))
}
#------------------------------------------------------------------------------------###

###----------------------------------------------------------------------------------###
### Running basic occupancy models									 ###
###----------------------------------------------------------------------------------###

ch_cr_north<- comm_hist_maker(records= recs, stations_on = ston, n.collap.days= 7) #error = undefined columns selected, #Error in if ( dif >60) { : missing value where TRUE/FALSE needed
str(ch_cr_north)

puma <- ch_cr_north[[2]][,,"Puma concolor"]
#write.csv(puma, "lwl_puma_detections.csv")

peccary <- ch_cr_north[[2]][,,"Pecari tajacu"]
#write.csv(peccary, "lwl_peccary_detections.csv")

paca <- ch_cr_north[[2]][,,"Cuniculus paca"]
#write.csv(paca, "lwl_paca_detections.csv")

tapir <- ch_cr_north[[2]][,,"Tapirus bairdii"]
#write.csv(tapir, "lwl_tapir_detections.csv")

oncilla <- ch_cr_north[[2]][,,"Leopardus tigrinus oncilla"]
#write.csv(oncilla, "lwl_oncilla_detections.csv")

ocelot <- ch_cr_north[[2]][,,"Leopardus pardalis"]
#write.csv(ocelot, "lwl_ocelot_detections.csv")

coyote <- ch_cr_north[[2]][,,"Canis latrans"]
#write.csv(coyote, "lwl_coyote_detections.csv")

eff<-ch_cr_north[[3]]


# Running Null model
library(unmarked)

umf_north<- unmarkedFrameOccu(y=peccary, siteCovs=, obsCovs=list(Eff=eff))

mod0 <- occu(~1~1, umf_north)  # Null Model
summary(mod0)
plogis(1.69)  	# Probability of occupancy = 0.8442242
plogis(-0.277)	# Probability of detection = 0.4311894

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf_north)  # Null Model
summary(m.psi1.pEff) 


#loading in covariates - refer to Markdown file "Extracting Site Covariates (1).Rmd" in order to extract covariates values
#already ran it ^, so just loading in the output file
siteCovs <- read.csv("Site_Covariates_Lowland3.csv")
siteCovs<-siteCovs[,-1]


#create a data frame with all rows and columns elevation, forest, and length
siteCovs<-data.frame(Lat=siteCovs[,"Latitud"], Long=siteCovs[,"Longitd"],
                     HFI=siteCovs[,"HFI_Value"], elev=siteCovs[,"Elevation"], 
                     Forest=siteCovs[,"Forest_Cover"], Water_Index = siteCovs[, "Water_Index"], 
                     NPP_value = siteCovs[, "NPP_value"], EdgeDens = siteCovs[, "EdgeDens"], 
                     PatchDens = siteCovs[, "PatchDens"], DisjCore = siteCovs[, "DisjCore"] )

#### Puma Occupancy ####
#Step 3
##number of sites (rows), number of occasions (columns)
dim(peccary) #23 sites, 19 times

##range of site covariates
range(siteCovs$elev) #25 583
range(siteCovs$HFI)
range(siteCovs$Water_Index)
range(siteCovs$NPP_value)
range(siteCovs$EdgeDens)
range(siteCovs$PatchDens)
range(siteCovs$DisjCore)

## combine everything into unmarkedFrameOccu
#umf<-unmarkedFrameOccu(y=obs, siteCovs=siteCovs, obsCovs=obsCovs)
#scaling site covariates gets rid of Hessian is singular error and NAns
#scale siteCovs
umf<-unmarkedFrameOccu(y=peccary, siteCovs = as.data.frame(scale(siteCovs)), obsCovs=list(Eff=eff))
summary(umf)

#first do null model (mod0)
mod0 <- occu(~1~1, umf)

#next do all univariate models
mod.elev <-occu(~1 ~elev, umf) 
mod.HFI <-occu(~1 ~HFI, umf)
mod.Eff <-occu(~eff ~1, umf) #warning: Some observations have been discarded because corresponding covariates were missing. 
mod.forest <- occu(~1~Forest,umf)
mod.prod <- occu(~1~NPP_value,umf)
#mod.road <- occu(~1~Road,umf)
mod.water <- occu(~1~Water_Index,umf)
mod.EdgeDens <- occu(~1~EdgeDens,umf)
mod.PatchDens <- occu(~1~PatchDens,umf)
mod.DisCore <- occu(~1~DisjCore,umf)

#compare univariate AIC's to null model AIC (ellie deleted mod.Eff and mod.road from this list)
UniList<-fitList(mod0, mod.elev, mod.HFI, mod.forest, mod.prod, mod.water, mod.EdgeDens, mod.PatchDens, mod.DisCore)
modSel(UniList) #best model is mod.elev

#Note which models are better than the null (mod.elev, mod.forest)
#use these models in your multivariate models, but disregard other variables

#multivariate models including mod.elev, mod.forest
mod.elev.forest <- occu(~1~elev+Forest,umf)

mod.forest <- occu(~1~Forest,umf)

mod.elev <- occu(~1~elev,umf)



#list models and compare AIC (including univariate models)
MultiList <- fitList(mod0,mod.elev.forest,mod.forest, mod.elev)

#model selection
modSel(MultiList) #mod.elev.forest is best, with mod.elev being equally supported


#now check models for significance


summary(mod.elev.forest) 
summary(mod.forest) 
summary(mod.elev)
summary(mod0) 

#none significant
