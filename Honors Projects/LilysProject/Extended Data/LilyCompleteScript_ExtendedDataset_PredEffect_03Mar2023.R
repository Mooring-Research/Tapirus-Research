###-------------------------------------------------------------------------###
### Data processing - Coyote-Ocelot avoidance per region
### Lily's honors project
### 25 Feb 2023
###-------------------------------------------------------------------------###



rm(list=ls())

#import data
setwd("C:\\Users\\dgrocha\\OneDrive - Southern Nazarene University\\Desktop\\PLNU\\Lily_HonorsProject\\Extended_Dataset_Dec2022")

#import data
SAR<- read.csv("Recs_1h_CR_Dataset01_North_Sarah_04Dez2022.csv")
head(SAR)
day<- as.Date(SAR$Date, "%m/%d/%Y"); SAR<- cbind(SAR, day)
head(SAR) 

Lil<- read.csv("Recs_1h_CR_Dataset01_South_Lily_04Dez2022.csv")
head(Lil)
day<- as.Date(Lil$Date, "%m/%d/%y"); Lil<- cbind(Lil, day)
head(Lil)

ELL<- read.csv("Recs_1h_CR_Dataset04_Lowland_Ellie_04Dez2022.csv")
head(ELL)
day<- as.Date(ELL$Date, "%m/%d/%Y"); ELL<- cbind(ELL, day)
head(ELL)

EMI<- read.csv("Recs_1h_CR_Dataset02_Central_Emily_04Dez2022.csv")
head(EMI)
day<- as.Date(EMI$Date, "%m/%d/%Y"); EMI<- cbind(EMI, day)
head(EMI)

#create master
master<-rbind(SAR, Lil, ELL, EMI)
dim(master)
head(master)

which(is.na(master$day))

# It is not possible for me to know how many day each camera was active (Maybe Mike can help).
# But the number of day that each camera was active is the same for ocelots or Cayotes.
# Still, it would help the model to know effort, because the higher the effort the greater the chance that
#both species will be recorded at that given camera.
# I will use a proxy (number of days with animal photos) since we will not be able to know the exact number of day.

CT<-sort(unique(master$Camr_Nm))
p.effort<- vector()
masterb<- master[1,]; masterb<- masterb[-1,]
for(d in 1: length(CT)){
  x<- master[master$Camr_Nm== CT[d],]
  p.effort[d]<- length(unique(x[,"day"]))
  masterb<- rbind(masterb, x[order(x$day),])
}

names(p.effort)<- CT
head(masterb, 500)
head(masterb)
tail(masterb)

# Lets filter the dataset to make sure that there is only one record per species, per day camera, per day.

dat<- masterb[1,]
for(r in 2:nrow(masterb)){
  if(masterb[r,"Camr_Nm"]==dat[nrow(dat),"Camr_Nm"] &
     masterb[r,"Species"]==dat[nrow(dat),"Species"] &
     masterb[r,"day"]==dat[nrow(dat),"day"]){} else{dat[nrow(dat)+1,]<- masterb[r,]}
}
head(dat, 1000)
dim(dat)

# Getting Coyote and Ocelot number of records (number of day species were recorded) per camera.
ct_species<- table(dat$Camr_Nm, dat$Species)
head(ct_species)
(site_totals<- as.data.frame(rowSums(ct_species)));range(site_totals) #741
(spp_totals<- as.data.frame(colSums(ct_species)));range(spp_totals) #2601

colnames(ct_species)
dim(ct_species)
cooc<- ct_species[,c("Panthera onca", "Puma concolor", "Leopardus pardalis", "Canis latrans")]


head(cooc)
cooc<- as.data.frame(cbind(cooc[,1],cooc[,2], cooc[,3],cooc[,4]))
colnames(cooc)<- c("Jaguar","Puma", "Ocelot", "Coyote")
head(cooc)
dim(cooc)

rownames(cooc)== CT; names(p.effort)== CT ##??
mydata<- data.frame(CT= CT, DaysActive= p.effort, CoyoteDetcs= cooc$Coyote, OcelotDetcs= cooc$Ocelot, PumaDetcs=cooc$Puma, JaguarDetcs = cooc$Jaguar)
rownames(mydata)<- NULL
mydata	#ok, now we have the exaclty same data format we used last analysis round (but now with more cameras and multiple years)

colSums(mydata[-1])



#####-----------------------------data processing------------------------------#####

### not sure the best way to run this??

m0<- glm(OcelotDetcs ~ CoyoteDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m0) 	

m1<- glm(OcelotDetcs ~ PumaDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m1) 	

m4<- glm(OcelotDetcs ~ JaguarDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m4) 

m2<- glm(CoyoteDetcs ~ PumaDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m2) 	

m3<- glm(CoyoteDetcs ~ JaguarDetcs, data=mydata, family=poisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m3) 	

	







par(mfrow=c(1,3))
plot(density(rnboth), main="Coyote and Ocelot present (Both)")
abline(v = n.both, col="red", lwd=1, lty=1)
abline(v = quantile(rnboth)[c(2,4)], col="grey", lwd=0.5, lty=2)

plot(density(rnnone), main="Coyote and Ocelot Absent (None)")
abline(v = n.none, col="red", lwd=1, lty=1)
abline(v = quantile(rnnone)[c(2,4)], col="grey", lwd=0.5, lty=2)

plot(density(rneith), main="Either Coyote or Ocelot present (Either)")
abline(v = n.eith, col="red", lwd=1, lty=1)
abline(v = quantile(rneith)[c(2,4)], col="grey", lwd=0.5, lty=2)

