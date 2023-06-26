
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

# Lets filter the dataset in a way that there is only one record per species, per dar camera, per day.

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
(site_totals<- as.data.frame(rowSums(ct_species)));range(site_totals)
(spp_totals<- as.data.frame(colSums(ct_species)));range(spp_totals)

colnames(ct_species)
cooc<- ct_species[,c("Canis latrans","Leopardus pardalis")]
head(cooc)
cooc<- as.data.frame(cbind(cooc[,1],cooc[,2]))
colnames(cooc)<- c("Coyote","Ocelot")
head(cooc)
dim(cooc)

rownames(cooc)== CT; names(p.effort)== CT

region<- c(rep("Sarah", length(unique(SAR$Camr_Nm))), 
		rep("Lily", length(unique(Lil$Camr_Nm))),
		rep("Ellie", length(unique(ELL$Camr_Nm))),
		rep("Emily", length(unique(EMI$Camr_Nm))))

region2<- c(rep("Sarah", length(unique(SAR$Camr_Nm))), 
		rep("All", length(unique(Lil$Camr_Nm))),
		rep("All", length(unique(ELL$Camr_Nm))),
		rep("All", length(unique(EMI$Camr_Nm))))

mydata<- data.frame(CT= CT, DaysActive= p.effort, OcelotDetcs= cooc$Coyote, CayoteDetcs= cooc$Ocelot, Region= region, Region2= region2)
rownames(mydata)<- NULL
mydata	#ok, now we have the exaclty same data format we used last analysis round (but now with more cameras and multiple years)

###------------------------------------------------------------------------------------------------###

hist(mydata$DaysActive)
hist(mydata$CayoteDetcs) 	#This is count data. It will not fit to a norma distribution.
hist(mydata$OcelotDetcs) 	#This is count data. It will not fit to a norma distribution.

plot(mydata$OcelotDetcs, mydata$CayoteDetcs) #Keep it simple. We do not need ggplot.

m1<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=poisson)
summary(m1)
m1<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=poisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m1) 	# Accounting for effort makes result slightly different

(var(mydata$OcelotDetcs)/mean(mydata$OcelotDetcs)) #Overdispersion? YES. In that case we should not use Poisson distribution, lets use something else

m2<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#Quasipoison Distribution
summary(m2)		# As expected the direction and effect size is very similar, but confidence interval is larger. 

library(MASS)
m3<- glm.nb(OcelotDetcs ~ CayoteDetcs, data=mydata)		#NEgative Binomial Distribution
summary(m3)		# The same as Quasipoisson.

m4<- glm.nb(OcelotDetcs ~ CayoteDetcs + DaysActive, data=mydata)		#NEgative Binomial Distribution
summary(m4)		# 

m5<- glm.nb(OcelotDetcs ~ Region + CayoteDetcs + DaysActive , data=mydata)		#NEgative Binomial Distribution
summary(m5)	

m6<- glm.nb(OcelotDetcs ~ Region2 + CayoteDetcs + DaysActive , data=mydata)		#NEgative Binomial Distribution
summary(m6)	

# CONCLUSION: It seems like there is an effect, and based on our best model it is marginally significant. Maybe a bigger dataset would help us. 

# So far we have tested if there is a relationship between number of detections of ocelot and number of detections of coyotes.
# Let's try something different. If they are avoiding each other, the number of site that we have one species or the other should
#be higher than expected by chance, rihgt? This is one hypothesis that is very easy to test. 

pa_Coyote<- mydata$CayoteDetcs
pa_Coyote[pa_Coyote>1]<-1
pa_Ocelot<- mydata$OcelotDetcs
pa_Ocelot[pa_Ocelot>1]<-1
either<- pa_Ocelot + pa_Coyote
either[either>1]<-0

rneith<- vector()
for(i in 1:3000){
	s1<-sample(seq(1,nrow(mydata),1),nrow(mydata))
	s2<-sample(seq(1,nrow(mydata),1),nrow(mydata))
	nt<- cbind(pa_Coyote[s1],pa_Ocelot[s2])
	rneith[i]<-sum(rowSums(nt,na.rm=T)==1)
}

plot(density(rneith), main="Either Coyote or Ocelot present (Either)")
abline(v = quantile(rneith, c(.05, .95)), col="grey", lwd=0.5, lty=2)
abline(v = sum(either,na.rm=T), col="red", lwd=1, lty=1)

# WOW... using this other approach it also seems like our result is a bit extreme in comparison to what would be expecte if there was no avoidence.
# I guess it is worth keep investigating it.

# Another interesting aspect is to check if they avoid one another in a temporal scale. All we have done so far is spatial.

#### Temporal Analysis
recs_t<- masterb
head(recs_t)

recs_t$hour<-format(as.POSIXct(recs_t$Time, format="%H:%M:%S"), "%H:%M")

ocelot.data<- recs_t[recs_t$Species=="Leopardus pardalis", ]
coyote.data<- recs_t[recs_t$Species=="Canis latrans", ]

library("overlap")

#transformando watch time to day time
timeToFraction<- function(time) {
   # Function to convert times in the form hh:mm
   # to fraction of the 24-hour day
 
 t = unlist(strsplit(time, ":"))
 n = length(time)
 hours = as.numeric(t[2*(1:n)-1])
 minutes = as.numeric(t[2*(1:n)])
 (hours + minutes/60) / 24
}

quote_hour<-matrix(recs_t$hour) # (ex.: "10:30")

f.hour<-round(timeToFraction(quote_hour),3)
range(f.hour)

timeRad<-f.hour*2*pi

mydata2<- cbind(recs_t, f.hour= round(f.hour,3), rad.hour= round(timeRad,3))
head(mydata2)

ocelot.data<- mydata2[mydata2$Species== "Leopardus pardalis",]
nrow(ocelot.data)
coyote.data<- mydata2[mydata2$Species== "Canis latrans",]
nrow(coyote.data)

# Ok... data is ready to run overlap analysis, take it from here Lily!
overlapEst(ocelot.data$rad.hour,coyote.data$rad.hour)
overlapPlot(ocelot.data$rad.hour,coyote.data$rad.hour)

densityPlot(ocelot.data$rad.hour, linet = c(1,1), linec = c("red", "blue"), linew = c(2, 2),
	rug=TRUE,  main="Ocelot Activity Pattern", xcenter = "noon")

densityPlot(coyote.data$rad.hour, linet = c(1,1), linec = c("red", "blue"), linew = c(2, 2),
	rug=TRUE,  main="Coyote Activity Pattern", xcenter = "noon")

overlapPlot(ocelot.data$rad.hour,coyote.data$rad.hour, linet = c(1,1), linec = c("red", "blue"), linew = c(2, 2),
	rug=TRUE,  main="Ocelot vs. Coyote Activity Pattern", xcenter = "noon")
legend("topleft", c("Ocelot", "Coyote"), lty=1, col=c("red", "blue"))

