#clear environment
rm(list=ls())

#import data
setwd("C:\\Users\\dgrocha\\OneDrive - Southern Nazarene University\\Desktop\\PLNU\\Lily_HonorsProject")

#import detection info
mydata<- read.csv("DailyRecs_OceCoy.csv")
hist(mydata$DaysActive)
hist(mydata$CayoteDetcs) 	#This is count data. It will not fit to a normal distribution.
hist(mydata$OcelotDetcs) 	#This is count data. It will not fit to a normal distribution.

plot(mydata$OcelotDetcs, mydata$CayoteDetcs) #Keep it simple. We do not need ggplot.

m1<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=poisson)
summary(m1)
m1<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=poisson, offset=scale(mydata$DaysActive)) 	#the offset accounts for the fact that effort is not equal across CTs.
summary(m1) 	# Accounting for effort makes result slightly different

(var(mydata$OcelotDetcs)/mean(mydata$OcelotDetcs)) #Overdispersion? YES. In that case we should not use Poisson distribution, let's use something else

m2<- glm(OcelotDetcs ~ CayoteDetcs, data=mydata, family=quasipoisson, offset=scale(mydata$DaysActive)) 	#Quasipoison Distribution
summary(m2)		# As expected the direction and effect size is very similar, but confidence interval is larger. 
library(MASS)
m3<- glm.nb(OcelotDetcs ~ CayoteDetcs, data=mydata)		#NEgative Binomial Distribution
summary(m3)		# The same as Quasipoisson.

# CONCLUSION: It seems like there is an effect, and based on our best model it is marginally significant. Maybe a bigger dataset would help us. 

# So far we have tested if there is a relationship between the number of detections of ocelots and the number of detections of coyotes.
# Let's try something different. If they are avoiding each other, the number of site that we have one species or the other shouls
#be higher than expected by chance, right? This is one hypothesis that is very easy to test. 

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

# WOW... using this other approach it also seems like our result is a bit extreme in comparison to what would be expected if there was no avoidance.
# I guess it is worth investigating it.

# Another interesting aspect is to check if they avoid one another on a temporal scale. All we have done so far is spatial.

#### Temporal Analysis
# Importing detection histories from different datasets
cr1<- readRDS("ch_cr1_1d.rds")
cr2<- readRDS("ch_cr2_1d.rds")
cr3<- readRDS("ch_cr3_1d.rds")
cr4<- readRDS("ch_cr4_1d.rds")

recs_t<- rbind(cr1[[4]], cr2[[4]], cr3[[4]], cr4[[4]])
head(recs_t)

recs_t$hour<-format(as.POSIXct(recs_t$hour, format="%H:%M:%S"), "%H:%M")

ocelot.data<- recs_t[recs_t$species=="Leopardus pardalis", ]
coyote.data<- recs_t[recs_t$species=="Canis latrans", ]

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

ocelot.data<- mydata2[mydata2$species== "Leopardus pardalis",]
nrow(ocelot.data)
coyote.data<- mydata2[mydata2$species== "Canis latrans",]
nrow(coyote.data)

# Ok... data is ready to run overlap analysis, take it from here Lily!