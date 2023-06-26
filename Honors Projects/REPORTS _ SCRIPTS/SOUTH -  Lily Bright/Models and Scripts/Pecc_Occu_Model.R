#############################
###title: "Occupancy Models-Peccary"
###author: "Lily Bright"
###date: "6/6/2022"
###output: html_document
#############################

rm(list=ls())

setwd("/Users/lilybright/Documents/PLNU/Summer Research 2022/Official Data 2")
dir()

###----------------------------------------------------------------------------------###
### Running Peccary occupancy models									 ###
###----------------------------------------------------------------------------------###



### Running Peccary Null model##
pecc <- read.csv("Peccary Occ.csv")
eff<-read.csv("Effort Chart.csv")
sitecovs <- read.csv("Site_Covariates.csv")
#?sum(peccary[-1], na.rm=T)
eff<-read.csv("Effort Chart.csv")
sitecovs <- read.csv("Site_Covariates.csv")
#?sum(paca[-1], na.rm=T)
eff<-read.csv("Effort Chart.csv")
head(eff)
sitecovs <- read.csv("Site_Covariates.csv")

library(unmarked)
umf_south_pe<- unmarkedFrameOccu(y=pecc[,-1], siteCovs=as.data.frame(scale(sitecovs[,-c(1:4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf_south_pe)

##Null Model
pecmod0 <- occu(~1~1, umf_south_pe)  # Null Model
summary(pecmod0)
plogis(-0.461)  	# Probability of occupancy .387 not significant
plogis( -3.15)	# Probability of detection .041 significant
##AIC: 73.94


# Running model with Eff as survey covariate(det)
peccEff<- occu(~Eff~1, umf_south_pe)  # Null Model
summary(peccEff)#high p-values
plogis(-0.425)#.39
new.peff<- 1:7
probpeceff<-plogis(-22.28+2.74*new.peff) #.55
plot(probpeceff)#positivie correlation not significant
##AIC: 74.59


#Elevation and HFI Model(occ intercept)
peccmodELHF <- occu(~1 ~ Elev + HFI,umf_south_pe)
summary(peccmodELHF) #not significant
plogis( -3.16)	# Probability of detection .04 significant
#AIC:66.155

#Elevation Model
peccmodele <- occu(~1~Elev, umf_south_pe)  # 
summary(peccmodele)
##AIC: 64.19
##does not converge!!!!

#HFI Model
peccmodhfi <- occu(~1~HFI, umf_south_pe)  # Null Model
summary(peccmodhfi)
##AIC: 74.39
##does not converge!!!!

#Effort and Elevation Model
peccmodeffele <- occu(~Eff~Elev, umf_south_pe)  # 
summary(peccmodeffele)
##AIC: 64.91
###Model doesnt coverge!!


#Effort and HFI Model
peccmodeffhfi <- occu(~Eff ~HFI, umf_south_pe)  # Null Model
summary(peccmodeffhfi)
##AIC: 74.9


#eff, ele and hfi
peccmodhfieffele <- occu(~Eff~HFI + Elev, umf_south_pe)  # Null Model
summary(peccmodhfieffele) #not significant
##AIC: 66.68


#Primary Forest model
peccprimforest <- occu(~1 ~primForest, umf_south_pe)  # Null Model
summary(peccprimforest) #large pvalue
plogis(-3.15) #Det prob .04 significant
range(sitecovs$primForest) #0:95
new.primForest<- 0:95
probprimF<-plogis(-0.749 + 1.303*new.primForest)#positive correlation? but large p-value :(
plot(probprimF) #exponential positive graph, not significant
#AIC: 74.63


#Net Productivity model
peccnpp <- occu(~1 ~NPP, umf_south_pe)  # Null Model
summary(peccnpp)
plogis(-3.18) #det prob 0.04 not significant
range(sitecovs$NPP) #61:94
new.NPP<- 61:94
probNPP<-plogis(-1.02 -6.80 *new.NPP)#negative S curve correlation? but large p-value :(
plot(probNPP) #straight line not significant
#AIC:69.17

##Npp, Ele, HFI
peccnppelehfi <- occu(~1 ~NPP + Elev +HFI, umf_south_pe)  # Null Model
summary(peccnppelehfi)
#AIC:66.3
#did not converge!!!

##Distance to Road
peccroad <- occu(~1 ~d.Road, umf_south_pe)  
summary(peccroad)
#AIC: 64.76
#did not converge!!

##Distance to River
peccriver <- occu(~1 ~d.River, umf_south_pe)  
summary(peccriver)
#AIC: 73.91
#model did not converge!!

##Make AIC table Comparison
#leave out models that do not converge
detList.pecc<-fitList(peccmodELHF,peccEff,pecmod0,peccmodeffhfi,peccmodhfieffele,peccprimforest,peccnpp)
Pecc_AIC_Table<-modSel(detList.pecc)
str(Pecc_AIC_Table)
ModelRankPecc<-as.data.frame(Pecc_AIC_Table@Full)
write.csv(ModelRankPecc, "Pecc_AIC_Table.csv")