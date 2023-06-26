#############################
###title: "Occupancy Models-Puma"
###author: "Lily Bright"
###date: "6/6/2022"
###output: html_document
#############################

rm(list=ls())

setwd("/Users/lilybright/Documents/PLNU/Summer Research 2022/Official Data 2")
dir()

###----------------------------------------------------------------------------------###
### Running basic occupancy models									 ###
###----------------------------------------------------------------------------------###

puma <- read.csv("Puma Occ.csv")
head(puma)
sum(puma[-1], na.rm=T)
eff<-read.csv("Effort Chart.csv")
head(eff)
sitecovs <- read.csv("Site_Covariates.csv")
dim(sitecovs)
dim(puma)
dim(eff)
library(unmarked)
umf_south_pu<- unmarkedFrameOccu(y=puma[,-1], siteCovs=as.data.frame(scale(sitecovs[,-c(1:4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf_south_pu)

# Running Puma Null model
mod0 <- occu(~1~1, umf_south_pu)  # Null Model
summary(mod0)
plogis(-1.26)  	# Probability of occupancy .221
plogis( -2.57)	# Probability of detection .071 significant
#AIC :69.9139

# Running model with Eff as survey covariate(det)
m.psi1.pEff<- occu(~Eff~1, umf_south_pu)  
summary(m.psi1.pEff)
plogis(-1.32)#.21
new.eff<- 1:7
probeff<-plogis(0.20- 0.43*new.eff) #.55
plot(probeff)#negative correlation? shouldnt it be positive??
##AIC: 68.7


#Elevation and HFI Model(occ intercept)
pumamodELHF <- occu(~1 ~ Elev + HFI, umf_south_pu)
summary(pumamodELHF)
##AIC:69.27
##Does not converge


#Elevation Model
pummodele <- occu(~1~Elev, umf_south_pu)  
summary(pummodele)
range(sitecovs$Elev) #924:3319
new.elev<- 924:3319
probelev<-plogis(-2.78 + 6.71*new.elev)
plot(probelev) # no correlation, no slope
plogis( -3.08)  	# Probability of occupancy .04 significant
##AIC: 67.27


#HFI Model
pummodhfi <- occu(~1~HFI, umf_south_pu)  
summary(pummodhfi)
range(sitecovs$HFI) #3:5
new.hfi<- 3:5
probhfi<-plogis(-1.303 -0.669*new.hfi) 
plot(probhfi) #negative correlation- not significant
plogis(-2.62)# Probability of occupancy .067
##AIC: 71.165


#Effort and Elevation Model
pummodeffele <- occu(~Eff~Elev, umf_south_pu)  #not significant
summary(pummodeffele)
#AIC: 66.17


#Effort and HFI Model
pummodeffhfi <- occu(~Eff ~HFI, umf_south_pu) #effor and occupancy intercept significant
summary(pummodeffhfi)
##AIC: 69.87


#eff, ele and hfi
pummodhfieffele <- occu(~Eff~HFI + Elev, umf_south_pu)  
summary(pummodhfieffele) #not significant
##AIC: 67.34


#Primary Forest model
primforest <- occu(~1 ~primForest, umf_south_pu)  
summary(primforest)
plogis(-2.58) #0.07
range(sitecovs$primForest) #0:95
new.primForest<- 0:95
probprimF<-plogis(-2.48 + 3.40*new.primForest)#positive correlation? but large p-value :(
plot(probprimF) #positive correlation- not significant
#AIC: 69.779


#Net Productivity model
pumanpp <- occu(~1 ~NPP, umf_south_pu) 
summary(pumanpp)
plogis(-2.57) #det prob 0.071 - significant
range(sitecovs$NPP) #0:95
new.NPP<- 61:94
probNPP<-plogis(-1.274- 0.155*new.NPP)#negative correlation? but large p-value :(
plot(probNPP) #negative correlation- not significant
#AIC: 71.84 ##worst model


##Distance to Road
pumaroad <- occu(~1 ~d.Road, umf_south_pu)  
summary(pumaroad)
plogis(-2.64) #det prob 0.0666
range(sitecovs$d.Road) #0.0666:0.205
new.road<- 0.0666:0.205
probroad<-plogis(-1.35 + 0.919*new.road)#didnt really plot
plot(probroad) #doesnt plot - not significant
#AIC: 70.18


##Distance to River
pumariver <- occu(~1 ~d.River, umf_south_pu)  
summary(pumariver)
plogis(-2.44) #det prob 0.08 - significant
range(sitecovs$d.River) #0.00207418: 0.05314260
new.river<- 0.00207418: 0.05314260
probriver<-plogis(-3.18 + 2.88*new.river)
plot(probriver)#didnt really plot - occup intercept significant but not d.river.06
#AIC: 62.99

##Make AIC table Comparison
#leave out models that do not converge
detList.pu<-fitList(mod0,m.psi1.pEff,pummodele,pummodhfi,pummodeffhfi, primforest, pummodeffele, pumanpp, pumaroad, pumariver, pummodhfieffele)
Puma_AIC_Table<-modSel(detList.pu)
str(Puma_AIC_Table)
ModelRankPuma<-as.data.frame(Puma_AIC_Table@Full)
write.csv(ModelRankPuma, "Puma_AIC_Table.csv")
