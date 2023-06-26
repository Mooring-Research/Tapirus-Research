#############################
###title: "Occupancy Models-Paca"
###author: "Lily Bright"
###date: "6/6/2022"
###output: html_document
#############################

rm(list=ls())

setwd("/Users/lilybright/Documents/PLNU/Summer Research 2022/Official Data 2")
dir()

###----------------------------------------------------------------------------------###
### Running Paca occupancy models									 ###
###----------------------------------------------------------------------------------###

### Running Paca Null model##
paca <- read.csv("Paca Occ.csv")
eff<-read.csv("Effort Chart.csv")
sitecovs <- read.csv("Site_Covariates.csv")
#?sum(paca[-1], na.rm=T)
eff<-read.csv("Effort Chart.csv")
head(eff)
sitecovs <- read.csv("Site_Covariates.csv")
  
library(unmarked)
umf_south_pa<- unmarkedFrameOccu(y=paca[,-1], siteCovs=as.data.frame(scale(sitecovs[,-c(1:4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf_south_pa)


### Running Paca Null model###
pacmod0 <- occu(~1~1, umf_south_pa)  # Null Model
summary(pacmod0)
plogis(-0.666)  	# Probability of occupancy .333
plogis( -1.83)	# Probability of detection .138
##AIC:152.7


##Running model with Eff as survey covariate(det)
pacEff<- occu(~Eff~1, umf_south_pa)  # Null Model
summary(pacEff)#high p-values
plogis(-0.669)#.33
new.peff<- 1:7
probpeff<-plogis(-6.0430+0.616*new.peff) #.55
plot(probpeff)#positivie correlation again????
##AIC: 152.15


##Elevation and HFI Model(occ intercept)
pacamodELHF <- occu(~1 ~ Elev + HFI,umf_south_pa)
summary(pacamodELHF)
#AIC:132.73


##Elevation Model
pacmodele <- occu(~1~Elev, umf_south_pa)  # 
summary(pacmodele)
range(sitecovs$Elev) #924:3319
new.elev<- 924:3319
probelev<-plogis(-0.83 -1.89*new.elev) #IDK WHAT TO DO WIFF THIS
plot(probelev) #straight line
plogis( -1.86)  	# Probability of det .134
##AIC: 145.58


##HFI Model
pacmodhfi <- occu(~1~HFI, umf_south_pa)  # Null Model
summary(pacmodhfi)
range(sitecovs$HFI) #3:5
new.hfi<- 3:5
probhfi<-plogis(-0.743 +  0.766*new.hfi) #IDK WHAT TO DO WIFF THIS
plot(probhfi) #positive correlation not significant
plogis(-1.82)  	# Probability of detection .134
##AIC: 152.1265


##Effort and Elevation Model
pacmodeffele <- occu(~Eff~Elev, umf_south_pa) 
summary(pacmodeffele)
##Model doesnt coverge!!


##Effort and HFI Model
pacmodeffhfi <- occu(~Eff ~HFI, umf_south_pa)
summary(pacmodeffhfi)
range(sitecovs$HFI) #3:5
new.hfi<- 3:5
probhfielev<-plogis(-0.746 +0.762*new.hfi) #IDK WHAT TO DO WIFF THIS
dethfielev<-plogis( -6.038 + 0.616*new.peff)
plot(dethfielev) #positive correlation not sig
plot(probhfielev) #positive correlation not sig
##AIC: 151.5474

#eff, ele and hfi
pacmodhfieffele <- occu(~Eff~HFI + Elev,umf_south_pa)  
summary(pacmodhfieffele) #all high pvalues
##AIC: 138.4976


#Primary Forest model
pacprimforest <- occu(~1 ~primForest, umf_south_pa) 
summary(pacprimforest) #large pvalue
plogis(-1.83) #Det prob .14
range(sitecovs$primForest) #0:95
new.primForest<- 0:95
probprimF<-plogis(-0.664 -0.102*new.primForest)#positive correlation? but large p-value :(
plot(probprimF) #negative correlation  not sig only det significant
#AIC: 154.68

#Net Productivity model
pacanpp <- occu(~1 ~NPP, umf_south_pa)  # Null Model
summary(pacanpp)
plogis(-2.04) #det prob 0.115
range(sitecovs$NPP) #61:94
new.NPP<- 61:94
probNPP<-plogis(13.695 - 0.193*new.NPP)#negative correlation not sig only det significant
plot(probNPP)
#AIC:142.8 

##Npp, Ele, HFI
pacanppelehfi <- occu(~1 ~NPP + Elev +HFI, umf_south_pa)
summary(pacanppelehfi) #not significant
#AIC:134.4944 BEST MODEL SO FARRRR


#AIC:147.87
##Distance to Road
pacaroad <- occu(~1 ~d.Road, umf_south_pa)  
summary(pacaroad)
plogis(-1.78) #det prob  0.14 signifcant pval
range(sitecovs$d.Road) #0.06643625 :0.20588820
new.paroad<- 0.06643625: 0.20588820
probparoad<-plogis(-1.94 -2.97 *new.paroad)
plot(probparoad) #doesnt plot
#AIC: 140.9982


##Distance to River
pacariver <- occu(~1 ~d.River, umf_south_pa)  
summary(pacariver)
plogis(-1.84) #det prob  0.1370513 signifcant pval
range(sitecovs$d.River) #0.00207418:0.05314260
new.pariver<- 0.00207418:0.05314260
probpariver<-plogis(0.107 -29.607*new.pariver)#didnt really plot
plot(probpariver) #not significant
#AIC: 153.87

##Make AIC table Comparison
#leave out models that do not converge
detList.paca<-fitList(pacmod0,pacEff, pacmodhfi, pacmodeffhfi, pacmodhfieffele, pacprimforest, pacanpp, pacanppelehfi, pacaroad, pacariver)
Paca_AIC_Table<-modSel(detList.paca)
str(Paca_AIC_Table)
ModelRankPaca<-as.data.frame(Paca_AIC_Table@Full)
write.csv(ModelRankPaca, "Paca_AIC_Table.csv")
