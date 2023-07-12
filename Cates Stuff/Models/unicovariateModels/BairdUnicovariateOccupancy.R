###------------------------------------------------------------------------------###
### Running models on baird's
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")##different for Cate
library(unmarked)

tapir_t<- readRDS("Baird's/tapir_CR.rds")
head(tapir_t)
eff_t<- readRDS("Baird's/eff_CR.rds")
head(eff_t)
cv_t3<- read.csv("Baird's/cv_t3.csv")
head(cv_t3)
cv_t3<- cbind(cv_t3[,2:5], round(scale(cv_t3[,6:ncol(cv_t3)]),3))

rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == cv_t3$Station

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=cv_t3, obsCovs=list(Eff=eff_t))
summary(umf)

#-----------------------------------------------------------------------
# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
#summary(m.psi1.pEff)

# Running unicovariate models
m.psiElev.pEff<- occu(~Eff~Elev , umf)
# summary(m.psiElev.pEff)

m.psiRoad.pEff<- occu(~Eff~d.Road , umf)
#summary(m.psiRoad.pEff)

m.psiTempmax.pEff<- occu(~Eff~ Avg.Max.Temp, umf) 
#summary(m.psiTempmax.pEff)
#plogis( -0.0803) #Occupancy Estimate= 0.4799358
#plogis(-0.0504) #Detection Estimate= 0.4874027

m.psiNDVI.pEff<- occu(~Eff~ NDVI, umf)
#summary(m.psiNDVI.pEff)

#~Eff ~Precipitaion
m.pEff.psiPrec<- occu(~Eff ~Precip, umf)
summary(m.pEff.psiPrec)

##collect in fitList
detListUni.bd<-fitList(m.psi1.pEff,
                 m.psiElev.pEff, 
                 m.psiRoad.pEff,
                 m.pEff.psiPrec,
                 m.psiTempmax.pEff,
                 m.psiNDVI.pEff)

##do AIC model selection
modSel(detListUni.bd) 