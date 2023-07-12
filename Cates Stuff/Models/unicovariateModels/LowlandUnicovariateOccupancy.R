###------------------------------------------------------------------------------###
### Combining datasets from AM
### Sarah Turcic (Edited by DR on 10Oct2022
### Date: 9/12/22
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Amazon (Lowland Tapir)")##different for Cate

library(unmarked)

#Read in Variables###################################################################

#read in tapir occurance records
tapir_t<- readRDS("Lowland/tapir_AM.rds")
head(tapir_t)

#read in effort table
eff_t<- readRDS("Lowland/eff_AM.rds")
head(eff_t)

#read in covariate table
sc_t<- read.csv("Lowland/cv_t_AM_v2.csv")
head(sc_t)

#scale covariates
sc_t2<- cbind(sc_t[,c(1:4)], round(scale(sc_t[,5:ncol(sc_t)]),3))

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == sc_t2$Station

# Checking for sitecov correlations
head(sc_t2)
as.dist(cor(sc_t2[,-c(1:4)]))
# Some sitecovs are correlated: Road&Elev, ED&PD&DC (do not include correlated covs in the same model)

#Establish Unmarked Data Frame##############################################################

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf) #67 sites with detection


#Running Models#######################################################################

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
m.psiElev.pEff<- occu(~Eff~Elev , umf)
summary(m.psiElev.pEff)
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)  
plogis(0.156)
plogis(0.265)
summary(m.psiRoad.pEff)
m.psiNDVI.pEff<- occu(~Eff~NDVI , umf)
summary(m.psiNDVI.pEff)
m.psiTemp.pEff<- occu(~Eff~Avg.Max.Temp , umf)
summary(m.psiTemp.pEff)
m.psiPrecip.pEff<- occu(~Eff~MAP , umf)
summary(m.psiPrecip.pEff)


##collect in fitList
detListUni.low<-fitList(m.psi1.pEff,
                 m.psiElev.pEff, 
                 m.psiRoad.pEff,
                 m.psiNDVI.pEff,
                 m.psiTemp.pEff,
                 m.psiPrecip.pEff)

##do AIC model selection
modSel(detListUni.low) 
