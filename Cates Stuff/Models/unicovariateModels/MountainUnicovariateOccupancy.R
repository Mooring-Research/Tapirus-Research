#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Peru (Mountain Tapir)")##different for Cate
dir()

######Read in Tapir table and Effort###########
tapir<- readRDS("Mountain/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
eff<- readRDS("Mountain/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("Mountain/Mt_T_Covs4.csv")

library(unmarked)

#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)
head(cov)

######Running Models!####################################
#Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf)  # Eff Model
summary(m.psi1.pEff)

#~Eff ~Elevation
m.pEff.psiElev<- occu(~Eff ~Elev, umf)
summary(m.pEff.psiElev)

#~Eff ~Precipitaion
m.pEff.psiPrec<- occu(~Eff ~Precip, umf)
summary(m.pEff.psiPrec)

#~Eff ~Road
mod.eff.road <- occu(~Eff ~d.Road, umf)
summary(mod.eff.road)

#~Eff ~TempMax
m.psiTempmax.pEff<- occu(~Eff~ AvgMaxTemp, umf) 
summary(m.psiTempmax.pEff)

#~Eff ~NDVI
m.psiNDVI.pEff<- occu(~Eff~ NDVI, umf) 
summary(m.psiNDVI.pEff)

##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other

detListUni.mt<-fitList(m.psi1.pEff,
                    m.pEff.psiPrec, 
                    m.pEff.psiElev, 
                    mod.eff.road, 
                    m.psiTempmax.pEff,
                    m.psiNDVI.pEff)

# modSel compares AND ranks the models against each other!
modSel(detListUni.mt) 
