#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")##different for Cate


######Read in Tapir table and Effort###########
tapir<- readRDS("Malayan/Collapsed_Capture_Malayan_Tapir.rds")
eff<- readRDS("Malayan/Effort_Malayan_Tapir.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("Malayan/Ma_T_Final_Covs.csv")

library(unmarked)


#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)
head(cov)

######Running Models!####################################

# Running model with Eff as survey covariate

m.psi1.pEff<- occu(~Eff~1, umf)  # Eff Model
#summary(m.psi1.pEff)

#~Eff ~Elevation
m.pEff.psiElev<- occu(~Eff ~Elev, umf)
summary(m.pEff.psiElev)

#~Eff ~Precipitaion
m.pEff.psiPrec<- occu(~Eff ~Precip, umf)
summary(m.pEff.psiPrec)
#hist(cov$Precipitation)

#~Eff ~Road
mod.eff.road <- occu(~Eff ~d.Road, umf)
summary(mod.eff.road)
plot(mod.eff.road)

#~Eff ~TempMax
m.psiTempmax.pEff<- occu(~Eff~ AvgMaxTemp, umf) 
summary(m.psiTempmax.pEff)

#~Eff ~NDVI
m.psiNDVI.pEff<- occu(~Eff~ NDVI, umf) 
summary(m.psiNDVI.pEff)

##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other
#detList.tapir<-fitList(mod0, m.psi1.pEff, m.p1.psiHFI, m.p1.psiElev, m.p1.psiPrec, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI)
detListUni.ma <-fitList(m.psi1.pEff,
                       m.pEff.psiPrec, 
                       m.pEff.psiElev,
                       mod.eff.road, 
                       m.psiTempmax.pEff,
                       m.psiNDVI.pEff)
# modSel compares AND ranks the models against eachother!

modSel(detListUni.ma)
