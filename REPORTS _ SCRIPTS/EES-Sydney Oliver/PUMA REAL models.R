setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")
rm(list=ls())
ocelot <- read.csv("ocelot_EES_7days.csv")
eff<-read.csv("eff_EES_7days.csv")
siteCovs <-read.csv("siteCovs.csv")

#Puma#
library(unmarked)
umf3<- unmarkedFrameOccu(y=ocelot[,-1], siteCovs=as.data.frame(scale(siteCovs[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf3)

mod0 <- occu(~1~1, umf3)#Null Model#
summary(mod0)

m.psi1.pEff<- occu(~Eff~1, umf3)
summary(m.psi1.pEff)

#Models to Run#
mod.psiElev.p3 <- occu(~1~siteCovs$ct_eLEV, umf3)
summary(mod.psiElev.p3)
mod.psiFore.p3 <- occu(~1~ct_fore, umf3)
summary(mod.psiFore.p3)
mod.psiHfp.p3 <- occu(~1~ct_hfp, umf3)
summary(mod.psiHfp.p3)
mod.psiHE.p3 <- occu(~1~ct_eLEV + ct_hfp, umf3)
summary(mod.psiHE.p3)
mod.psiWat.p3 <- occu(~1~ct_wat, umf3)
summary(mod.psiWat.p3)
mod.psiHW.p3 <- occu(~1~ct_hfp + ct_wat, umf3)
summary(mod.psiHW.p3)
mod.psiEW.p3 <- occu(~1~ct_eLEV + ct_wat, umf3)
summary(mod.psiEW.p3)



