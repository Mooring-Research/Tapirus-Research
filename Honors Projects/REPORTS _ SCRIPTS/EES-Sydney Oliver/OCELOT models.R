setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")
rm(list=ls())
ocelot <- read.csv("ocelot_EES_7days.csv")
eff<-read.csv("eff_EES_7days.csv")
siteCovs <-read.csv("siteCovs.csv")

#Ocelot##combine the models that perform better than the null#
library(unmarked)
umf3<- unmarkedFrameOccu(y=ocelot[,-1], siteCovs=as.data.frame(scale(siteCovs[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf3)
mod0<- occu(~1~1, umf3)#Null Model#
summary(mod0)

m.psi1.pEff<- occu(~Eff~1, umf3)
summary(m.psi1.pEff)

#Models to Run#
mod.psiElev.p3 <- occu(~1~siteCovs$ct_eLEV, umf3)
summary(mod.psiElev.p3)
mod.psiEdge.p3 <- occu(~1~edge, umf3)
summary(mod.psiEdge.p3)
mod.psiPatch.p3 <- occu(~1~patch, umf3)
summary(mod.psiPatch.p3)
mod.psiCore.p3 <- occu(~1~core, umf3)
summary(mod.psiCore.p3)
mod.psiRoad.p3 <- occu(~1~road, umf3)
summary(mod.psiRoad.p3)
mod.psiRiver.p3 <- occu(~1~river, umf3)
summary(mod.psiRiver.p3)
mod.psiNpp.p3 <- occu(~1~ct_npp, umf3)
summary(mod.psiNpp.p3)
mod.psiFore.p3 <- occu(~1~ct_fore, umf3)
summary(mod.psiFore.p3)
mod.psiHfp.p3 <- occu(~1~ct_hfp, umf3)
summary(mod.psiHfp.p3)
mod.psiPF.p3 <- occu(~1~patch + ct_fore, umf3)
summary(mod.psiPF.p3)
mod.psiHE.p3 <- occu(~1~ct_eLEV + ct_hfp, umf3)
summary(mod.psiHE.p3)
mod.psiWat.p3 <- occu(~1~ct_wat, umf3)
summary(mod.psiWat.p3)
mod.psiHW.p3 <- occu(~1~ct_hfp + ct_wat, umf3)
summary(mod.psiHW.p3)
mod.psiEW.p3 <- occu(~1~ct_eLEV + ct_wat, umf3)
summary(mod.psiEW.p3)

#Effort#
mod.psiElev.pEff3 <- occu(~Eff~ct_eLEV, umf3)
summary(mod.psiElev.pEff3)
mod.psiEdge.pEff3 <- occu(~Eff~edge, umf3)
summary(mod.psiEdge.pEff3)
mod.psiPatch.pEff3 <- occu(~Eff~patch, umf3)
summary(mod.psiPatch.pEff3)
mod.psiCore.pEff3 <- occu(~Eff~core, umf3)
summary(mod.psiCore.pEff3)
mod.psiRoad.pEff3 <- occu(~Eff~road, umf3)
summary(mod.psiRoad.pEff3)
mod.psiRiver.pEff3 <- occu(~Eff~river, umf3)
summary(mod.psiRiver.pEff3)
mod.psiNpp.pEff3 <- occu(~Eff~ct_npp, umf3)
summary(mod.psiNpp.pEff3)
mod.psiFore.pEff3 <- occu(~Eff~ct_fore, umf3)
summary(mod.psiFore.pEff3)
mod.psiHfp.pEff3 <- occu(~Eff~ct_hfp, umf3)
summary(mod.psiHfp.pEff3)
mod.psiHE.pEff3 <- occu(~Eff~ct_eLEV + ct_hfp, umf3)
summary(mod.psiHE.pEff3)
mod.psiWat.pEff3 <- occu(~Eff~ct_wat, umf3)
summary(mod.psiWat.pEff3)
mod.psiHW.pEff3 <- occu(~Eff~ct_hfp + ct_wat, umf3)
summary(mod.psiHW.pEff3)
mod.psiEW.pEff3 <- occu(~Eff~ct_eLEV + ct_wat, umf3)
summary(mod.psiEW.pEff3)
mod.psiEWH.pEff3 <- occu(~Eff~ct_eLEV + ct_wat + ct_hfp, umf3)
summary(mod.psiEWH.pEff3)


#--------------------Models that performed better than the null-----------#
mod.psiFore.p3 <- occu(~1~ct_fore, umf3)
summary(mod.psiFore.p3)






detList.ocelot<-fitList( mod0, m.psi1.pEff, mod.psiElev.p3,mod.psiEdge.p3,mod.psiPatch.p3,mod.psiCore.p3, mod.psiRoad.p3,mod.psiRiver.p3, mod.psiNpp.p3, mod.psiFore.p3,mod.psiHfp.p3,mod.psiHE.p3,mod.psiWat.p3,
	 mod.psiHW.p3,mod.psiPF.p3, mod.psiEW.p3,mod.psiElev.pEff3,mod.psiEdge.pEff3,mod.psiPatch.pEff3,mod.psiCore.pEff3, mod.psiNpp.pEff3, mod.psiFore.pEff3,mod.psiRoad.pEff3,mod.psiRiver.pEff3, mod.psiHfp.pEff3,
	 mod.psiHE.pEff3,mod.psiWat.pEff3,mod.psiHW.pEff3,mod.psiEW.pEff3,mod.psiEWH.pEff3)
Ocelot_AIC_Table <- modSel(detList.ocelot)
str(Ocelot_AIC_Table)
ModelRankOcelot <- as.data.frame(Ocelot_AIC_Table@Full)
write.csv(ModelRankOcelot, "Ocelot_AIC_Table.csv")


#-------------Predict for Plotting----------------#

#FOREST#
ocelot.fore = data.frame(ct_fore= ( seq(min(siteCovs$ct_fore), max(siteCovs$ct_fore),0.001) - mean(siteCovs$ct_fore) ) / sd(siteCovs$ct_fore))
pred.psi.fore <- predict(mod.psiFore.p3, newdata= ocelot.fore, "state")
plot(ocelot.fore$ct_fore, pred.psi.fore$Predicted, type="l",xlab="Forest Coverage", ylab="Occupancy probability", ylim=c(0,1))
points(ocelot.fore$ct_fore, pred.psi.fore$lower, type ="l", lty=2, col="blue")
points(ocelot.fore$ct_fore, pred.psi.fore$upper, type ="l", lty=2, col="blue")


