setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")
rm(list=ls())
collared_pecary <- read.csv("collared_pecary_EES_7days.csv")
eff<-read.csv("eff_EES_7days.csv")
siteCovs <-read.csv("siteCovs.csv")


#Collared Pecary##combine the models that perform better than the null#
library(unmarked)
umf2<- unmarkedFrameOccu(y=collared_pecary[,-1], siteCovs=as.data.frame(scale(siteCovs[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf2)

mod0 <- occu(~1~1, umf2)#Null Model#
summary(mod0)

m.psi1.pEff<- occu(~Eff~1, umf2)
summary(m.psi1.pEff)

#Models to Run#
mod.psiElev.p2 <- occu(~1~siteCovs$ct_eLEV, umf2)
summary(mod.psiElev.p2)
mod.psiEdge.p2 <- occu(~1~edge, umf2)
summary(mod.psiEdge.p2)
mod.psiPatch.p2 <- occu(~1~patch, umf2)
summary(mod.psiPatch.p2)
mod.psiCore.p2 <- occu(~1~core, umf2)
summary(mod.psiCore.p2)
mod.psiRoad.p2 <- occu(~1~road, umf2)
summary(mod.psiRoad.p2)
mod.psiRiver.p2 <- occu(~1~river, umf2)
summary(mod.psiRiver.p2)
mod.psiNpp.p2 <- occu(~1~ct_npp, umf2)
summary(mod.psiNpp.p2)
mod.psiFore.p2 <- occu(~1~ct_fore, umf2)
summary(mod.psiFore.p2)
mod.psiHfp.p2 <- occu(~1~ct_hfp, umf2)
summary(mod.psiHfp.p2)
mod.psiRP.p2 <- occu(~1~river + patch, umf2)
summary(mod.psiRP.p2)
mod.psiHE.p2 <- occu(~1~ct_eLEV + ct_hfp, umf2)
summary(mod.psiHE.p2)
mod.psiWat.p2 <- occu(~1~ct_wat, umf2)
summary(mod.psiWat.p2)
mod.psiHW.p2 <- occu(~1~ct_hfp + ct_wat, umf2)#notconverging#
summary(mod.psiHW.p2)
mod.psiEW.p2 <- occu(~1~ct_eLEV + ct_wat, umf2)
summary(mod.psiEW.p2)

#Effort#
mod.psiElev.pEff2 <- occu(~Eff~ct_eLEV, umf2)
summary(mod.psiElev.pEff2)
mod.psiEdge.pEff2 <- occu(~Eff~edge, umf2)
summary(mod.psiEdge.pEff2)
mod.psiPatch.pEff2 <- occu(~Eff~patch, umf2)
summary(mod.psiPatch.pEff2)
mod.psiCore.pEff2 <- occu(~Eff~core, umf2)#notconverging#
summary(mod.psiCore.pEff2)
mod.psiRoad.pEff2 <- occu(~Eff~road, umf2)
summary(mod.psiRoad.pEff2)
mod.psiRiver.pEff2 <- occu(~Eff~river, umf2)
summary(mod.psiRiver.pEff2)
mod.psiNpp.pEff2 <- occu(~Eff~ct_npp, umf2)
summary(mod.psiNpp.pEff2)
mod.psiFore.pEff2 <- occu(~Eff~ct_fore, umf2)
summary(mod.psiFore.pEff2)
mod.psiHfp.pEff2 <- occu(~Eff~ct_hfp, umf2)
summary(mod.psiHfp.pEff2)
mod.psiRP.pEff2 <- occu(~Eff~river + patch, umf2)
summary(mod.psiRP.pEff2)
mod.psiHnP.pEff2 <- occu(~Eff~ct_hfp, umf2)
summary(mod.psiHnP.pEff2)
mod.psiHnR.pEff2 <- occu(~Eff~ct_hfp + river, umf2)
summary(mod.psiHnR.pEff2)
mod.psiHE.pEff2 <- occu(~Eff~ct_eLEV + ct_hfp, umf2)
summary(mod.psiHE.pEff2)
mod.psiWat.pEff2 <- occu(~Eff~ct_wat, umf2)
summary(mod.psiWat.pEff2)
mod.psiHnRP.pEff2 <- occu(~Eff~ct_hfp + river + patch, umf2)
summary(mod.psiHnRP.pEff2)
mod.psiHnER.pEff2 <- occu(~Eff~ct_hfp + ct_eLEV + river, umf2)
summary(mod.psiHnER.pEff2)
mod.psiHW.pEff2 <- occu(~Eff~ct_hfp + ct_wat, umf2)#notconverging#
summary(mod.psiHW.pEff2)
mod.psiEW.pEff2 <- occu(~Eff~ct_eLEV + ct_wat, umf2)
summary(mod.psiEW.pEff2)
mod.psiEWH.pEff2 <- occu(~Eff~ct_eLEV + ct_wat + ct_hfp, umf2)
summary(mod.psiEWH.pEff2)

#--------------Models that perform better than the null-----------------------#
mod.psiRP.pEff2 <- occu(~Eff~river + patch, umf2)
summary(mod.psiRP.pEff2)
mod.psiHE.p2 <- occu(~1~ct_eLEV + ct_hfp, umf2)
summary(mod.psiHE.p2)
mod.psiHfp.p2 <- occu(~1~ct_hfp, umf2)
summary(mod.psiHfp.p2)
mod.psiRP.p2 <- occu(~1~river + patch, umf2)
summary(mod.psiRP.p2)
mod.psiHnP.pEff2 <- occu(~Eff~ct_hfp, umf2)
summary(mod.psiHnP.pEff2)
mod.psiHfp.pEff2 <- occu(~Eff~ct_hfp, umf2)
summary(mod.psiHfp.pEff2)
mod.psiHnR.pEff2 <- occu(~Eff~ct_hfp + river, umf2)
summary(mod.psiHnR.pEff2)
mod.psiHE.pEff2 <- occu(~Eff~ct_eLEV + ct_hfp, umf2)
summary(mod.psiHE.pEff2)
mod.psiRiver.p2 <- occu(~1~river, umf2)
summary(mod.psiRiver.p2)
mod.psiRiver.pEff2 <- occu(~Eff~river, umf2)
summary(mod.psiRiver.pEff2)
mod.psiHnER.pEff2 <- occu(~Eff~ct_hfp + ct_eLEV + river, umf2)
summary(mod.psiHnER.pEff2)
mod.psiPatch.p2 <- occu(~1~patch, umf2)
summary(mod.psiPatch.p2)
mod.psiPatch.pEff2 <- occu(~Eff~patch, umf2)
summary(mod.psiPatch.pEff2)
mod.psiHnRP.pEff2 <- occu(~Eff~ct_hfp + river + patch, umf2)
summary(mod.psiHnRP.pEff2)
mod.psiEWH.pEff2 <- occu(~Eff~ct_eLEV + ct_wat + ct_hfp, umf2)
summary(mod.psiEWH.pEff2)


detList.collared_pecary <-fitList( mod0,m.psi1.pEff,mod.psiEdge.p2,mod.psiPatch.p2,mod.psiCore.p2, mod.psiRP.p2, mod.psiRoad.p2,mod.psiRiver.p2, mod.psiElev.p2, mod.psiNpp.p2, mod.psiFore.p2,mod.psiHfp.p2,mod.psiHE.p2,mod.psiWat.p2,
	 mod.psiEW.p2,mod.psiElev.pEff2,mod.psiHnR.pEff2,mod.psiHnER.pEff2, mod.psiEdge.pEff2,mod.psiHnP.pEff2, mod.psiPatch.pEff2, mod.psiNpp.pEff2,mod.psiRoad.pEff2,mod.psiRiver.pEff2, mod.psiFore.pEff2,mod.psiHfp.pEff2,
	 mod.psiHE.pEff2,mod.psiWat.pEff2,mod.psiEW.pEff2,mod.psiHnRP.pEff2, mod.psiRP.pEff2, mod.psiEWH.pEff2)
Collared_Pecary_AIC_Table <- modSel(detList.collared_pecary)
str(Collared_Pecary_AIC_Table)
ModelRankCollared_Pecary <- as.data.frame(Collared_Pecary_AIC_Table@Full)
write.csv(ModelRankCollared_Pecary, "Collared_Pecary_AIC_Table.csv")

#-------------------use predict function for plotting----------------------------------#

#River, DO NOT USE#
#effriver = data.frame(river= ( seq(min(siteCovs$river), max(siteCovs$river),0.001) - mean(siteCovs$river) ) / sd(siteCovs$river))
#pred.psi.river <- predict(mod.psiRiver.pEff2, newdata= effriver, "state")
#plot(effriver$river, pred.psi.river$Predicted, type="l",xlab="Distance to River", ylab="Occupancy probability", ylim=c(0,1))
#points(effriver$river, pred.psi.river$lower, type ="l", lty=2, col="blue")
#points(effriver$river, pred.psi.river$upper, type ="l", lty=2, col="blue")


#PATCH, DO NOT USE#
#effpatch = data.frame(patch= ( seq(min(siteCovs$patch), max(siteCovs$patch),0.1) - mean(siteCovs$patch) ) / sd(siteCovs$patch))
#pred.psi.patch <- predict(mod.psiPatch.pEff2, newdata=effpatch, "state")
#plot(effpatch$patch, pred.psi.patch$Predicted, type="l",xlab="Patch", ylab="Occupancy probability", ylim=c(0,1))
#points(effpatch$patch, pred.psi.patch$lower, type ="l", lty=2, col="red")
#points(effpatch$patch, pred.psi.patch$upper, type ="l", lty=2, col="red")

#####HUMAN FOOT PRINT, USE THIS MODEL######
pecary.hfp = data.frame(ct_hfp= ( seq(min(siteCovs$ct_hfp), max(siteCovs$ct_hfp),0.001) - mean(siteCovs$ct_hfp) ) / sd(siteCovs$ct_hfp))
pred.psi.hfp <- predict(mod.psiHfp.p2, newdata= pecary.hfp, "state")
plot(pecary.hfp$ct_hfp, pred.psi.hfp$Predicted, type="l",xlab="Human Foot Print", ylab="Occupancy probability", main = "Impact of Human Footprint Index on Collared Peccary Occupancy-Central", ylim=c(0,1))
points(pecary.hfp$ct_hfp, pred.psi.hfp$lower, type ="l", lty=2, col="blue")
points(pecary.hfp$ct_hfp, pred.psi.hfp$upper, type ="l", lty=2, col="blue")

#ELEVATION, DO NOT USE#
#pecary.elev = data.frame(ct_eLEV= ( seq(min(siteCovs$ct_eLEV), max(siteCovs$ct_eLEV),0.001) - mean(siteCovs$ct_eLEV) ) / sd(siteCovs$ct_eLEV))
#pred.psi.elev <- predict(mod.psiElev.p2, newdata= pecary.elev, "state")
#plot(pecary.hfp$ct_hfp, pred.psi.hfp$Predicted, type="l",xlab="Human Foot Print", ylab="Occupancy probability", ylim=c(0,1))
#points(pecary.hfp$ct_hfp, pred.psi.hfp$lower, type ="l", lty=2, col="blue")
#points(pecary.hfp$ct_hfp, pred.psi.hfp$upper, type ="l", lty=2, col="blue")














