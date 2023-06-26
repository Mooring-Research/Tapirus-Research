setwd("C:\\Users\\sydne\\Documents\\Research Project\\REAL DATA SET")
rm(list=ls())
paca<-read.csv("paca_EES_7days.csv")
eff<-read.csv("eff_EES_7days.csv")
siteCovs <-read.csv("siteCovs.csv")

#Paca##combine the models that perform better than the null#
library(unmarked)
umf<- unmarkedFrameOccu(y=paca[,-1], siteCovs= as.data.frame(scale(siteCovs[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)

mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis(1.41) #probability of occupancy
plogis( -0.25) #probability of detection

m.psi1.pEff3<- occu(~Eff~1, umf)  
summary(m.psi1.pEff3)
plogis(1.4)#probability of occupancy
plogis( 0.251)#probability of detection

new.eff<-(seq(1,7,1))
pred.p <- round(plogis(-2.575 + -0.405 *new.eff),3)
pred.p

mod0@AIC
m.psi1.pEff@AIC

#Models to Run#
mod.psiElev.p1 <- occu(~1~ct_eLEV, umf)
summary(mod.psiElev.p1)
mod.psiEdge.p1 <- occu(~1~edge, umf)
summary(mod.psiEdge.p1)
mod.psiPatch.p1 <- occu(~1~patch, umf)
summary(mod.psiPatch.p1)
mod.psiCore.p1 <- occu(~1~core, umf)
summary(mod.psiCore.p1)
mod.psiRoad.p1 <- occu(~1~road, umf)
summary(mod.psiRoad.p1)
mod.psiRiver.p1 <- occu(~1~river, umf)#notconverging#
summary(mod.psiRiver.p1)
mod.psiNpp.p1 <- occu(~1~ct_npp, umf)
summary(mod.psiNpp.p1)
mod.psiFore.p1 <- occu(~1~ct_fore, umf)
summary(mod.psiFore.p1)
mod.psiHfp.p1 <- occu(~1~ct_hfp, umf)
summary(mod.psiHfp.p1)
mod.psiHE.p1 <- occu(~1~ct_eLEV + ct_hfp, umf)
summary(mod.psiHE.p1)
mod.psiWat.p1 <- occu(~1~ct_wat, umf)
summary(mod.psiWat.p1)
mod.psiHW.p1 <- occu(~1~ct_hfp + ct_wat, umf)
summary(mod.psiHW.p1)
mod.psiEW.p1 <- occu(~1~ct_eLEV + ct_wat, umf) #notconverging#
summary(mod.psiEW.p1)
mod.psiEWH.p1 <- occu(~1~ct_eLEV + ct_wat + ct_hfp, umf)
summary(mod.psiEWH.p1)



#Effort#
mod.psiElev.pEff <- occu(~Eff~ct_eLEV, umf)
summary(mod.psiElev.pEff)
mod.psiEdge.pEff <- occu(~Eff~edge, umf)
summary(mod.psiEdge.pEff)
mod.psiPatch.pEff <- occu(~Eff~patch, umf)
summary(mod.psiPatch.pEff)
mod.psiCore.pEff <- occu(~Eff~core, umf)
summary(mod.psiCore.pEff)
mod.psiFeg.pEff <- occu(~Eff~ct_fore + edge, umf)
summary(mod.psiFeg.pEff)
mod.psiWF.pEff <- occu(~Eff~ct_wat+ct_fore, umf)
summary(mod.psiWF.pEff)
mod.psiPF.pEff <- occu(~Eff~patch + ct_fore, umf)
summary(mod.psiPF.pEff)
mod.psiHfF.pEff <- occu(~Eff~ct_hfp+ct_fore, umf)
summary(mod.psiHF.pEff)
mod.psiRoad.pEff <- occu(~Eff~road, umf)
summary(mod.psiRoad.pEff)
mod.psiRiver.pEff <- occu(~1~river, umf)
summary(mod.psiRiver.pEff)
mod.psiNpp.pEff <- occu(~Eff~ct_npp, umf)
summary(mod.psiNpp.pEff)
mod.psiFore.pEff <- occu(~Eff~ct_fore, umf)
summary(mod.psiFore.pEff)
mod.psiHfp.pEff <- occu(~Eff~ct_hfp, umf)
summary(mod.psiHfp.pEff)
mod.psiHE.pEff <- occu(~Eff~ct_eLEV + ct_hfp, umf)
summary(mod.psiHE.pEff)
mod.psiWat.pEff <- occu(~Eff~ct_wat, umf)
summary(mod.psiWat.pEff)
mod.psiHW.pEff <- occu(~Eff~ct_hfp + ct_wat, umf)
summary(mod.psiHW.pEff)
mod.psiEW.pEff <- occu(~Eff~ct_eLEV + ct_wat, umf)
summary(mod.psiEW.pEff)
mod.psiEWH.pEff <- occu(~Eff~ct_eLEV + ct_wat + ct_hfp, umf)
summary(mod.psiEWH.pEff)

#Models that perform better than the null#


detList.Paca<-fitList( mod0, m.psi1.pEff3, mod.psiElev.p1,mod.psiEdge.p1, mod.psiPatch.p1, mod.psiCore.p1, mod.psiRoad.p1,mod.psiRiver.p1, mod.psiNpp.p1, mod.psiFore.p1,mod.psiHfp.p1,mod.psiHE.p1,mod.psiWat.p1,
	 mod.psiHW.p1, mod.psiEW.p1,mod.psiElev.pEff, mod.psiPF.pEff,mod.psiFeg.pEff, mod.psiEdge.pEff,mod.psiPatch.pEff,mod.psiCore.pEff, mod.psiWF.pEff, mod.psiHfF.pEff, mod.psiRoad.pEff,mod.psiRiver.pEff, mod.psiNpp.pEff, mod.psiFore.pEff,mod.psiHfp.pEff,
	 mod.psiHE.pEff,mod.psiWat.pEff,mod.psiHW.pEff,mod.psiEW.pEff,mod.psiEWH.pEff)
Paca_AIC_Table <- modSel(detList.Paca)
str(Paca_AIC_Table)
ModelRankPaca <- as.data.frame(Paca_AIC_Table@Full)
write.csv(ModelRankPaca, "Paca_AIC_Table.csv")

#-------------------------Predict for Plotting------------------------------#
effpaca <- 1:7
plot()


paca.hfp = data.frame(ct_hfp= ( seq(min(siteCovs$ct_hfp), max(siteCovs$ct_hfp),0.001) - mean(siteCovs$ct_hfp) ) / sd(siteCovs$ct_hfp))
pred.psi.hfp <- predict(mod.psiHfp.p2, newdata= paca.hfp, "state")
plot(paca.hfp$ct_hfp, pred.psi.hfp$Predicted, type="l",xlab="Human Foot Print", ylab="Occupancy probability", ylim=c(0,1))
points(paca.hfp$ct_hfp, pred.psi.hfp$lower, type ="l", lty=2, col="blue")
points(paca.hfp$ct_hfp, pred.psi.hfp$upper, type ="l", lty=2, col="blue")





