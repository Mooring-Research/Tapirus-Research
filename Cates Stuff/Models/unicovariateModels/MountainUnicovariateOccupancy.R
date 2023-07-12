#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")#Directory of R-project "Models" on github
dir()#

######Read in Tapir table and Effort###########
tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")

library(unmarked)

#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))

######Running Models!####################################
#Running Null model
mod0              <- occu(~1~1, umf)  # Null Model
m.psi1.pEff       <- occu(~Eff~1, umf)  # Eff Model
m.psiElev.pEff    <- occu(~Eff ~Elev, umf)
m.psiPrec.pEff    <- occu(~Eff ~Precip, umf)
m.psiRoad.pEff    <- occu(~Eff ~d.Road, umf)
m.psiTempmax.pEff <- occu(~Eff~ AvgMaxTemp, umf) 
m.psiNDVI.pEff    <- occu(~Eff~ NDVI, umf) 
m.psiTempmin.pEff <- occu(~Eff~ AvgMinTemp, umf)
m.psiElev.p1      <- occu(~1 ~Elev, umf)

##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other

detListUni.mt<-fitList(m.psi1.pEff,
                    m.pEff.psiPrec, 
                    m.pEff.psiElev, 
                    m.eff.road, 
                    m.psiTempmax.pEff,
                    m.psiNDVI.pEff)

# modSel compares AND ranks the models against each other!
modSel(detListUni.mt) 
