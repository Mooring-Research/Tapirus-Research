#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")##different for Cate


######Read in Tapir table and Effort###########
tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Collapsed_Capture_Malayan_Tapir.rds")

eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Data Processing/Effort_Malayan_Tapir.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Ma_T_Final_Covs.csv")

library(unmarked)


#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)
head(cov)

######Running Models!####################################

# Running model with Eff as survey covariate
m.psi1.pEff      <- occu(~Eff~1, umf)  # Eff Model
m.psiElev.pEff   <- occu(~Eff ~Elev, umf)
m.psiPrec.pEff   <- occu(~Eff ~Precip, umf)
mod.psiRoad.pEff <- occu(~Eff ~d.Road, umf)
m.psiTempmax.pEff<- occu(~Eff~ AvgMaxTemp, umf) 
m.psiNDVI.pEff   <- occu(~Eff~ NDVI, umf) 
m.psiTempmin.pEff<- occu(~Eff~ AvgMinTemp, umf)
m.psiHFI.pEff    <- occu(~Eff ~HFI, umf)





##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other
#detList.tapir<-fitList(mod0, m.psi1.pEff, m.p1.psiHFI, m.p1.psiElev, m.p1.psiPrec, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI)
detListUni.ma <-fitList(m.psi1.pEff      ,
                        m.psiElev.pEff   ,
                        m.psiPrec.pEff   ,
                        mod.psiRoad.pEff ,
                        m.psiTempmax.pEff,
                        m.psiNDVI.pEff   ,
                        m.psiTempmin.pEff,
                        m.psiHFI.pEff    
)

# modSel compares AND ranks the models against eachother!
modSel(detListUni.ma)
