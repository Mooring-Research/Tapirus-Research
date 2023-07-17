
require(AICcmodavg)
require(unmarked)

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")#Directory of R-project "Models" on github
dir()#

######Read in Tapir table and Effort###########
MT_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
MT_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")



#####Model-Prep######################

MT_umf<- unmarkedFrameOccu(y=MT_tapir[,-1], siteCovs= as.data.frame(scale(MT_cov[,-c(1,2,3,4)])), obsCovs=list(Eff=MT_eff[,-1]))

######Running Models!####################################
#Running Null model
MT_mod0              <- occu(~1~1, MT_umf)  # Null Model
MT_m.psi1.pEff       <- occu(~Eff~1, MT_umf)  # Eff Model
MT_m.psiElev.pEff    <- occu(~Eff ~Elev, MT_umf)
MT_m.psiPrec.pEff    <- occu(~Eff ~Precip, MT_umf)
MT_m.psiRoad.pEff    <- occu(~Eff ~d.Road, MT_umf)
MT_m.psiTempmax.pEff <- occu(~Eff~ AvgMaxTemp, MT_umf) 
MT_m.psiNDVI.pEff    <- occu(~Eff~ NDVI, MT_umf) 
MT_m.psiTempmin.pEff <- occu(~Eff~ AvgMinTemp, MT_umf)
MT_m.psiElev.p1      <- occu(~1 ~Elev, MT_umf)
MT_m.psiElevTempmax.pEff<- occu(~Eff~ Elev + AvgMaxTemp, MT_umf)
MT_m.psiElevRoad.pEff<- occu(~Eff~ Elev + d.Road, MT_umf)
##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other

MT_detlist<-fitList(MT_mod0              ,
                    MT_m.psi1.pEff       ,
                    MT_m.psiElev.pEff    ,
                    MT_m.psiPrec.pEff    ,
                    MT_m.psiRoad.pEff    ,
                    MT_m.psiTempmax.pEff ,
                    MT_m.psiNDVI.pEff    ,
                    MT_m.psiTempmin.pEff ,
                    MT_m.psiElev.p1       
)
MT_mod_list<- list(
                MT_mod0              ,
                MT_m.psi1.pEff       ,
                MT_m.psiElev.pEff    ,
                MT_m.psiPrec.pEff    ,
                MT_m.psiRoad.pEff    ,
                MT_m.psiTempmax.pEff ,
                MT_m.psiNDVI.pEff    ,
                MT_m.psiTempmin.pEff ,
                MT_m.psiElev.p1       
                )

MT_mod_names<- c("MT_mod0"              ,
              "MT_m.psi1.pEff"       ,
              "MT_m.psiElev.pEff"    ,
              "MT_m.psiPrec.pEff"    ,
              "MT_m.psiRoad.pEff"    ,
              "MT_m.psiTempmax.pEff" ,
              "MT_m.psiNDVI.pEff"    ,
              "MT_m.psiTempmin.pEff" ,
              "MT_m.psiElev.p1" )


aictab(cand.set = mod_list, modnames = mod_names)
modSel(MT_detlist)
