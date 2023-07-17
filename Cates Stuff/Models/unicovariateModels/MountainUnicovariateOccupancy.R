###------------------------------------------------------------------------------###
### Running Unicovariate models on Mountain tapir
### CA 12Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Mountain_Tapir_revised_DR.rds >> tapir occurance records
#> Effort_Mountain_Tapir_revised_DR.rds >> effort table
#> Mt_T_Covs4.csv >> covariate table


#clear system
rm(list=ls())
library(unmarked)

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")#Directory of R-project "Models" on github
dir()#

######Read in Tapir table and Effort###########
MT_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
MT_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/MT_covs.csv", comment.char = "#")



#####Model-Prep######################
#Double check that all modeling columns are scaled correctly 
MT_umf<- unmarkedFrameOccu(y=MT_tapir[,-1], siteCovs= as.data.frame(scale(MT_cov[,-c(1,2,3,4,5)])), obsCovs=list(Eff=MT_eff[,-1]))

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

# modSel compares AND ranks the models against each other!
modSel(MT_detlist) 


sink("unicovariateModselAll.txt", append = TRUE)
print("Mountain Tapir Model Selection")
modSel(MT_detlist)
sink()

#function for psi value
pf <- function(x) {
  occu <- 0
  if(length(x@estimates@estimates$state@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$state@estimates)) {
      occu <- (occu + plogis(x@estimates@estimates$state@estimates[i])) 
    }
    occu <- occu/(length(x@estimates@estimates$state@estimates)-1)
  } else {
    occu <- plogis(x@estimates@estimates$state@estimates[2])
  }
  print(paste("𝜓= ", signif(occu, digits = 4)))
}

# Function to give detection probabilities (p) for models 
pd <- function(x) {
  detp <- 0
  if(length(x@estimates@estimates$det@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$det@estimates)) {
      detp <- (detp + plogis(x@estimates@estimates$det@estimates[i])) 
    }
    detp <- detp/(length(x@estimates@estimates$det@estimates)-1)
  } else {
    detp <- plogis(x@estimates@estimates$det@estimates[2])
  }
  print(paste("p= ", signif(detp, digits=4)))
}

#function of both funcs
pfpd<- function(x){
  print(x@formula)
  pf(x)
  pd(x)
}
