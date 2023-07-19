###------------------------------------------------------------------------------###
### Running Unicovariate models on Mountain tapir
### CA 19Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Mountain_Tapir_revised_DR.rds >> tapir occurance records
#> Effort_Mountain_Tapir_revised_DR.rds >> effort table
#> Mt_T_Covs4.csv now MT_covs.csv (17 jul 2023) >> covariate table

#clear system
rm(list=ls())

library(unmarked)

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")#Directory of R-project "Models" on github

######Read in Tapir table and Effort###########
MT_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
MT_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
#MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")
MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/MT_covs.csv", comment.char = "#")

#####Model-Prep######################
#Double check that all modeling columns are scaled correctly 
MT_umf<- unmarkedFrameOccu(y=MT_tapir[,-1], siteCovs= as.data.frame(scale(MT_cov[,-c(1:5)])), obsCovs=list(Eff=MT_eff[,-1]))

######Running Models!####################################
#Running Null model
MT_mod0 							          	<- occu(~1~	  1, MT_umf)  # Null Model
MT_m.psi1.pEff							      <- occu(~Eff~ 1,   MT_umf)  # Eff Model
MT_m.pEff.psiElev						    <- occu(~Eff~ Elev, MT_umf)
MT_m.pEff.psiPrec					    	<- occu(~Eff~ Precip, MT_umf)
MT_mod.eff.road 					    	  <- occu(~Eff~ d.Road, MT_umf)
MT_m.psiTempmin.pEff				    	<- occu(~Eff~ AvgMinTemp, MT_umf)
MT_m.psiTempmax.pEff					    <- occu(~Eff~ AvgMaxTemp, MT_umf) 
MT_m.psiNDVI.pEff					    	<- occu(~Eff~ NDVI, MT_umf) 					        #<^single
MT_m.psiElevNDVI.pEff					  <- occu(~Eff~ Elev + NDVI, MT_umf)	
MT_m.psiElevMaxTemp.pEff				  <- occu(~Eff~ Elev + AvgMaxTemp, MT_umf)	
MT_m.psiNDVIMaxTemp.pEff				  <- occu(~Eff~ NDVI + AvgMaxTemp, MT_umf)	
MT_m.psiElevPrecip.pEff				  <- occu(~Eff~ Elev + Precip, MT_umf)	
MT_m.psiElevRoad.pEff					  <- occu(~Eff~ Elev + d.Road, MT_umf)	
MT_m.psiAvgMaxTempPrecip.pEff		<- occu(~Eff~ AvgMaxTemp + Precip, MT_umf)	
MT_m.psiAvgMaxTempRoad.pEff			<- occu(~Eff~ AvgMaxTemp + d.Road, MT_umf)	
MT_m.psiPrecipRoad.pEff				  <- occu(~Eff~ Precip + d.Road, MT_umf)	
MT_m.psiPrecipNDVI.pEff				  <- occu(~Eff~ Precip + NDVI, MT_umf)	
MT_m.psiNDVIRoad.pEff					  <- occu(~Eff~ NDVI + d.Road, MT_umf)			    #<^double
MT_m.psiElevTempmaxPrecip.pEff		<- occu(~Eff~ Elev + AvgMaxTemp + Precip, MT_umf)	
MT_m.psiElevTempmaxRoad.pEff			<- occu(~Eff~ Elev + AvgMaxTemp + d.Road, MT_umf)	
MT_m.psiElevTempmaxNDVI.pEff			<- occu(~Eff~ Elev + AvgMaxTemp + NDVI, MT_umf)	
MT_m.psiTempmaxPrecipRoad.pEff		<- occu(~Eff~ AvgMaxTemp + Precip + d.Road, MT_umf)	
MT_m.psiAvgMaxTempPrecipNDVI.pEff<- occu(~Eff~ AvgMaxTemp + Precip + NDVI, MT_umf)	
MT_m.psiAvgMaxTempNDVIRoad.pEff	 <- occu(~Eff~ AvgMaxTemp + NDVI + d.Road, MT_umf)	
MT_m.psiPrecipRoadNDVI.pEff			<- occu(~Eff~ Precip + d.Road + NDVI, MT_umf)	
MT_m.psiPrecipElevNDVI.pEff			<- occu(~Eff~ Precip + Elev + NDVI, MT_umf)	
MT_m.psiPrecipElevRoad.pEff			<- occu(~Eff~ Precip + Elev + d.Road, MT_umf)	
MT_m.psiRoadNDVIElev.pEff		 		<- occu(~Eff~ d.Road + NDVI + Elev, MT_umf)	#<^triple

#m.psiTempElevNDVIRoad.pEff 		<- occu(~Eff~ AvgMaxTemp + Elev + NDVI + d.Road, umf)	
#m.psiElevNDVIPrecipRoad.pEff 		<- occu(~Eff~ Elev + NDVI + Precip + d.Road, umf)	
#m.psiTempElevRoadPrecip.pEff 		<- occu(~Eff~ AvgMaxTemp + Elev + d.Road + Precip, umf)	
#m.psiTempNDVIRoadPrecip.pEff 		<- occu(~Eff~ AvgMaxTemp + NDVI + d.Road + Precip, umf)	
#m.psiTempElevNDVIPrecip.pEff 		<- occu(~Eff~ AvgMaxTemp + Elev + NDVI + Precip, umf)	#<^quadruple
#m.psiTempElevNDVIRoadPrecip.pEff	<- occu(~Eff~ AvgMaxTemp + Elev + NDVI + d.Road + Precip, umf)	#quintuple

##collect in fitList ---> 2 variables 
detList2<-fitList(MT_mod0 					,	
                  MT_m.psi1.pEff			,		
                  MT_m.pEff.psiElev			,	
                  MT_m.pEff.psiPrec				,
                  MT_mod.eff.road 				,
                  MT_m.psiTempmin.pEff			,
                  MT_m.psiTempmax.pEff			,
                  MT_m.psiNDVI.pEff				,
                  MT_m.psiElevNDVI.pEff		,	
                  MT_m.psiElevMaxTemp.pEff,		
                  MT_m.psiNDVIMaxTemp.pEff,		
                  MT_m.psiElevPrecip.pEff	,		
                  MT_m.psiElevRoad.pEff		,	
                  MT_m.psiAvgMaxTempPrecip.pEff	,
                  MT_m.psiAvgMaxTempRoad.pEff		,
                  MT_m.psiPrecipRoad.pEff			,
                  MT_m.psiPrecipNDVI.pEff			,
                  MT_m.psiNDVIRoad.pEff			,
                  MT_m.psiElevTempmaxPrecip.pEff	,
                  MT_m.psiElevTempmaxRoad.pEff	,
                  MT_m.psiElevTempmaxNDVI.pEff	,
                  MT_m.psiTempmaxPrecipRoad.pEff	,
                  MT_m.psiAvgMaxTempPrecipNDVI.pEff,
                  MT_m.psiAvgMaxTempNDVIRoad.pEff	 ,
                  MT_m.psiPrecipRoadNDVI.pEff		,
                  MT_m.psiPrecipElevNDVI.pEff		,
                  MT_m.psiPrecipElevRoad.pEff		,
                  MT_m.psiRoadNDVIElev.pEff		 
)

##do AIC model selection
modSel(detList2) 

