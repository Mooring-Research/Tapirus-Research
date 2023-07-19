###------------------------------------------------------------------------------###
### Running Multivariate models on Mountain tapir
### CA 19Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Mountain_Tapir_revised_DR.rds >> tapir occurance records
#> Effort_Mountain_Tapir_revised_DR.rds >> effort table
#> Mt_T_Covs4.csv NOW> MT_covs.csv (17 jul 2023) >> covariate table

#clear system
rm(list=ls())

library(unmarked)
#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Cates Stuff/Models/modelsOutputToFile_function.R")

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
#use C:/Users/chris/Tapirus-Research/Cates Stuff/Models/findCovCombinations.R to find combinations of covariates
MT_m.psi1.pEff						        <- occu(~Eff~ 1, MT_umf)  #effnull
#MT_m.psiHFI.pEff 					        <- occu(~Eff~ HFI, MT_umf)
MT_m.psiElev.pEff				         	<- occu(~Eff~ Elev, MT_umf)
MT_m.psiPrec.pEff				        	<- occu(~Eff~ Precip, MT_umf)
MT_m.psiRoad.pEff			        	  <- occu(~Eff~ d.Road, MT_umf)
MT_m.psiTempmin.pEff			      	<- occu(~Eff~ AvgMinTemp, MT_umf)
MT_m.psiTempMTx.pEff			      	<- occu(~Eff~ AvgMaxTemp, MT_umf) 
MT_m.psiNDVI.pEff				        	<- occu(~Eff~ NDVI, MT_umf) 
MT_m.psiNPP.pEff                  <- occu(~Eff~ NPP, MT_umf)#<^SINGLE

MT_m.psi2.pEff					        	<- occu(~Eff~ 1+1, MT_umf) #2 effnull
MT_m.psiElevAvgMaxTemp.pEff			  <- occu(~Eff~ Elev + AvgMaxTemp, MT_umf)	
MT_m.psiElevPrecip.pEff				    <- occu(~Eff~ Elev + Precip, MT_umf)	
MT_m.psiElevRoad.pEff				      <- occu(~Eff~ Elev + d.Road, MT_umf)	
MT_m.psiElevNDVI.pEff				      <- occu(~Eff~ Elev + NDVI, MT_umf)	
MT_m.psiAvgMaxTempPrecip.pEff		  <- occu(~Eff~ AvgMaxTemp + Precip, MT_umf)	
MT_m.psiAvgMaxTempRoad.pEff		  	<- occu(~Eff~ AvgMaxTemp + d.Road, MT_umf)	
MT_m.psiAvgMaxTempNDVI.pEff			  <- occu(~Eff~ AvgMaxTemp + NDVI, MT_umf)	
MT_m.psiAvgMinTempPrecip.pEff	    <- occu(~Eff~ Precip + AvgMinTemp, MT_umf)
MT_m.psiAvgMinTempElev.pEff	      <- occu(~Eff~ AvgMinTemp + Elev, MT_umf)
#MT_m.psiAvgMinTempHFI.pEff	      <- occu(~Eff~ AvgMinTemp + HFI, MT_umf)
MT_m.psiAvgMinTempRoad.pEff	      <- occu(~Eff~ AvgMinTemp + d.Road, MT_umf)
MT_m.psiAvgMinTempNDVI.pEff	      <- occu(~Eff~ AvgMinTemp + NDVI, MT_umf)
MT_m.psiAvgMinTempAvgMaxTemp.pEff	<- occu(~Eff~ AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.psiAvgMinTempNPP.pEff	      <- occu(~Eff~ AvgMinTemp + NPP, MT_umf)
MT_m.psiPrecipRoad.pEff				    <- occu(~Eff~ Precip + d.Road, MT_umf)	
MT_m.psiPrecipNDVI.pEff				    <- occu(~Eff~ Precip + NDVI, MT_umf)	
MT_m.psiNDVIRoad.pEff				      <- occu(~Eff~ NDVI + d.Road, MT_umf)
# MT_m.psiHFIPrecip.pEff	          <- occu(~Eff~ HFI + Precip, MT_umf)
# MT_m.psiHFIElev.pEff	            <- occu(~Eff~ HFI + Elev, MT_umf)
# MT_m.psiHFIRoad.pEff	            <- occu(~Eff~ HFI + d.Road, MT_umf)
# MT_m.psiHFINDVI.pEff	            <- occu(~Eff~ HFI + NDVI, MT_umf)
# MT_m.psiHFIAvgMaxTemp.pEff	      <- occu(~Eff~ HFI + AvgMaxTemp, MT_umf)
# MT_m.psiHFIAvgMinTemp.pEff	      <- occu(~Eff~ HFI + AvgMinTemp, MT_umf)
# MT_m.psiHFINPP.pEff	              <- occu(~Eff~ HFI + NPP, MT_umf)#<^DOUBLE	

MT_m.psi3.pEff 					  		          <- occu(~Eff~ 1+1+1, MT_umf) #3 effnull
# MT_m.PrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, MT_umf)	
MT_m.PrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, MT_umf)
MT_m.PrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, MT_umf)
MT_m.PrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, MT_umf)
MT_m.PrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, MT_umf) 
MT_m.PrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, MT_umf)
# MT_m.PrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, MT_umf)
# MT_m.PrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, MT_umf)
# MT_m.PrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, MT_umf)
# MT_m.PrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, MT_umf)
# MT_m.PrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, MT_umf)
MT_m.PrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, MT_umf)
MT_m.PrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, MT_umf)
MT_m.PrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, MT_umf)
MT_m.PrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, MT_umf)
MT_m.PrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, MT_umf)
MT_m.PrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, MT_umf)
MT_m.PrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, MT_umf)
MT_m.PrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.PrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, MT_umf)
MT_m.PrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, MT_umf)

# MT_m.ElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, MT_umf)
# MT_m.ElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, MT_umf)
# MT_m.ElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, MT_umf)
# MT_m.ElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, MT_umf)
# MT_m.ElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, MT_umf)
MT_m.ElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, MT_umf)
MT_m.ElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, MT_umf)
MT_m.ElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, MT_umf)
MT_m.ElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, MT_umf)
MT_m.ElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, MT_umf)
MT_m.ElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, MT_umf)
MT_m.ElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, MT_umf)
MT_m.ElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , MT_umf)
MT_m.ElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , MT_umf)
MT_m.ElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, MT_umf)

# MT_m.HFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, MT_umf)
# MT_m.HFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MT_umf)
# MT_m.HFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, MT_umf)
# MT_m.HFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, MT_umf)
# MT_m.HFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MT_umf)
# MT_m.HFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, MT_umf)
# MT_m.HFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, MT_umf)
# MT_m.HFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, MT_umf)
# MT_m.HFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, MT_umf)
# MT_m.HFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, MT_umf)

MT_m.RoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, MT_umf)
MT_m.RoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, MT_umf)
MT_m.RoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, MT_umf)
MT_m.RoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.RoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, MT_umf)
MT_m.RoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, MT_umf)

MT_m.NDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.NDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, MT_umf)
MT_m.NDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, MT_umf)

MT_m.AvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, MT_umf)#<^TRIPLE



######Model list#####
MT_detlist<- fitList(
  MT_m.psi1.pEff						,
  #MT_m.psiHFI.pEff 					,
  MT_m.psiElev.pEff				    ,
  MT_m.psiPrec.pEff				    ,
  MT_m.psiRoad.pEff			        ,
  MT_m.psiTempmin.pEff			    ,
  MT_m.psiTempMTx.pEff			    ,
  MT_m.psiNDVI.pEff				    ,
  MT_m.psiNPP.pEff                 	,
  MT_m.psi2.pEff					    ,
  MT_m.psiElevAvgMaxTemp.pEff			,
  MT_m.psiElevPrecip.pEff				,
  MT_m.psiElevRoad.pEff				,
  MT_m.psiElevNDVI.pEff				,
  MT_m.psiAvgMaxTempPrecip.pEff		,
  MT_m.psiAvgMaxTempRoad.pEff		  	,
  MT_m.psiAvgMaxTempNDVI.pEff			,
  MT_m.psiAvgMinTempPrecip.pEff	    ,
  MT_m.psiAvgMinTempElev.pEff	      	,
 # MT_m.psiAvgMinTempHFI.pEff	      	,
  MT_m.psiAvgMinTempRoad.pEff	      	,
  MT_m.psiAvgMinTempNDVI.pEff	      	,
  MT_m.psiAvgMinTempAvgMaxTemp.pEff	,
  MT_m.psiAvgMinTempNPP.pEff	     	,
  MT_m.psiPrecipRoad.pEff				,
  MT_m.psiPrecipNDVI.pEff				,
  MT_m.psiNDVIRoad.pEff				,
  # MT_m.psiHFIPrecip.pEff	         	,
  # MT_m.psiHFIElev.pEff	         	,
  # MT_m.psiHFIRoad.pEff	         	,
  # MT_m.psiHFINDVI.pEff	         	,
  # MT_m.psiHFIAvgMaxTemp.pEff	     	,
  # MT_m.psiHFIAvgMinTemp.pEff	     	,
  # MT_m.psiHFINPP.pEff	             	,
  MT_m.psi3.pEff 					  	,
  #MT_m.PrecipElevHFI.pEff           	,
  MT_m.PrecipElevRoad.pEff          	,
  MT_m.PrecipElevNDVI.pEff          	,
  MT_m.PrecipElevAvgMinTemp.pEff    	,
  MT_m.PrecipElevAvgMaxTemp.pEff    	,
  MT_m.PrecipElevNPP.pEff           	,
  #MT_m.PrecipHFIRoad.pEff             ,
  # MT_m.PrecipHFINDVI.pEff             ,
  # MT_m.PrecipHFIAvgMinTemp.pEff       ,
  # MT_m.PrecipHFIAvgMaxTemp.pEff       ,
  # MT_m.PrecipHFINPP.pEff              ,
  MT_m.PrecipRoadNDVI.pEff            ,
  MT_m.PrecipRoadAvgMinTemp.pEff      ,
  MT_m.PrecipRoadAvgMaxTemp.pEff      ,
  MT_m.PrecipRoadNPP.pEff             ,
  MT_m.PrecipNDVIAvgMinTemp.pEff      ,
  MT_m.PrecipNDVIAvgMaxTemp.pEff      ,
  MT_m.PrecipNDVINPP.pEff             ,
  MT_m.PrecipAvgMinTempAvgMaxTemp.pEff,
  MT_m.PrecipAvgMinTempNPP.pEff       ,
  MT_m.PrecipAvgMaxTempNPP.pEff       ,
  # MT_m.ElevHFIRoad.pEff               ,
  # MT_m.ElevHFINDVI.pEff               ,
  # MT_m.ElevHFIAvgMinTemp.pEff         ,
  # MT_m.ElevHFIAvgMaxTemp.pEff         ,
  # MT_m.ElevHFINPP.pEff                ,
  MT_m.ElevRoadNDVI.pEff              ,
  MT_m.ElevRoadAvgMinTemp.pEff        ,
  MT_m.ElevRoadAvgMaxTemp.pEff        ,
  MT_m.ElevRoadNPP.pEff               ,
  MT_m.ElevNDVIAvgMinTemp.pEff        ,
  MT_m.ElevNDVIAvgMaxTemp.pEff        ,
  MT_m.ElevNDVINPP.pEff               ,
  MT_m.ElevAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.ElevAvgMinTempNPP.pEff         ,
  MT_m.ElevAvgMaxTempNPP.pEff         ,
  # MT_m.HFIRoadNDVI.pEff               ,
  # MT_m.HFIRoadAvgMinTemp.pEff         ,
  # MT_m.HFIRoadAvgMaxTemp.pEff         ,
  # MT_m.HFIRoadNPP.pEff                ,
  # MT_m.HFINDVIAvgMinTemp.pEff         ,
  # MT_m.HFINDVIAvgMaxTemp.pEff         ,
  # MT_m.HFINDVINPP.pEff                ,
  # MT_m.HFIAvgMinTempAvgMaxTemp.pEff   ,
  # MT_m.HFIAvgMinTempNPP.pEff          ,
  # MT_m.HFIAvgMaxTempNPP.pEff          ,
  MT_m.RoadNDVIAvgMinTemp.pEff        ,
  MT_m.RoadNDVIAvgMaxTemp.pEff        ,
  MT_m.RoadNDVINPP.pEff               ,
  MT_m.RoadAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.RoadAvgMinTempNPP.pEff         ,
  MT_m.RoadAvgMaxTempNPP.pEff         ,
  MT_m.NDVIAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.NDVIAvgMinTempNPP.pEff         ,
  MT_m.NDVIAvgMaxTempNPP.pEff         ,
  MT_m.AvgMinTempAvgMaxTempNPP.pEff    
  
)

#####Output####
columns<- c(6:13) #"Precip"  "Elev" "HFI" "d.Road"  "NDVI" "AvgMinTemp" "AvgMaxTemp" "NPP"       
sink("MT_multiMods.txt", append = FALSE)

cat("19, July 2023\n")
cat("\n***Correlation Matrix***\n")
cor(MT_cov[, columns]) #correlation MTtrix on numerical fields
print("**Mountain Tapir Models**")
cat()
modSel(MT_detlist) #model selection table
cat("\n**Summaries**\n")
getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()
