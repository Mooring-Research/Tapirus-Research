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
source("C:/Users/chris/Tapirus-Research/Christian/Recent Models/modelsOutputToFile_function.R")

#set wd
setwd("C:/Users/chris/Tapirus-Research/Christian/Recent Models")

######Read in Tapir table and Effort###########
MT_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
MT_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
#MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")
MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/MT_covs_new.csv", comment.char = "#")

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
MT_m.psiAvgMinTemp.pEff			      	<- occu(~Eff~ AvgMinTemp, MT_umf)
MT_m.psiAvgMaxTemp.pEff			      	<- occu(~Eff~ AvgMaxTemp, MT_umf) 
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
MT_m.psiPrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, MT_umf)
MT_m.psiPrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, MT_umf)
MT_m.psiPrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, MT_umf)
MT_m.psiPrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, MT_umf) 
MT_m.psiPrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, MT_umf)
# MT_m.psiPrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, MT_umf)
# MT_m.psiPrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, MT_umf)
# MT_m.psiPrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, MT_umf)
# MT_m.psiPrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, MT_umf)
# MT_m.psiPrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, MT_umf)
MT_m.psiPrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, MT_umf)
MT_m.psiPrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, MT_umf)
MT_m.psiPrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, MT_umf)
MT_m.psiPrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, MT_umf)
MT_m.psiPrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, MT_umf)
MT_m.psiPrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, MT_umf)
MT_m.psiPrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, MT_umf)
MT_m.psiPrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.psiPrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, MT_umf)
MT_m.psiPrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, MT_umf)

# MT_m.psiElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, MT_umf)
# MT_m.psiElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, MT_umf)
# MT_m.psiElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, MT_umf)
# MT_m.psiElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, MT_umf)
# MT_m.psiElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, MT_umf)
MT_m.psiElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, MT_umf)
MT_m.psiElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, MT_umf)
MT_m.psiElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, MT_umf)
MT_m.psiElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, MT_umf)
MT_m.psiElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, MT_umf)
MT_m.psiElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, MT_umf)
MT_m.psiElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, MT_umf)
MT_m.psiElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , MT_umf)
MT_m.psiElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , MT_umf)
MT_m.psiElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, MT_umf)

# MT_m.psiHFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, MT_umf)
# MT_m.psiHFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MT_umf)
# MT_m.psiHFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, MT_umf)
# MT_m.psiHFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, MT_umf)
# MT_m.psiHFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MT_umf)
# MT_m.psiHFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, MT_umf)
# MT_m.psiHFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, MT_umf)
# MT_m.psiHFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, MT_umf)
# MT_m.psiHFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, MT_umf)
# MT_m.psiHFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, MT_umf)

MT_m.psiRoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, MT_umf)
MT_m.psiRoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, MT_umf)
MT_m.psiRoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, MT_umf)
MT_m.psiRoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.psiRoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, MT_umf)
MT_m.psiRoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, MT_umf)

MT_m.psiNDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, MT_umf)
MT_m.psiNDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, MT_umf)
MT_m.psiNDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, MT_umf)

MT_m.psiAvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, MT_umf)#<^TRIPLE



######Model list#####
MT_detlist<- fitList(
  MT_m.psi1.pEff						,
  #MT_m.psiHFI.pEff 					,
  MT_m.psiElev.pEff				    ,
  MT_m.psiPrec.pEff				    ,
  MT_m.psiRoad.pEff			        ,
  MT_m.psiAvgMinTemp.pEff			    ,
  MT_m.psiAvgMaxTemp.pEff			    ,
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
  #MT_m.psiPrecipElevHFI.pEff           	,
  MT_m.psiPrecipElevRoad.pEff          	,
  MT_m.psiPrecipElevNDVI.pEff          	,
  MT_m.psiPrecipElevAvgMinTemp.pEff    	,
  MT_m.psiPrecipElevAvgMaxTemp.pEff    	,
  MT_m.psiPrecipElevNPP.pEff           	,
  #MT_m.psiPrecipHFIRoad.pEff             ,
  # MT_m.psiPrecipHFINDVI.pEff             ,
  # MT_m.psiPrecipHFIAvgMinTemp.pEff       ,
  # MT_m.psiPrecipHFIAvgMaxTemp.pEff       ,
  # MT_m.psiPrecipHFINPP.pEff              ,
  MT_m.psiPrecipRoadNDVI.pEff            ,
  MT_m.psiPrecipRoadAvgMinTemp.pEff      ,
  MT_m.psiPrecipRoadAvgMaxTemp.pEff      ,
  MT_m.psiPrecipRoadNPP.pEff             ,
  MT_m.psiPrecipNDVIAvgMinTemp.pEff      ,
  MT_m.psiPrecipNDVIAvgMaxTemp.pEff      ,
  MT_m.psiPrecipNDVINPP.pEff             ,
  MT_m.psiPrecipAvgMinTempAvgMaxTemp.pEff,
  MT_m.psiPrecipAvgMinTempNPP.pEff       ,
  MT_m.psiPrecipAvgMaxTempNPP.pEff       ,
  # MT_m.psiElevHFIRoad.pEff               ,
  # MT_m.psiElevHFINDVI.pEff               ,
  # MT_m.psiElevHFIAvgMinTemp.pEff         ,
  # MT_m.psiElevHFIAvgMaxTemp.pEff         ,
  # MT_m.psiElevHFINPP.pEff                ,
  MT_m.psiElevRoadNDVI.pEff              ,
  MT_m.psiElevRoadAvgMinTemp.pEff        ,
  MT_m.psiElevRoadAvgMaxTemp.pEff        ,
  MT_m.psiElevRoadNPP.pEff               ,
  MT_m.psiElevNDVIAvgMinTemp.pEff        ,
  MT_m.psiElevNDVIAvgMaxTemp.pEff        ,
  MT_m.psiElevNDVINPP.pEff               ,
  MT_m.psiElevAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.psiElevAvgMinTempNPP.pEff         ,
  MT_m.psiElevAvgMaxTempNPP.pEff         ,
  # MT_m.psiHFIRoadNDVI.pEff               ,
  # MT_m.psiHFIRoadAvgMinTemp.pEff         ,
  # MT_m.psiHFIRoadAvgMaxTemp.pEff         ,
  # MT_m.psiHFIRoadNPP.pEff                ,
  # MT_m.psiHFINDVIAvgMinTemp.pEff         ,
  # MT_m.psiHFINDVIAvgMaxTemp.pEff         ,
  # MT_m.psiHFINDVINPP.pEff                ,
  # MT_m.psiHFIAvgMinTempAvgMaxTemp.pEff   ,
  # MT_m.psiHFIAvgMinTempNPP.pEff          ,
  # MT_m.psiHFIAvgMaxTempNPP.pEff          ,
  MT_m.psiRoadNDVIAvgMinTemp.pEff        ,
  MT_m.psiRoadNDVIAvgMaxTemp.pEff        ,
  MT_m.psiRoadNDVINPP.pEff               ,
  MT_m.psiRoadAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.psiRoadAvgMinTempNPP.pEff         ,
  MT_m.psiRoadAvgMaxTempNPP.pEff         ,
  MT_m.psiNDVIAvgMinTempAvgMaxTemp.pEff  ,
  MT_m.psiNDVIAvgMinTempNPP.pEff         ,
  MT_m.psiNDVIAvgMaxTempNPP.pEff         ,
  MT_m.psiAvgMinTempAvgMaxTempNPP.pEff    
  
)

#####Output####
columns<- c(6:13) #"Precip"  "Elev" "HFI" "d.Road"  "NDVI" "AvgMinTemp" "AvgMaxTemp" "NPP"       
sink("MT_multiMods2.txt", append = FALSE)

cat("20, July 2023\n")
cat("\n***Correlation Matrix***\n")
cor(MT_cov[, columns]) #correlation MTtrix on numerical fields
print("**Mountain Tapir Models**")
cat()
modSel(MT_detlist) #model selection table
cat("\n**Summaries**\n")
getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()

