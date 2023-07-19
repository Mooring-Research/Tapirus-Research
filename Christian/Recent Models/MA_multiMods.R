###------------------------------------------------------------------------------###
### Running multivariate models on Malayan tapir
### CA 19Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Malayan_Tapir.rds >> tapir occurance records
#> Effort_Malayan_Tapir.rds >> effort table
#> Ma_T_Final_Covs.csv NOW> MA_covs.csv 17jul2023>> covariate table


rm(list=ls())
library(unmarked)

#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Cates Stuff/Models/modelsOutputToFile_function.R")

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")##Directory of R-project "Models" on github
#dir()


######Read in Tapir table and Effort###########
MA_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Collapsed_Capture_Malayan_Tapir.rds")

MA_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Data Processing/Effort_Malayan_Tapir.rds")


#MA_cv<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Ma_T_Final_Covs.csv")
MA_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/MA_covs.csv")

#unmarked frame
MA_umf<- unmarkedFrameOccu(y=MA_tapir[,-1], siteCovs= as.data.frame(scale(MA_cov[,-c(1:5)])), obsCovs=list(Eff=MA_eff[,-1]))



######Running Models####################################
#use C:/Users/chris/Tapirus-Research/Cates Stuff/Models/findCovCombinations.R to find combinations of covariates
MA_m.psi1.pEff						        <- occu(~Eff~ 1, MA_umf)  #effnull
MA_m.psiHFI.pEff 					        <- occu(~Eff~ HFI, MA_umf)
MA_m.psiElev.pEff				         	<- occu(~Eff~ Elev, MA_umf)
MA_m.psiPrec.pEff				        	<- occu(~Eff~ Precip, MA_umf)
MA_m.psiRoad.pEff			        	  <- occu(~Eff~ d.Road, MA_umf)
MA_m.psiTempmin.pEff			      	<- occu(~Eff~ AvgMinTemp, MA_umf)
MA_m.psiTempmax.pEff			      	<- occu(~Eff~ AvgMaxTemp, MA_umf) 
MA_m.psiNDVI.pEff				        	<- occu(~Eff~ NDVI, MA_umf) 
MA_m.psiNPP.pEff                  <- occu(~Eff~ NPP, MA_umf)#<^SINGLE

MA_m.psi2.pEff					        	<- occu(~Eff~ 1+1, MA_umf) #2 effnull
MA_m.psiElevAvgMaxTemp.pEff			  <- occu(~Eff~ Elev + AvgMaxTemp, MA_umf)	
MA_m.psiElevPrecip.pEff				    <- occu(~Eff~ Elev + Precip, MA_umf)	
MA_m.psiElevRoad.pEff				      <- occu(~Eff~ Elev + d.Road, MA_umf)	
MA_m.psiElevNDVI.pEff				      <- occu(~Eff~ Elev + NDVI, MA_umf)	
MA_m.psiAvgMaxTempPrecip.pEff		  <- occu(~Eff~ AvgMaxTemp + Precip, MA_umf)	
MA_m.psiAvgMaxTempRoad.pEff		  	<- occu(~Eff~ AvgMaxTemp + d.Road, MA_umf)	
MA_m.psiAvgMaxTempNDVI.pEff			  <- occu(~Eff~ AvgMaxTemp + NDVI, MA_umf)	
MA_m.psiAvgMinTempPrecip.pEff	    <- occu(~Eff~ Precip + AvgMinTemp, MA_umf)
MA_m.psiAvgMinTempElev.pEff	      <- occu(~Eff~ AvgMinTemp + Elev, MA_umf)
MA_m.psiAvgMinTempHFI.pEff	      <- occu(~Eff~ AvgMinTemp + HFI, MA_umf)
MA_m.psiAvgMinTempRoad.pEff	      <- occu(~Eff~ AvgMinTemp + d.Road, MA_umf)
MA_m.psiAvgMinTempNDVI.pEff	      <- occu(~Eff~ AvgMinTemp + NDVI, MA_umf)
MA_m.psiAvgMinTempAvgMaxTemp.pEff	<- occu(~Eff~ AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.psiAvgMinTempNPP.pEff	      <- occu(~Eff~ AvgMinTemp + NPP, MA_umf)
MA_m.psiPrecipRoad.pEff				    <- occu(~Eff~ Precip + d.Road, MA_umf)	
MA_m.psiPrecipNDVI.pEff				    <- occu(~Eff~ Precip + NDVI, MA_umf)	
MA_m.psiNDVIRoad.pEff				      <- occu(~Eff~ NDVI + d.Road, MA_umf)
MA_m.psiHFIPrecip.pEff	          <- occu(~Eff~ HFI + Precip, MA_umf)
MA_m.psiHFIElev.pEff	            <- occu(~Eff~ HFI + Elev, MA_umf)
MA_m.psiHFIRoad.pEff	            <- occu(~Eff~ HFI + d.Road, MA_umf)
MA_m.psiHFINDVI.pEff	            <- occu(~Eff~ HFI + NDVI, MA_umf)
MA_m.psiHFIAvgMaxTemp.pEff	      <- occu(~Eff~ HFI + AvgMaxTemp, MA_umf)
MA_m.psiHFIAvgMinTemp.pEff	      <- occu(~Eff~ HFI + AvgMinTemp, MA_umf)
MA_m.psiHFINPP.pEff	              <- occu(~Eff~ HFI + NPP, MA_umf)#<^DOUBLE	

MA_m.psi3.pEff 					  		          <- occu(~Eff~ 1+1+1, MA_umf) #3 effnull
MA_m.PrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, MA_umf)	
MA_m.PrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, MA_umf)
MA_m.PrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, MA_umf)
MA_m.PrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, MA_umf)
MA_m.PrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, MA_umf) 
MA_m.PrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, MA_umf)
MA_m.PrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, MA_umf)
MA_m.PrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, MA_umf)
MA_m.PrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, MA_umf)
MA_m.PrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, MA_umf)
MA_m.PrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, MA_umf)
MA_m.PrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, MA_umf)
MA_m.PrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, MA_umf)
MA_m.PrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, MA_umf)
MA_m.PrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, MA_umf)
MA_m.PrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, MA_umf)
MA_m.PrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, MA_umf)
MA_m.PrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, MA_umf)
MA_m.PrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.PrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, MA_umf)
MA_m.PrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, MA_umf)

MA_m.ElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, MA_umf)
MA_m.ElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, MA_umf)
MA_m.ElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, MA_umf)
MA_m.ElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, MA_umf)
MA_m.ElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, MA_umf)
MA_m.ElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, MA_umf)
MA_m.ElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, MA_umf)
MA_m.ElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, MA_umf)
MA_m.ElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, MA_umf)
MA_m.ElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, MA_umf)
MA_m.ElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, MA_umf)
MA_m.ElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, MA_umf)
MA_m.ElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , MA_umf)
MA_m.ElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , MA_umf)
MA_m.ElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, MA_umf)

MA_m.HFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, MA_umf)
MA_m.HFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MA_umf)
MA_m.HFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, MA_umf)
MA_m.HFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, MA_umf)
MA_m.HFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MA_umf)
MA_m.HFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, MA_umf)
MA_m.HFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, MA_umf)
MA_m.HFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.HFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, MA_umf)
MA_m.HFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, MA_umf)

MA_m.RoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, MA_umf)
MA_m.RoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, MA_umf)
MA_m.RoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, MA_umf)
MA_m.RoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.RoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, MA_umf)
MA_m.RoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, MA_umf)

MA_m.NDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.NDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, MA_umf)
MA_m.NDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, MA_umf)

MA_m.AvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, MA_umf)#<^TRIPLE



######Model list#####
MA_detlist<- fitList(
  MA_m.psi1.pEff						,
  MA_m.psiHFI.pEff 					,
  MA_m.psiElev.pEff				    ,
  MA_m.psiPrec.pEff				    ,
  MA_m.psiRoad.pEff			        ,
  MA_m.psiTempmin.pEff			    ,
  MA_m.psiTempmax.pEff			    ,
  MA_m.psiNDVI.pEff				    ,
  MA_m.psiNPP.pEff                 	,
  MA_m.psi2.pEff					    ,
  MA_m.psiElevAvgMaxTemp.pEff			,
  MA_m.psiElevPrecip.pEff				,
  MA_m.psiElevRoad.pEff				,
  MA_m.psiElevNDVI.pEff				,
  MA_m.psiAvgMaxTempPrecip.pEff		,
  MA_m.psiAvgMaxTempRoad.pEff		  	,
  MA_m.psiAvgMaxTempNDVI.pEff			,
  MA_m.psiAvgMinTempPrecip.pEff	    ,
  MA_m.psiAvgMinTempElev.pEff	      	,
  MA_m.psiAvgMinTempHFI.pEff	      	,
  MA_m.psiAvgMinTempRoad.pEff	      	,
  MA_m.psiAvgMinTempNDVI.pEff	      	,
  MA_m.psiAvgMinTempAvgMaxTemp.pEff	,
  MA_m.psiAvgMinTempNPP.pEff	     	,
  MA_m.psiPrecipRoad.pEff				,
  MA_m.psiPrecipNDVI.pEff				,
  MA_m.psiNDVIRoad.pEff				,
  MA_m.psiHFIPrecip.pEff	         	,
  MA_m.psiHFIElev.pEff	         	,
  MA_m.psiHFIRoad.pEff	         	,
  MA_m.psiHFINDVI.pEff	         	,
  MA_m.psiHFIAvgMaxTemp.pEff	     	,
  MA_m.psiHFIAvgMinTemp.pEff	     	,
  MA_m.psiHFINPP.pEff	             	,
  MA_m.psi3.pEff 					  	,
  MA_m.PrecipElevHFI.pEff           	,
  MA_m.PrecipElevRoad.pEff          	,
  MA_m.PrecipElevNDVI.pEff          	,
  MA_m.PrecipElevAvgMinTemp.pEff    	,
  MA_m.PrecipElevAvgMaxTemp.pEff    	,
  MA_m.PrecipElevNPP.pEff           	,
  MA_m.PrecipHFIRoad.pEff             ,
  MA_m.PrecipHFINDVI.pEff             ,
  MA_m.PrecipHFIAvgMinTemp.pEff       ,
  MA_m.PrecipHFIAvgMaxTemp.pEff       ,
  MA_m.PrecipHFINPP.pEff              ,
  MA_m.PrecipRoadNDVI.pEff            ,
  MA_m.PrecipRoadAvgMinTemp.pEff      ,
  MA_m.PrecipRoadAvgMaxTemp.pEff      ,
  MA_m.PrecipRoadNPP.pEff             ,
  MA_m.PrecipNDVIAvgMinTemp.pEff      ,
  MA_m.PrecipNDVIAvgMaxTemp.pEff      ,
  MA_m.PrecipNDVINPP.pEff             ,
  MA_m.PrecipAvgMinTempAvgMaxTemp.pEff,
  MA_m.PrecipAvgMinTempNPP.pEff       ,
  MA_m.PrecipAvgMaxTempNPP.pEff       ,
  MA_m.ElevHFIRoad.pEff               ,
  MA_m.ElevHFINDVI.pEff               ,
  MA_m.ElevHFIAvgMinTemp.pEff         ,
  MA_m.ElevHFIAvgMaxTemp.pEff         ,
  MA_m.ElevHFINPP.pEff                ,
  MA_m.ElevRoadNDVI.pEff              ,
  MA_m.ElevRoadAvgMinTemp.pEff        ,
  MA_m.ElevRoadAvgMaxTemp.pEff        ,
  MA_m.ElevRoadNPP.pEff               ,
  MA_m.ElevNDVIAvgMinTemp.pEff        ,
  MA_m.ElevNDVIAvgMaxTemp.pEff        ,
  MA_m.ElevNDVINPP.pEff               ,
  MA_m.ElevAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.ElevAvgMinTempNPP.pEff         ,
  MA_m.ElevAvgMaxTempNPP.pEff         ,
  MA_m.HFIRoadNDVI.pEff               ,
  MA_m.HFIRoadAvgMinTemp.pEff         ,
  MA_m.HFIRoadAvgMaxTemp.pEff         ,
  MA_m.HFIRoadNPP.pEff                ,
  MA_m.HFINDVIAvgMinTemp.pEff         ,
  MA_m.HFINDVIAvgMaxTemp.pEff         ,
  MA_m.HFINDVINPP.pEff                ,
  MA_m.HFIAvgMinTempAvgMaxTemp.pEff   ,
  MA_m.HFIAvgMinTempNPP.pEff          ,
  MA_m.HFIAvgMaxTempNPP.pEff          ,
  MA_m.RoadNDVIAvgMinTemp.pEff        ,
  MA_m.RoadNDVIAvgMaxTemp.pEff        ,
  MA_m.RoadNDVINPP.pEff               ,
  MA_m.RoadAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.RoadAvgMinTempNPP.pEff         ,
  MA_m.RoadAvgMaxTempNPP.pEff         ,
  MA_m.NDVIAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.NDVIAvgMinTempNPP.pEff         ,
  MA_m.NDVIAvgMaxTempNPP.pEff         ,
  MA_m.AvgMinTempAvgMaxTempNPP.pEff    
  
)

#####Output####
columns<- c(6:13) #"Precip"  "Elev" "HFI" "d.Road"  "NDVI" "AvgMinTemp" "AvgMaxTemp" "NPP"       
sink("MA_multiMods.txt", append = FALSE)

  cat("19, July 2023\n")
  cat("\n***Correlation Matrix***\n")
  cor(MA_cov[, columns]) #correlation matrix on numerical fields
  print("**Malayan Tapir Models**")
  cat()
  modSel(MA_detlist) #model selection table
  cat("\n**Summaries**\n")
  getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()
