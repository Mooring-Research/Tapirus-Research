###------------------------------------------------------------------------------###
### Running multivariate models on lowland tapir
### CA 24Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> tapir_AM.rds >> tapir occurance records
#> eff_AM.rds >> effort table
#> cv_t_AM_v2.csv >> covariate table

rm(list=ls())
library(unmarked)

setwd("C:/Users/chris/Tapirus-Research/Christian/Recent Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Amazon (Lowland Tapir)")#Directory of R-project "Models" on github
#dir()


#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Christian/Recent Models/modelsOutputToFile_function.R")


#read in tapir occurance records
AM_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/tapir_AM.rds")
#read in effort table
AM_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/eff_AM.rds")
#read in covariate table
AM_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/cv_t_AM_v2.csv")

names(AM_cov)[which(names(AM_cov) %in% c("Avg.Max.Temp", "Avg.Min.Temp", "MAP"))] <- c("AvgMaxTemp","AvgMinTemp", "Precip")

AM_cov<- AM_cov[,which(names(AM_cov) %in% c("Elev", "HFI", "NPP", "NDVI", "d.Road", "AvgMaxTemp", "AvgMinTemp", "Precip"))]
#scale covs #exclude location info, for all covs except HFI scaled
colsToScale<- c(1,3:8) #!HFI
AM_cov[,colsToScale]<- scale(AM_cov[, colsToScale])

#Establish Unmarked Data Frame##############################################################

AM_umf<- unmarkedFrameOccu(y=AM_tapir, siteCovs=AM_cov, obsCovs=list(Eff=AM_eff))

#Running Models#######################################################################
AM_m.psi1.pEff						        <- occu(~Eff~ 1, AM_umf)  #effnull
AM_m.psiHFI.pEff 					        <- occu(~Eff~ HFI, AM_umf)
AM_m.psiElev.pEff				         	<- occu(~Eff~ Elev, AM_umf)
AM_m.psiPrec.pEff				        	<- occu(~Eff~ Precip, AM_umf)
AM_m.psiRoad.pEff			        	  <- occu(~Eff~ d.Road, AM_umf)
AM_m.psiTempmin.pEff			      	<- occu(~Eff~ AvgMinTemp, AM_umf)
AM_m.psiTempmax.pEff			      	<- occu(~Eff~ AvgMaxTemp, AM_umf) 
AM_m.psiNDVI.pEff				        	<- occu(~Eff~ NDVI, AM_umf) 
AM_m.psiNPP.pEff                  <- occu(~Eff~ NPP, AM_umf)#<^SINGLE

AM_m.psi2.pEff					        	<- occu(~Eff~ 1+1, AM_umf) #2 effnull
AM_m.psiElevAvgMaxTemp.pEff			  <- occu(~Eff~ Elev + AvgMaxTemp, AM_umf)	
AM_m.psiElevPrecip.pEff				    <- occu(~Eff~ Elev + Precip, AM_umf)	
AM_m.psiElevRoad.pEff				      <- occu(~Eff~ Elev + d.Road, AM_umf)	
AM_m.psiElevNDVI.pEff				      <- occu(~Eff~ Elev + NDVI, AM_umf)	
AM_m.psiAvgMaxTempPrecip.pEff		  <- occu(~Eff~ AvgMaxTemp + Precip, AM_umf)	
AM_m.psiAvgMaxTempRoad.pEff		  	<- occu(~Eff~ AvgMaxTemp + d.Road, AM_umf)	
AM_m.psiAvgMaxTempNDVI.pEff			  <- occu(~Eff~ AvgMaxTemp + NDVI, AM_umf)	
AM_m.psiAvgMinTempPrecip.pEff	    <- occu(~Eff~ Precip + AvgMinTemp, AM_umf)
AM_m.psiAvgMinTempElev.pEff	      <- occu(~Eff~ AvgMinTemp + Elev, AM_umf)
AM_m.psiAvgMinTempHFI.pEff	      <- occu(~Eff~ AvgMinTemp + HFI, AM_umf)
AM_m.psiAvgMinTempRoad.pEff	      <- occu(~Eff~ AvgMinTemp + d.Road, AM_umf)
AM_m.psiAvgMinTempNDVI.pEff	      <- occu(~Eff~ AvgMinTemp + NDVI, AM_umf)
AM_m.psiAvgMinTempAvgMaxTemp.pEff	<- occu(~Eff~ AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.psiAvgMinTempNPP.pEff	      <- occu(~Eff~ AvgMinTemp + NPP, AM_umf)
AM_m.psiPrecipRoad.pEff				    <- occu(~Eff~ Precip + d.Road, AM_umf)	
AM_m.psiPrecipNDVI.pEff				    <- occu(~Eff~ Precip + NDVI, AM_umf)	
AM_m.psiNDVIRoad.pEff				      <- occu(~Eff~ NDVI + d.Road, AM_umf)
AM_m.psiHFIPrecip.pEff	          <- occu(~Eff~ HFI + Precip, AM_umf)
AM_m.psiHFIElev.pEff	            <- occu(~Eff~ HFI + Elev, AM_umf)
AM_m.psiHFIRoad.pEff	            <- occu(~Eff~ HFI + d.Road, AM_umf)
AM_m.psiHFINDVI.pEff	            <- occu(~Eff~ HFI + NDVI, AM_umf)
AM_m.psiHFIAvgMaxTemp.pEff	      <- occu(~Eff~ HFI + AvgMaxTemp, AM_umf)
AM_m.psiHFINPP.pEff	              <- occu(~Eff~ HFI + NPP, AM_umf)#<^DOUBLE

AM_m.psi3.pEff 					  		          <- occu(~Eff~ 1+1+1, AM_umf) #3 effnull
AM_m.psiPrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, AM_umf)	
AM_m.psiPrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, AM_umf)
AM_m.psiPrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, AM_umf)
AM_m.psiPrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, AM_umf)
AM_m.psiPrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, AM_umf) 
AM_m.psiPrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, AM_umf)
AM_m.psiPrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, AM_umf)
AM_m.psiPrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, AM_umf)
AM_m.psiPrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, AM_umf)
AM_m.psiPrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, AM_umf)
AM_m.psiPrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, AM_umf)
AM_m.psiPrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, AM_umf)
AM_m.psiPrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, AM_umf)
AM_m.psiPrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, AM_umf)
AM_m.psiPrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, AM_umf)
AM_m.psiPrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, AM_umf)
AM_m.psiPrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, AM_umf)
AM_m.psiPrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, AM_umf)
AM_m.psiPrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.psiPrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, AM_umf)
AM_m.psiPrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, AM_umf)

AM_m.psiElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, AM_umf)
AM_m.psiElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, AM_umf)
AM_m.psiElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, AM_umf)
AM_m.psiElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, AM_umf)
AM_m.psiElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, AM_umf)
AM_m.psiElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, AM_umf)
AM_m.psiElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, AM_umf)
AM_m.psiElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, AM_umf)
AM_m.psiElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, AM_umf)
AM_m.psiElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, AM_umf)
AM_m.psiElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, AM_umf)
AM_m.psiElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, AM_umf)
AM_m.psiElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , AM_umf)
AM_m.psiElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , AM_umf)
AM_m.psiElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, AM_umf)

AM_m.psiHFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, AM_umf)
AM_m.psiHFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, AM_umf)
AM_m.psiHFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, AM_umf)
AM_m.psiHFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, AM_umf)
AM_m.psiHFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, AM_umf)
AM_m.psiHFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, AM_umf)
AM_m.psiHFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, AM_umf)
AM_m.psiHFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.psiHFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, AM_umf)
AM_m.psiHFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, AM_umf)

AM_m.psiRoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, AM_umf)
AM_m.psiRoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, AM_umf)
AM_m.psiRoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, AM_umf)
AM_m.psiRoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.psiRoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, AM_umf)
AM_m.psiRoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, AM_umf)

AM_m.psiNDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.psiNDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, AM_umf)
AM_m.psiNDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, AM_umf)

AM_m.psiAvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, AM_umf)#<^TRIPLE


######Model list#####
AM_detlist<- fitList( AM_m.psi1.pEff	,
  AM_m.psiHFI.pEff 						,
  AM_m.psiElev.pEff						,
  AM_m.psiPrec.pEff						,
  AM_m.psiRoad.pEff			    		,
  AM_m.psiTempmin.pEff					,
  AM_m.psiTempmax.pEff					,
  AM_m.psiNDVI.pEff						,
  AM_m.psiNPP.pEff              ,
  AM_m.psi2.pEff							,
  AM_m.psiElevAvgMaxTemp.pEff				,
  AM_m.psiElevPrecip.pEff					,
  AM_m.psiElevRoad.pEff					,
  AM_m.psiElevNDVI.pEff					,
  AM_m.psiAvgMaxTempPrecip.pEff			,
  AM_m.psiAvgMaxTempRoad.pEff				,
  AM_m.psiAvgMaxTempNDVI.pEff				,
  AM_m.psiAvgMinTempPrecip.pEff			,
  AM_m.psiAvgMinTempElev.pEff	    		,
  AM_m.psiAvgMinTempHFI.pEff	    		,
  AM_m.psiAvgMinTempRoad.pEff	    		,
  AM_m.psiAvgMinTempNDVI.pEff	       		,
  AM_m.psiAvgMinTempAvgMaxTemp.pEff   	,
  AM_m.psiAvgMinTempNPP.pEff	      		, 
  AM_m.psiPrecipRoad.pEff					,
  AM_m.psiPrecipNDVI.pEff					,
  AM_m.psiNDVIRoad.pEff					,
  AM_m.psiHFIPrecip.pEff	          		,
  AM_m.psiHFIElev.pEff	          		,
  AM_m.psiHFIRoad.pEff	          		,
  AM_m.psiHFINDVI.pEff	          		,
  AM_m.psiHFIAvgMaxTemp.pEff	      		,
  AM_m.psiHFINPP.pEff	              		,
  AM_m.psi3.pEff 					  		, 
  AM_m.psiPrecipElevHFI.pEff           		,
  AM_m.psiPrecipElevRoad.pEff          		, 
  AM_m.psiPrecipElevNDVI.pEff          		, 
  AM_m.psiPrecipElevAvgMinTemp.pEff    		, 
  AM_m.psiPrecipElevAvgMaxTemp.pEff    	  	,
  AM_m.psiPrecipElevNPP.pEff           		, 
  AM_m.psiPrecipHFIRoad.pEff                ,
  AM_m.psiPrecipHFINDVI.pEff                ,
  AM_m.psiPrecipHFIAvgMinTemp.pEff          ,
  AM_m.psiPrecipHFIAvgMaxTemp.pEff          ,
  AM_m.psiPrecipHFINPP.pEff                 ,
  AM_m.psiPrecipRoadNDVI.pEff               ,	
  AM_m.psiPrecipRoadAvgMinTemp.pEff         ,	
  AM_m.psiPrecipRoadAvgMaxTemp.pEff         ,	
  AM_m.psiPrecipRoadNPP.pEff                ,	
  AM_m.psiPrecipNDVIAvgMinTemp.pEff         ,	
  AM_m.psiPrecipNDVIAvgMaxTemp.pEff         ,	
  AM_m.psiPrecipNDVINPP.pEff                ,	
  AM_m.psiPrecipAvgMinTempAvgMaxTemp.pEff   ,	
  AM_m.psiPrecipAvgMinTempNPP.pEff          ,	
  AM_m.psiPrecipAvgMaxTempNPP.pEff          ,	
  AM_m.psiElevHFIRoad.pEff                  ,
  AM_m.psiElevHFINDVI.pEff                  ,
  AM_m.psiElevHFIAvgMinTemp.pEff            ,
  AM_m.psiElevHFIAvgMaxTemp.pEff            ,
  AM_m.psiElevHFINPP.pEff                   ,
  AM_m.psiElevRoadNDVI.pEff                 ,	
  AM_m.psiElevRoadAvgMinTemp.pEff           ,	
  AM_m.psiElevRoadAvgMaxTemp.pEff           ,	
  AM_m.psiElevRoadNPP.pEff                  ,	
  AM_m.psiElevNDVIAvgMinTemp.pEff           ,	
  AM_m.psiElevNDVIAvgMaxTemp.pEff           ,	
  AM_m.psiElevNDVINPP.pEff                  ,	
  AM_m.psiElevAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.psiElevAvgMinTempNPP.pEff            ,	
  AM_m.psiElevAvgMaxTempNPP.pEff            ,	
  AM_m.psiHFIRoadNDVI.pEff                  ,
  AM_m.psiHFIRoadAvgMinTemp.pEff            ,
  AM_m.psiHFIRoadAvgMaxTemp.pEff            ,
  AM_m.psiHFIRoadNPP.pEff                   ,
  AM_m.psiHFINDVIAvgMinTemp.pEff            ,
  AM_m.psiHFINDVIAvgMaxTemp.pEff            ,
  AM_m.psiHFINDVINPP.pEff                   ,
  AM_m.psiHFIAvgMinTempAvgMaxTemp.pEff      ,
  AM_m.psiHFIAvgMinTempNPP.pEff             ,
  AM_m.psiHFIAvgMaxTempNPP.pEff             ,
  AM_m.psiRoadNDVIAvgMinTemp.pEff           ,	
  AM_m.psiRoadNDVIAvgMaxTemp.pEff           ,	
  AM_m.psiRoadNDVINPP.pEff                  ,	
  AM_m.psiRoadAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.psiRoadAvgMinTempNPP.pEff            ,	
  AM_m.psiRoadAvgMaxTempNPP.pEff            ,	
  AM_m.psiNDVIAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.psiNDVIAvgMinTempNPP.pEff            ,	
  AM_m.psiNDVIAvgMaxTempNPP.pEff            ,	
  AM_m.psiAvgMinTempAvgMaxTempNPP.pEff      
  
)

#previos#####
# 
# 
# AM_m.psi1.pEff							  	<- occu(~Eff~ 1, AM_umf) 
# AM_m.psiElev.pEff						  	<- occu(~Eff~ Elev , AM_umf)
# AM_m.psiHFI.pEff						  	<- occu(~Eff~ HFI , AM_umf)
# AM_m.psiFor.pEff						  	<- occu(~Eff~ Forest , AM_umf)
# AM_m.psiNPP.pEff						  	<- occu(~Eff~ NPP , AM_umf)
# AM_m.psiRoad.pEff							  <- occu(~Eff~ d.Road , AM_umf)  
# AM_m.psiRiver.pEff			      	<- occu(~Eff~ d.River, AM_umf)   
# AM_m.psiED.pEff				         	<- occu(~Eff~ EdgeDens , AM_umf)
# AM_m.psiPD.pEff								  <- occu(~Eff~ PatchDens , AM_umf)
# AM_m.psiDC.pEff				        	<- occu(~Eff~ DisjCore , AM_umf)
# AM_m.psiWat.pEff				        <- occu(~Eff~ Water , AM_umf)
# AM_m.psiReg.pEff			        	<- occu(~Eff~ Dataset , AM_umf)
# AM_m.psiNDVI.pEff			        	<- occu(~Eff~ NDVI , AM_umf)
# AM_m.psiTemp.pEff				        <- occu(~Eff~ Avg.Max.Temp , AM_umf)
# AM_m.psiPrecip.pEff			      	<- occu(~Eff~ MAP , AM_umf)
# AM_m.psiTempElev.pEff 		    	<- occu(~Eff~ Avg.Max.Temp + Elev, AM_umf)
# AM_m.psiTempNDVI.pEff		      	<- occu(~Eff~ NDVI + Avg.Max.Temp, AM_umf)
# AM_m.psiTempRoad.pEff 		    	<- occu(~Eff~ d.Road + Avg.Max.Temp, AM_umf)
# AM_m.psiTempPrecip.pEff 	    	<- occu(~Eff~ MAP + Avg.Max.Temp, AM_umf)
# AM_m.psiElevNDVI.pEff 		    	<- occu(~Eff~ NDVI + Elev, AM_umf)
# AM_m.psiElevRoad.pEff 		    	<- occu(~Eff~ Elev + d.Road, AM_umf)
# AM_m.psiElevPrecip.pEff 	    	<- occu(~Eff~ Elev + MAP, AM_umf)
# AM_m.psiNDVIroad.pEff 		    	<- occu(~Eff~ NDVI + d.Road, AM_umf)
# AM_m.psiNDVIprecip.pEff     		<- occu(~Eff~ NDVI + MAP, AM_umf)
# AM_m.psiPrecipRoad.pEff      		<- occu(~Eff~ MAP + d.Road, AM_umf)
# AM_m.psiTempElevNDVI.pEff 	   	<- occu(~Eff~ Avg.Max.Temp + Elev + NDVI, AM_umf) 
# AM_m.psiTempElevRoad.pEff 	   	<- occu(~Eff~ Elev + Avg.Max.Temp + d.Road, AM_umf) 
# AM_m.psiTempElevPrecip.pEff    	<- occu(~Eff~ Elev + Avg.Max.Temp + MAP, AM_umf) 
# AM_m.psiTempNDVIRoad.pEff 	   	<- occu(~Eff~ NDVI + Avg.Max.Temp + d.Road, AM_umf) 
# AM_m.psiTempNDVIPrecip.pEff    	<- occu(~Eff~ NDVI + MAP + Avg.Max.Temp, AM_umf) 
# AM_m.psiTempRoadPrecip.pEff    	<- occu(~Eff~ Avg.Max.Temp + d.Road + MAP, AM_umf) 
# AM_m.psiElevRoadNDVI.pEff 	   	<- occu(~Eff~ Elev + d.Road + NDVI, AM_umf) 
# AM_m.psiElevPrecipNDVI.pEff    	<- occu(~Eff~ NDVI + Elev + MAP, AM_umf) 
# AM_m.psiElevRoadPrecip.pEff		  <- occu(~Eff~ Elev + d.Road + MAP, AM_umf) 
# AM_m.psiNDVIPrecipRoad.pEff 	  <- occu(~Eff~ MAP + d.Road + NDVI, AM_umf) 
# AM_m.psiTempElevNDVIRoad.pEff 	<- occu(~Eff~ Avg.Max.Temp + Elev + NDVI + d.Road, AM_umf) 
# AM_m.psiTempElevNDVIPrecip.pEff <- occu(~Eff~ Elev + Avg.Max.Temp + NDVI + MAP, AM_umf) 
# AM_m.psiTempElevRoadPrecip.pEff <- occu(~Eff~ Elev + Avg.Max.Temp + MAP + d.Road, AM_umf) 
# AM_m.psiTempNDVIRoadPrecip.pEff <- occu(~Eff~ NDVI + Avg.Max.Temp + d.Road + MAP, AM_umf) 
# AM_m.psiElevNDVIPrecipRoad.pEff <- occu(~Eff~ NDVI + MAP + Elev + d.Road, AM_umf) 
# 
# ### Models list##
# AM_detlist<- list(
#   AM_m.psi1.pEff					,
#   AM_m.psiElev.pEff				,
#   AM_m.psiHFI.pEff				,
#   AM_m.psiFor.pEff				,
#   AM_m.psiNPP.pEff				,
#   AM_m.psiRoad.pEff				,
#   AM_m.psiRiver.pEff				,
#   AM_m.psiED.pEff					,
#   AM_m.psiPD.pEff					,
#   AM_m.psiDC.pEff					,
#   AM_m.psiWat.pEff				,
#   AM_m.psiReg.pEff				,
#   AM_m.psiNDVI.pEff				,
#   AM_m.psiTemp.pEff				,
#   AM_m.psiPrecip.pEff				,
#   AM_m.psiTempElev.pEff 			,
#   AM_m.psiTempNDVI.pEff			,
#   AM_m.psiTempRoad.pEff 			,
#   AM_m.psiTempPrecip.pEff 		,
#   AM_m.psiElevNDVI.pEff 			,
#   AM_m.psiElevRoad.pEff 			,
#   AM_m.psiElevPrecip.pEff 		,
#   AM_m.psiNDVIroad.pEff 			,
#   AM_m.psiNDVIprecip.pEff 		,
#   AM_m.psiPrecipRoad.pEff 		,
#   AM_m.psiTempElevNDVI.pEff 		,
#   AM_m.psiTempElevRoad.pEff 		,
#   AM_m.psiTempElevPrecip.pEff 	,
#   AM_m.psiTempNDVIRoad.pEff 		,
#   AM_m.psiTempNDVIPrecip.pEff 	,
#   AM_m.psiTempRoadPrecip.pEff 	,
#   AM_m.psiElevRoadNDVI.pEff 		,
#   AM_m.psiElevPrecipNDVI.pEff 	,
#   AM_m.psiElevRoadPrecip.pEff		,
#   AM_m.psiNDVIPrecipRoad.pEff 	,
#   AM_m.psiTempElevNDVIRoad.pEff 	,
#   AM_m.psiTempElevNDVIPrecip.pEff ,
#   AM_m.psiTempElevRoadPrecip.pEff ,
#   AM_m.psiTempNDVIRoadPrecip.pEff ,
#   AM_m.psiElevNDVIPrecipRoad.pEff  
# )


#####output#####

columns<- c(1:8)

#sink("Multivariate model summaries/AM_multiMods2.txt", append = FALSE)

cat("24, July 2023\n")
cat("\n***Correlation Matrix***\n")
cor(AM_cov) #correlation matrix on numerical fields
print("\n**Lowland Tapir Models**")
cat()
modSel(AM_detlist) #model selection table
cat("\n**Summaries**\n")
getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()


