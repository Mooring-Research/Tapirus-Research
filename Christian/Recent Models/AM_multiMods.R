###------------------------------------------------------------------------------###
### Running multivariate models on lowland tapir
### CA 19Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> tapir_AM.rds >> tapir occurance records
#> eff_AM.rds >> effort table
#> cv_t_AM_v2.csv >> covariate table

rm(list=ls())
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Amazon (Lowland Tapir)")#Directory of R-project "Models" on github
#dir()

library(unmarked)

#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Cates Stuff/Models/modelsOutputToFile_function.R")


#read in tapir occurance records
AM_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/tapir_AM.rds")
#read in effort table
AM_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/eff_AM.rds")
#read in covariate table
AM_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/cv_t_AM_v2.csv")

#scale covariates
AM_cov<- cbind(AM_cov[,c(1:4)], round(scale(AM_cov[,5:ncol(AM_cov)]),3))

names(AM_cov)[c(16,17,18)] <- c("AvgMaxTemp","AvgMinTemp", "Precip")


#ensure rownames match
# rownames(AM_tapir) == rownames(AM_eff)
# rownames(AM_eff) == AM_cv$Station

# Checking for sitecov correlations
#as.dist(cor(AM_cv[,-c(1:4)]))
# Some sitecovs are correlated: Road&Elev, ED&PD&DC (do not include correlated covs in the same model)

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
AM_m.PrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, AM_umf)	
AM_m.PrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, AM_umf)
AM_m.PrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, AM_umf)
AM_m.PrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, AM_umf)
AM_m.PrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, AM_umf) 
AM_m.PrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, AM_umf)
AM_m.PrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, AM_umf)
AM_m.PrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, AM_umf)
AM_m.PrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, AM_umf)
AM_m.PrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, AM_umf)
AM_m.PrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, AM_umf)
AM_m.PrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, AM_umf)
AM_m.PrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, AM_umf)
AM_m.PrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, AM_umf)
AM_m.PrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, AM_umf)
AM_m.PrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, AM_umf)
AM_m.PrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, AM_umf)
AM_m.PrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, AM_umf)
AM_m.PrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.PrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, AM_umf)
AM_m.PrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, AM_umf)

AM_m.ElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, AM_umf)
AM_m.ElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, AM_umf)
AM_m.ElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, AM_umf)
AM_m.ElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, AM_umf)
AM_m.ElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, AM_umf)
AM_m.ElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, AM_umf)
AM_m.ElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, AM_umf)
AM_m.ElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, AM_umf)
AM_m.ElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, AM_umf)
AM_m.ElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, AM_umf)
AM_m.ElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, AM_umf)
AM_m.ElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, AM_umf)
AM_m.ElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , AM_umf)
AM_m.ElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , AM_umf)
AM_m.ElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, AM_umf)

AM_m.HFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, AM_umf)
AM_m.HFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, AM_umf)
AM_m.HFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, AM_umf)
AM_m.HFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, AM_umf)
AM_m.HFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, AM_umf)
AM_m.HFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, AM_umf)
AM_m.HFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, AM_umf)
AM_m.HFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.HFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, AM_umf)
AM_m.HFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, AM_umf)

AM_m.RoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, AM_umf)
AM_m.RoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, AM_umf)
AM_m.RoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, AM_umf)
AM_m.RoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.RoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, AM_umf)
AM_m.RoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, AM_umf)

AM_m.NDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, AM_umf)
AM_m.NDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, AM_umf)
AM_m.NDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, AM_umf)

AM_m.AvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, AM_umf)#<^TRIPLE


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
  AM_m.PrecipElevHFI.pEff           		, 
  AM_m.PrecipElevRoad.pEff          		, 
  AM_m.PrecipElevNDVI.pEff          		, 
  AM_m.PrecipElevAvgMinTemp.pEff    		, 
  AM_m.PrecipElevAvgMaxTemp.pEff    	  	,
  AM_m.PrecipElevNPP.pEff           		, 
  AM_m.PrecipHFIRoad.pEff                ,	
  AM_m.PrecipHFINDVI.pEff                ,	
  AM_m.PrecipHFIAvgMinTemp.pEff          ,	
  AM_m.PrecipHFIAvgMaxTemp.pEff          ,	
  AM_m.PrecipHFINPP.pEff                 ,	
  AM_m.PrecipRoadNDVI.pEff               ,	
  AM_m.PrecipRoadAvgMinTemp.pEff         ,	
  AM_m.PrecipRoadAvgMaxTemp.pEff         ,	
  AM_m.PrecipRoadNPP.pEff                ,	
  AM_m.PrecipNDVIAvgMinTemp.pEff         ,	
  AM_m.PrecipNDVIAvgMaxTemp.pEff         ,	
  AM_m.PrecipNDVINPP.pEff                ,	
  AM_m.PrecipAvgMinTempAvgMaxTemp.pEff   ,	
  AM_m.PrecipAvgMinTempNPP.pEff          ,	
  AM_m.PrecipAvgMaxTempNPP.pEff          ,	
  AM_m.ElevHFIRoad.pEff                  ,	
  AM_m.ElevHFINDVI.pEff                  ,	
  AM_m.ElevHFIAvgMinTemp.pEff            ,	
  AM_m.ElevHFIAvgMaxTemp.pEff            ,	
  AM_m.ElevHFINPP.pEff                   ,	
  AM_m.ElevRoadNDVI.pEff                 ,	
  AM_m.ElevRoadAvgMinTemp.pEff           ,	
  AM_m.ElevRoadAvgMaxTemp.pEff           ,	
  AM_m.ElevRoadNPP.pEff                  ,	
  AM_m.ElevNDVIAvgMinTemp.pEff           ,	
  AM_m.ElevNDVIAvgMaxTemp.pEff           ,	
  AM_m.ElevNDVINPP.pEff                  ,	
  AM_m.ElevAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.ElevAvgMinTempNPP.pEff            ,	
  AM_m.ElevAvgMaxTempNPP.pEff            ,	
  AM_m.HFIRoadNDVI.pEff                  ,	
  AM_m.HFIRoadAvgMinTemp.pEff            ,	
  AM_m.HFIRoadAvgMaxTemp.pEff            ,	
  AM_m.HFIRoadNPP.pEff                   ,	
  AM_m.HFINDVIAvgMinTemp.pEff            ,	
  AM_m.HFINDVIAvgMaxTemp.pEff            ,	
  AM_m.HFINDVINPP.pEff                   ,	
  AM_m.HFIAvgMinTempAvgMaxTemp.pEff      ,	
  AM_m.HFIAvgMinTempNPP.pEff             ,	
  AM_m.HFIAvgMaxTempNPP.pEff             ,	
  AM_m.RoadNDVIAvgMinTemp.pEff           ,	
  AM_m.RoadNDVIAvgMaxTemp.pEff           ,	
  AM_m.RoadNDVINPP.pEff                  ,	
  AM_m.RoadAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.RoadAvgMinTempNPP.pEff            ,	
  AM_m.RoadAvgMaxTempNPP.pEff            ,	
  AM_m.NDVIAvgMinTempAvgMaxTemp.pEff     ,	
  AM_m.NDVIAvgMinTempNPP.pEff            ,	
  AM_m.NDVIAvgMaxTempNPP.pEff            ,	
  AM_m.AvgMinTempAvgMaxTempNPP.pEff      
  
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
columns<- c(5:18)

sink("AM_multiMods.txt", append = FALSE)

cat("19, July 2023\n")
cat("\n***Correlation Matrix***\n")
cor(AM_cov[, columns]) #correlation matrix on numerical fields
print("\n**Lowland Tapir Models**")
cat()
modSel(AM_detlist) #model selection table
cat("\n**Summaries**\n")
getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()


