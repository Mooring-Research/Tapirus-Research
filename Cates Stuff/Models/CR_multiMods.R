###------------------------------------------------------------------------------###
### Running multivariate models on baird's
### Edited by: CA 19Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> tapir_CR.rds >> tapir occurance records
#> eff_CR.rds >> effort table
#> cv_t3.csv >> covariate table


rm(list=ls())

library(unmarked)
#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Cates Stuff/Models/modelsOutputToFile_function.R")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")#Directory of R-project "Models" on github
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")


#occurance recs
CR_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/tapir_CR.rds") 
#effort table
CR_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/eff_CR.rds")
#covariates
CR_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/cv_t3.csv")

#rename columns to names in models below
names(CR_cov)[c(16,17)] <- c("AvgMinTemp","AvgMaxTemp")



CR_umf<- unmarkedFrameOccu(y=CR_tapir, siteCovs= as.data.frame(scale(CR_cov[,-c(1:5)])), obsCovs=list(Eff=CR_eff))
#-----------------------------------------------------------------------
#Running Models#######################################################################
CR_m.psi1.pEff						        <- occu(~Eff~ 1, CR_umf)  #effnull
CR_m.psiHFI.pEff 					        <- occu(~Eff~ HFI, CR_umf)
CR_m.psiElev.pEff				         	<- occu(~Eff~ Elev, CR_umf)
CR_m.psiPrec.pEff				        	<- occu(~Eff~ Precip, CR_umf)
CR_m.psiRoad.pEff			        	  <- occu(~Eff~ d.Road, CR_umf)
CR_m.psiTempmin.pEff			      	<- occu(~Eff~ AvgMinTemp, CR_umf)
CR_m.psiTempmax.pEff			      	<- occu(~Eff~ AvgMaxTemp, CR_umf) 
CR_m.psiNDVI.pEff				        	<- occu(~Eff~ NDVI, CR_umf) 
CR_m.psiNPP.pEff                  <- occu(~Eff~ NPP, CR_umf)#<^SINGLE

CR_m.psi2.pEff					        	<- occu(~Eff~ 1+1, CR_umf) #2 effnull
CR_m.psiElevAvgMaxTemp.pEff			  <- occu(~Eff~ Elev + AvgMaxTemp, CR_umf)	
CR_m.psiElevPrecip.pEff				    <- occu(~Eff~ Elev + Precip, CR_umf)	
CR_m.psiElevRoad.pEff				      <- occu(~Eff~ Elev + d.Road, CR_umf)	
CR_m.psiElevNDVI.pEff				      <- occu(~Eff~ Elev + NDVI, CR_umf)	
CR_m.psiAvgMaxTempPrecip.pEff		  <- occu(~Eff~ AvgMaxTemp + Precip, CR_umf)	
CR_m.psiAvgMaxTempRoad.pEff		  	<- occu(~Eff~ AvgMaxTemp + d.Road, CR_umf)	
CR_m.psiAvgMaxTempNDVI.pEff			  <- occu(~Eff~ AvgMaxTemp + NDVI, CR_umf)	
CR_m.psiAvgMinTempPrecip.pEff	    <- occu(~Eff~ Precip + AvgMinTemp, CR_umf)
CR_m.psiAvgMinTempElev.pEff	      <- occu(~Eff~ AvgMinTemp + Elev, CR_umf)
CR_m.psiAvgMinTempHFI.pEff	      <- occu(~Eff~ AvgMinTemp + HFI, CR_umf)
CR_m.psiAvgMinTempRoad.pEff	      <- occu(~Eff~ AvgMinTemp + d.Road, CR_umf)
CR_m.psiAvgMinTempNDVI.pEff	      <- occu(~Eff~ AvgMinTemp + NDVI, CR_umf)
CR_m.psiAvgMinTempAvgMaxTemp.pEff	<- occu(~Eff~ AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.psiAvgMinTempNPP.pEff	      <- occu(~Eff~ AvgMinTemp + NPP, CR_umf)
CR_m.psiPrecipRoad.pEff				    <- occu(~Eff~ Precip + d.Road, CR_umf)	
CR_m.psiPrecipNDVI.pEff				    <- occu(~Eff~ Precip + NDVI, CR_umf)	
CR_m.psiNDVIRoad.pEff				      <- occu(~Eff~ NDVI + d.Road, CR_umf)
CR_m.psiHFIPrecip.pEff	          <- occu(~Eff~ HFI + Precip, CR_umf)
CR_m.psiHFIElev.pEff	            <- occu(~Eff~ HFI + Elev, CR_umf)
CR_m.psiHFIRoad.pEff	            <- occu(~Eff~ HFI + d.Road, CR_umf)
CR_m.psiHFINDVI.pEff	            <- occu(~Eff~ HFI + NDVI, CR_umf)
CR_m.psiHFIAvgMaxTemp.pEff	      <- occu(~Eff~ HFI + AvgMaxTemp, CR_umf)
CR_m.psiHFIAvgMinTemp.pEff	      <- occu(~Eff~ HFI + AvgMinTemp, CR_umf)
CR_m.psiHFINPP.pEff	              <- occu(~Eff~ HFI + NPP, CR_umf)#<^DOUBLE	

CR_m.psi3.pEff 					  		          <- occu(~Eff~ 1+1+1, CR_umf) #3 effnull
CR_m.PrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, CR_umf)	
CR_m.PrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, CR_umf)
CR_m.PrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, CR_umf)
CR_m.PrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, CR_umf)
CR_m.PrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, CR_umf) 
CR_m.PrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, CR_umf)
CR_m.PrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, CR_umf)
CR_m.PrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, CR_umf)
CR_m.PrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, CR_umf)
CR_m.PrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, CR_umf)
CR_m.PrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, CR_umf)
CR_m.PrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, CR_umf)
CR_m.PrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, CR_umf)
CR_m.PrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, CR_umf)
CR_m.PrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, CR_umf)
CR_m.PrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, CR_umf)
CR_m.PrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, CR_umf)
CR_m.PrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, CR_umf)
CR_m.PrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.PrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, CR_umf)
CR_m.PrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, CR_umf)

CR_m.ElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, CR_umf)
CR_m.ElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, CR_umf)
CR_m.ElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, CR_umf)
CR_m.ElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, CR_umf)
CR_m.ElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, CR_umf)
CR_m.ElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, CR_umf)
CR_m.ElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, CR_umf)
CR_m.ElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, CR_umf)
CR_m.ElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, CR_umf)
CR_m.ElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, CR_umf)
CR_m.ElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, CR_umf)
CR_m.ElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, CR_umf)
CR_m.ElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , CR_umf)
CR_m.ElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , CR_umf)
CR_m.ElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, CR_umf)

CR_m.HFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, CR_umf)
CR_m.HFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, CR_umf)
CR_m.HFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, CR_umf)
CR_m.HFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, CR_umf)
CR_m.HFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, CR_umf)
CR_m.HFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, CR_umf)
CR_m.HFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, CR_umf)
CR_m.HFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.HFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, CR_umf)
CR_m.HFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, CR_umf)

CR_m.RoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, CR_umf)
CR_m.RoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, CR_umf)
CR_m.RoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, CR_umf)
CR_m.RoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.RoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, CR_umf)
CR_m.RoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, CR_umf)

CR_m.NDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.NDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, CR_umf)
CR_m.NDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, CR_umf)

CR_m.AvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, CR_umf)#<^TRIPLE


######Model list#####
CR_detlist<- fitList( CR_m.psi1.pEff	,
                      CR_m.psiHFI.pEff 						,
                      CR_m.psiElev.pEff						,
                      CR_m.psiPrec.pEff						,
                      CR_m.psiRoad.pEff			    		,
                      CR_m.psiTempmin.pEff					,
                      CR_m.psiTempmax.pEff					,
                      CR_m.psiNDVI.pEff						,
                      CR_m.psiNPP.pEff              ,
                      CR_m.psi2.pEff							,
                      CR_m.psiElevAvgMaxTemp.pEff				,
                      CR_m.psiElevPrecip.pEff					,
                      CR_m.psiElevRoad.pEff					,
                      CR_m.psiElevNDVI.pEff					,
                      CR_m.psiAvgMaxTempPrecip.pEff			,
                      CR_m.psiAvgMaxTempRoad.pEff				,
                      CR_m.psiAvgMaxTempNDVI.pEff				,
                      CR_m.psiAvgMinTempPrecip.pEff			,
                      CR_m.psiAvgMinTempElev.pEff	    		,
                      CR_m.psiAvgMinTempHFI.pEff	    		,
                      CR_m.psiAvgMinTempRoad.pEff	    		,
                      CR_m.psiAvgMinTempNDVI.pEff	       		,
                      CR_m.psiAvgMinTempAvgMaxTemp.pEff   	,
                      CR_m.psiAvgMinTempNPP.pEff	      		, 
                      CR_m.psiPrecipRoad.pEff					,
                      CR_m.psiPrecipNDVI.pEff					,
                      CR_m.psiNDVIRoad.pEff					,
                      CR_m.psiHFIPrecip.pEff	          		,
                      CR_m.psiHFIElev.pEff	          		, 
                      CR_m.psiHFIRoad.pEff	          		, 
                      CR_m.psiHFINDVI.pEff	          		, 
                      CR_m.psiHFIAvgMaxTemp.pEff	      		,
                      CR_m.psiHFIAvgMinTemp.pEff	      		,
                      CR_m.psiHFINPP.pEff	              		,
                      CR_m.psi3.pEff 					  		, 
                      CR_m.PrecipElevHFI.pEff           		, 
                      CR_m.PrecipElevRoad.pEff          		, 
                      CR_m.PrecipElevNDVI.pEff          		, 
                      CR_m.PrecipElevAvgMinTemp.pEff    		, 
                      CR_m.PrecipElevAvgMaxTemp.pEff    	  	,
                      CR_m.PrecipElevNPP.pEff           		, 
                      CR_m.PrecipHFIRoad.pEff                ,	
                      CR_m.PrecipHFINDVI.pEff                ,	
                      CR_m.PrecipHFIAvgMinTemp.pEff          ,	
                      CR_m.PrecipHFIAvgMaxTemp.pEff          ,	
                      CR_m.PrecipHFINPP.pEff                 ,	
                      CR_m.PrecipRoadNDVI.pEff               ,	
                      CR_m.PrecipRoadAvgMinTemp.pEff         ,	
                      CR_m.PrecipRoadAvgMaxTemp.pEff         ,	
                      CR_m.PrecipRoadNPP.pEff                ,	
                      CR_m.PrecipNDVIAvgMinTemp.pEff         ,	
                      CR_m.PrecipNDVIAvgMaxTemp.pEff         ,	
                      CR_m.PrecipNDVINPP.pEff                ,	
                      CR_m.PrecipAvgMinTempAvgMaxTemp.pEff   ,	
                      CR_m.PrecipAvgMinTempNPP.pEff          ,	
                      CR_m.PrecipAvgMaxTempNPP.pEff          ,	
                      CR_m.ElevHFIRoad.pEff                  ,	
                      CR_m.ElevHFINDVI.pEff                  ,	
                      CR_m.ElevHFIAvgMinTemp.pEff            ,	
                      CR_m.ElevHFIAvgMaxTemp.pEff            ,	
                      CR_m.ElevHFINPP.pEff                   ,	
                      CR_m.ElevRoadNDVI.pEff                 ,	
                      CR_m.ElevRoadAvgMinTemp.pEff           ,	
                      CR_m.ElevRoadAvgMaxTemp.pEff           ,	
                      CR_m.ElevRoadNPP.pEff                  ,	
                      CR_m.ElevNDVIAvgMinTemp.pEff           ,	
                      CR_m.ElevNDVIAvgMaxTemp.pEff           ,	
                      CR_m.ElevNDVINPP.pEff                  ,	
                      CR_m.ElevAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.ElevAvgMinTempNPP.pEff            ,	
                      CR_m.ElevAvgMaxTempNPP.pEff            ,	
                      CR_m.HFIRoadNDVI.pEff                  ,	
                      CR_m.HFIRoadAvgMinTemp.pEff            ,	
                      CR_m.HFIRoadAvgMaxTemp.pEff            ,	
                      CR_m.HFIRoadNPP.pEff                   ,	
                      CR_m.HFINDVIAvgMinTemp.pEff            ,	
                      CR_m.HFINDVIAvgMaxTemp.pEff            ,	
                      CR_m.HFINDVINPP.pEff                   ,	
                      CR_m.HFIAvgMinTempAvgMaxTemp.pEff      ,	
                      CR_m.HFIAvgMinTempNPP.pEff             ,	
                      CR_m.HFIAvgMaxTempNPP.pEff             ,	
                      CR_m.RoadNDVIAvgMinTemp.pEff           ,	
                      CR_m.RoadNDVIAvgMaxTemp.pEff           ,	
                      CR_m.RoadNDVINPP.pEff                  ,	
                      CR_m.RoadAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.RoadAvgMinTempNPP.pEff            ,	
                      CR_m.RoadAvgMaxTempNPP.pEff            ,	
                      CR_m.NDVIAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.NDVIAvgMinTempNPP.pEff            ,	
                      CR_m.NDVIAvgMaxTempNPP.pEff            ,	
                      CR_m.AvgMinTempAvgMaxTempNPP.pEff      
                      
)


#####output#####
columns<- c(6:18)

sink("CR_multiMods.txt", append = FALSE)

cat("19, July 2023\n")
cat("\n***Correlation Matrix***\n")
cor(CR_cov[, columns]) #correlation matrix on numerical fields
print("\n**Baird's Tapir Models**")
cat()
modSel(CR_detlist) #model selection table
cat("**")
cat("\n**Summaries**\n")
getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()
