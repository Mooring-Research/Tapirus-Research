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
source("C:/Users/chris/Tapirus-Research/Christian/Recent Models/modelsOutputToFile_function.R")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")#Directory of R-project "Models" on github
setwd("C:/Users/chris/Tapirus-Research/Christian/Recent Models")


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
CR_m.psiPrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, CR_umf)	
CR_m.psiPrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, CR_umf)
CR_m.psiPrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, CR_umf)
CR_m.psiPrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, CR_umf)
CR_m.psiPrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, CR_umf) 
CR_m.psiPrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, CR_umf)
CR_m.psiPrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, CR_umf)
CR_m.psiPrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, CR_umf)
CR_m.psiPrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, CR_umf)
CR_m.psiPrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, CR_umf)
CR_m.psiPrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, CR_umf)
CR_m.psiPrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, CR_umf)
CR_m.psiPrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, CR_umf)
CR_m.psiPrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, CR_umf)
CR_m.psiPrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, CR_umf)
CR_m.psiPrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, CR_umf)
CR_m.psiPrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, CR_umf)
CR_m.psiPrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, CR_umf)
CR_m.psiPrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.psiPrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, CR_umf)
CR_m.psiPrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, CR_umf)

CR_m.psiElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, CR_umf)
CR_m.psiElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, CR_umf)
CR_m.psiElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, CR_umf)
CR_m.psiElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, CR_umf)
CR_m.psiElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, CR_umf)
CR_m.psiElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, CR_umf)
CR_m.psiElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, CR_umf)
CR_m.psiElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, CR_umf)
CR_m.psiElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, CR_umf)
CR_m.psiElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, CR_umf)
CR_m.psiElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, CR_umf)
CR_m.psiElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, CR_umf)
CR_m.psiElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , CR_umf)
CR_m.psiElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , CR_umf)
CR_m.psiElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, CR_umf)

CR_m.psiHFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, CR_umf)
CR_m.psiHFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, CR_umf)
CR_m.psiHFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, CR_umf)
CR_m.psiHFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, CR_umf)
CR_m.psiHFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, CR_umf)
CR_m.psiHFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, CR_umf)
CR_m.psiHFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, CR_umf)
CR_m.psiHFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.psiHFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, CR_umf)
CR_m.psiHFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, CR_umf)

CR_m.psiRoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, CR_umf)
CR_m.psiRoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, CR_umf)
CR_m.psiRoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, CR_umf)
CR_m.psiRoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.psiRoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, CR_umf)
CR_m.psiRoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, CR_umf)

CR_m.psiNDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, CR_umf)
CR_m.psiNDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, CR_umf)
CR_m.psiNDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, CR_umf)

CR_m.psiAvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, CR_umf)#<^TRIPLE


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
                      CR_m.psiElevHFIRoad.pEff                  ,	
                      CR_m.psiElevHFINDVI.pEff                  ,	
                      CR_m.psiElevHFIAvgMinTemp.pEff            ,	
                      CR_m.psiElevHFIAvgMaxTemp.pEff            ,	
                      CR_m.psiElevHFINPP.pEff                   ,	
                      CR_m.psiElevRoadNDVI.pEff                 ,	
                      CR_m.psiElevRoadAvgMinTemp.pEff           ,	
                      CR_m.psiElevRoadAvgMaxTemp.pEff           ,	
                      CR_m.psiElevRoadNPP.pEff                  ,	
                      CR_m.psiElevNDVIAvgMinTemp.pEff           ,	
                      CR_m.psiElevNDVIAvgMaxTemp.pEff           ,	
                      CR_m.psiElevNDVINPP.pEff                  ,	
                      CR_m.psiElevAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.psiElevAvgMinTempNPP.pEff            ,	
                      CR_m.psiElevAvgMaxTempNPP.pEff            ,	
                      CR_m.psiHFIRoadNDVI.pEff                  ,	
                      CR_m.psiHFIRoadAvgMinTemp.pEff            ,	
                      CR_m.psiHFIRoadAvgMaxTemp.pEff            ,	
                      CR_m.psiHFIRoadNPP.pEff                   ,	
                      CR_m.psiHFINDVIAvgMinTemp.pEff            ,	
                      CR_m.psiHFINDVIAvgMaxTemp.pEff            ,	
                      CR_m.psiHFINDVINPP.pEff                   ,	
                      CR_m.psiHFIAvgMinTempAvgMaxTemp.pEff      ,	
                      CR_m.psiHFIAvgMinTempNPP.pEff             ,	
                      CR_m.psiHFIAvgMaxTempNPP.pEff             ,	
                      CR_m.psiRoadNDVIAvgMinTemp.pEff           ,	
                      CR_m.psiRoadNDVIAvgMaxTemp.pEff           ,	
                      CR_m.psiRoadNDVINPP.pEff                  ,	
                      CR_m.psiRoadAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.psiRoadAvgMinTempNPP.pEff            ,	
                      CR_m.psiRoadAvgMaxTempNPP.pEff            ,	
                      CR_m.psiNDVIAvgMinTempAvgMaxTemp.pEff     ,	
                      CR_m.psiNDVIAvgMinTempNPP.pEff            ,	
                      CR_m.psiNDVIAvgMaxTempNPP.pEff            ,	
                      CR_m.psiAvgMinTempAvgMaxTempNPP.pEff      
                      
)


#####output#####
columns<- c(6:18)

sink("CR_multiMods2.txt", append = FALSE)

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
