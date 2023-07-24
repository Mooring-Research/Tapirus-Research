###------------------------------------------------------------------------------###
### Running multivariate models on Malayan tapir
### CA 24Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Malayan_Tapir.rds >> tapir occurance records
#> Effort_Malayan_Tapir.rds >> effort table
#> Ma_T_Final_Covs.csv NOW> MA_covs.csv 17jul2023>> covariate table


rm(list=ls())
library(unmarked)

#grabs this handy function from another file
source("C:/Users/chris/Tapirus-Research/Christian/Recent Models/modelsOutputToFile_function.R")

#set wd
setwd("C:/Users/chris/Tapirus-Research/Christian/Recent Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")##Directory of R-project "Models" on github
#dir()


######Read in Tapir table and Effort###########
MA_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Collapsed_Capture_Malayan_Tapir.rds")

MA_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Data Processing/Effort_Malayan_Tapir.rds")


#MA_cv<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Ma_T_Final_Covs.csv")
MA_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/MA_covs.csv")

#unmarked frame
#double check what columns are scaled!!
MA_cov<- MA_cov[,which(names(MA_cov) %in% c("Elev", "HFI", "NPP", "NDVI", "d.Road", "AvgMaxTemp", "AvgMinTemp", "Precip"))]

colsToScale<- which(names(MA_cov) %in% c("Elev", "NPP", "NDVI", "d.Road", "AvgMaxTemp", "AvgMinTemp", "Precip")) #scale all but HFI
MA_cov[colsToScale]<- scale(MA_cov[,colsToScale])

MA_umf<- unmarkedFrameOccu(y=MA_tapir[,-1], siteCovs= MA_cov, obsCovs=list(Eff=MA_eff[,-1]))



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
MA_m.psiPrecipElevHFI.pEff           		  <- occu(~Eff~ Precip + HFI + Elev, MA_umf)	
MA_m.psiPrecipElevRoad.pEff          		  <- occu(~Eff~ Precip + Elev + d.Road, MA_umf)
MA_m.psiPrecipElevNDVI.pEff          		  <- occu(~Eff~ Precip + Elev + NDVI, MA_umf)
MA_m.psiPrecipElevAvgMinTemp.pEff    		  <- occu(~Eff~ Precip + Elev + AvgMinTemp, MA_umf)
MA_m.psiPrecipElevAvgMaxTemp.pEff    	  	<- occu(~Eff~ Precip + Elev + AvgMaxTemp, MA_umf) 
MA_m.psiPrecipElevNPP.pEff           		  <- occu(~Eff~ Precip + Elev + NPP, MA_umf)
MA_m.psiPrecipHFIRoad.pEff                 <- occu(~Eff~ Precip + HFI + d.Road, MA_umf)
MA_m.psiPrecipHFINDVI.pEff                 <- occu(~Eff~ Precip + HFI + NDVI, MA_umf)
MA_m.psiPrecipHFIAvgMinTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMinTemp, MA_umf)
MA_m.psiPrecipHFIAvgMaxTemp.pEff           <- occu(~Eff~ Precip + HFI + AvgMaxTemp, MA_umf)
MA_m.psiPrecipHFINPP.pEff                  <- occu(~Eff~ Precip + HFI + NPP, MA_umf)
MA_m.psiPrecipRoadNDVI.pEff                <- occu(~Eff~ Precip + d.Road + NDVI, MA_umf)
MA_m.psiPrecipRoadAvgMinTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMinTemp, MA_umf)
MA_m.psiPrecipRoadAvgMaxTemp.pEff          <- occu(~Eff~ Precip + d.Road + AvgMaxTemp, MA_umf)
MA_m.psiPrecipRoadNPP.pEff                 <- occu(~Eff~ Precip + d.Road + NPP, MA_umf)
MA_m.psiPrecipNDVIAvgMinTemp.pEff          <- occu(~Eff~ Precip + NDVI + NDVI, MA_umf)
MA_m.psiPrecipNDVIAvgMaxTemp.pEff          <- occu(~Eff~ Precip + NDVI + AvgMaxTemp, MA_umf)
MA_m.psiPrecipNDVINPP.pEff                 <- occu(~Eff~ Precip + NDVI + NPP, MA_umf)
MA_m.psiPrecipAvgMinTempAvgMaxTemp.pEff    <- occu(~Eff~ Precip + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.psiPrecipAvgMinTempNPP.pEff           <- occu(~Eff~ Precip + AvgMinTemp + NPP, MA_umf)
MA_m.psiPrecipAvgMaxTempNPP.pEff           <- occu(~Eff~ Precip + AvgMaxTemp + NPP, MA_umf)

MA_m.psiElevHFIRoad.pEff                   <- occu(~Eff~ Elev + HFI + d.Road, MA_umf)
MA_m.psiElevHFINDVI.pEff                   <- occu(~Eff~ Elev + HFI + NDVI, MA_umf)
MA_m.psiElevHFIAvgMinTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMinTemp, MA_umf)
MA_m.psiElevHFIAvgMaxTemp.pEff             <- occu(~Eff~ Elev + HFI + AvgMaxTemp, MA_umf)
MA_m.psiElevHFINPP.pEff                    <- occu(~Eff~ Elev + HFI + NPP, MA_umf)
MA_m.psiElevRoadNDVI.pEff                  <- occu(~Eff~ Elev + d.Road + NDVI, MA_umf)
MA_m.psiElevRoadAvgMinTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMinTemp, MA_umf)
MA_m.psiElevRoadAvgMaxTemp.pEff            <- occu(~Eff~ Elev + d.Road + AvgMaxTemp, MA_umf)
MA_m.psiElevRoadNPP.pEff                   <- occu(~Eff~ Elev + d.Road + NPP, MA_umf)
MA_m.psiElevNDVIAvgMinTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMinTemp, MA_umf)
MA_m.psiElevNDVIAvgMaxTemp.pEff            <- occu(~Eff~ Elev + NDVI + AvgMaxTemp, MA_umf)
MA_m.psiElevNDVINPP.pEff                   <- occu(~Eff~ Elev + NDVI + NPP, MA_umf)
MA_m.psiElevAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ Elev + AvgMinTemp + AvgMaxTemp , MA_umf)
MA_m.psiElevAvgMinTempNPP.pEff             <- occu(~Eff~ Elev + AvgMinTemp + NPP , MA_umf)
MA_m.psiElevAvgMaxTempNPP.pEff             <- occu(~Eff~ Elev + AvgMaxTemp + NPP, MA_umf)

MA_m.psiHFIRoadNDVI.pEff                   <- occu(~Eff~ HFI + d.Road + NDVI, MA_umf)
MA_m.psiHFIRoadAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MA_umf)
MA_m.psiHFIRoadAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMaxTemp, MA_umf)
MA_m.psiHFIRoadNPP.pEff                    <- occu(~Eff~ HFI + d.Road + NPP, MA_umf)
MA_m.psiHFINDVIAvgMinTemp.pEff             <- occu(~Eff~ HFI + d.Road + AvgMinTemp, MA_umf)
MA_m.psiHFINDVIAvgMaxTemp.pEff             <- occu(~Eff~ HFI + d.Road + NDVI, MA_umf)
MA_m.psiHFINDVINPP.pEff                    <- occu(~Eff~ HFI + NDVI + NPP, MA_umf)
MA_m.psiHFIAvgMinTempAvgMaxTemp.pEff       <- occu(~Eff~ HFI + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.psiHFIAvgMinTempNPP.pEff              <- occu(~Eff~ HFI + AvgMinTemp + NPP, MA_umf)
MA_m.psiHFIAvgMaxTempNPP.pEff              <- occu(~Eff~ HFI + AvgMaxTemp + NPP, MA_umf)

MA_m.psiRoadNDVIAvgMinTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMinTemp, MA_umf)
MA_m.psiRoadNDVIAvgMaxTemp.pEff            <- occu(~Eff~ d.Road + NDVI + AvgMaxTemp, MA_umf)
MA_m.psiRoadNDVINPP.pEff                   <- occu(~Eff~ d.Road + NDVI + NPP, MA_umf)
MA_m.psiRoadAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ d.Road + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.psiRoadAvgMinTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMinTemp + NPP, MA_umf)
MA_m.psiRoadAvgMaxTempNPP.pEff             <- occu(~Eff~ d.Road + AvgMaxTemp + NPP, MA_umf)

MA_m.psiNDVIAvgMinTempAvgMaxTemp.pEff      <- occu(~Eff~ NDVI + AvgMinTemp + AvgMaxTemp, MA_umf)
MA_m.psiNDVIAvgMinTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMinTemp + NPP, MA_umf)
MA_m.psiNDVIAvgMaxTempNPP.pEff             <- occu(~Eff~ NDVI + AvgMaxTemp + NPP, MA_umf)

MA_m.psiAvgMinTempAvgMaxTempNPP.pEff       <- occu(~Eff~ AvgMinTemp + AvgMaxTemp + NPP, MA_umf)#<^TRIPLE



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
  MA_m.psiPrecipElevHFI.pEff           	,
  MA_m.psiPrecipElevRoad.pEff          	,
  MA_m.psiPrecipElevNDVI.pEff          	,
  MA_m.psiPrecipElevAvgMinTemp.pEff    	,
  MA_m.psiPrecipElevAvgMaxTemp.pEff    	,
  MA_m.psiPrecipElevNPP.pEff           	,
  MA_m.psiPrecipHFIRoad.pEff             ,
  MA_m.psiPrecipHFINDVI.pEff             ,
  MA_m.psiPrecipHFIAvgMinTemp.pEff       ,
  MA_m.psiPrecipHFIAvgMaxTemp.pEff       ,
  MA_m.psiPrecipHFINPP.pEff              ,
  MA_m.psiPrecipRoadNDVI.pEff            ,
  MA_m.psiPrecipRoadAvgMinTemp.pEff      ,
  MA_m.psiPrecipRoadAvgMaxTemp.pEff      ,
  MA_m.psiPrecipRoadNPP.pEff             ,
  MA_m.psiPrecipNDVIAvgMinTemp.pEff      ,
  MA_m.psiPrecipNDVIAvgMaxTemp.pEff      ,
  MA_m.psiPrecipNDVINPP.pEff             ,
  MA_m.psiPrecipAvgMinTempAvgMaxTemp.pEff,
  MA_m.psiPrecipAvgMinTempNPP.pEff       ,
  MA_m.psiPrecipAvgMaxTempNPP.pEff       ,
  MA_m.psiElevHFIRoad.pEff               ,
  MA_m.psiElevHFINDVI.pEff               ,
  MA_m.psiElevHFIAvgMinTemp.pEff         ,
  MA_m.psiElevHFIAvgMaxTemp.pEff         ,
  MA_m.psiElevHFINPP.pEff                ,
  MA_m.psiElevRoadNDVI.pEff              ,
  MA_m.psiElevRoadAvgMinTemp.pEff        ,
  MA_m.psiElevRoadAvgMaxTemp.pEff        ,
  MA_m.psiElevRoadNPP.pEff               ,
  MA_m.psiElevNDVIAvgMinTemp.pEff        ,
  MA_m.psiElevNDVIAvgMaxTemp.pEff        ,
  MA_m.psiElevNDVINPP.pEff               ,
  MA_m.psiElevAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.psiElevAvgMinTempNPP.pEff         ,
  MA_m.psiElevAvgMaxTempNPP.pEff         ,
  MA_m.psiHFIRoadNDVI.pEff               ,
  MA_m.psiHFIRoadAvgMinTemp.pEff         ,
  MA_m.psiHFIRoadAvgMaxTemp.pEff         ,
  MA_m.psiHFIRoadNPP.pEff                ,
  MA_m.psiHFINDVIAvgMinTemp.pEff         ,
  MA_m.psiHFINDVIAvgMaxTemp.pEff         ,
  MA_m.psiHFINDVINPP.pEff                ,
  MA_m.psiHFIAvgMinTempAvgMaxTemp.pEff   ,
  MA_m.psiHFIAvgMinTempNPP.pEff          ,
  MA_m.psiHFIAvgMaxTempNPP.pEff          ,
  MA_m.psiRoadNDVIAvgMinTemp.pEff        ,
  MA_m.psiRoadNDVIAvgMaxTemp.pEff        ,
  MA_m.psiRoadNDVINPP.pEff               ,
  MA_m.psiRoadAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.psiRoadAvgMinTempNPP.pEff         ,
  MA_m.psiRoadAvgMaxTempNPP.pEff         ,
  MA_m.psiNDVIAvgMinTempAvgMaxTemp.pEff  ,
  MA_m.psiNDVIAvgMinTempNPP.pEff         ,
  MA_m.psiNDVIAvgMaxTempNPP.pEff         ,
  MA_m.psiAvgMinTempAvgMaxTempNPP.pEff    
  
)

#####Output####
      
#sink("Multivariate model summaries/MA_multiMods3.txt", append = FALSE)

  cat("24, July 2023\n")
  cat("\n***Correlation Matrix***\n")
  cor(MA_cov) #correlation matrix on numerical fields  #"Precip"  "Elev" "HFI" "d.Road"  "NDVI" "AvgMinTemp" "AvgMaxTemp" "NPP" 
  print("**Malayan Tapir Models**")
  cat()
  modSel(MA_detlist) #model selection table
  cat("\n**Summaries**\n")
  getStats() #print p and psi for all models function in C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R

sink()
