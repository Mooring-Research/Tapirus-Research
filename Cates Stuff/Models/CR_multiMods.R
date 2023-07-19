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
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")#Directory of R-project "Models" on github
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")


#occurance recs
CR_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/tapir_CR.rds") 
#effort table
CR_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/eff_CR.rds")
#covariates
CR_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/cv_t3.csv")

# #remove first column and scale all numerical values
# CR_cv<- cbind(CR_cv[,c(2:5, 7)], round(scale(CR_cv[,c(6, 8:ncol(CR_cv))])))



CR_umf<- unmarkedFrameOccu(y=CR_tapir, siteCovs= as.data.frame(scale(CR_cov[,-c(1:5)])), obsCovs=list(Eff=CR_eff))
#-----------------------------------------------------------------------
CR_mod0 								<- occu(~1~   1, CR_umf)  
CR_m.psi1.pEff							<- occu(~Eff~ 1, CR_umf) 
CR_m.psiElev.pEff						<- occu(~Eff~ Elev , CR_umf)	
CR_m.psiHFI.pEff						<- occu(~Eff~ HFI , CR_umf) 	
CR_m.psiFor.pEff						<- occu(~Eff~ Forest , CR_umf)
CR_m.psiNPP.pEff						<- occu(~Eff~ NPP , CR_umf)
CR_m.psiRoad.pEff						<- occu(~Eff~ d.Road , CR_umf)
CR_m.psiRiver.pEff						<- occu(~Eff~ d.River , CR_umf)
CR_m.psiED.pEff						<- occu(~Eff~ EdgeDens , CR_umf)
CR_m.psiPD.pEff						<- occu(~Eff~ PatchDens , CR_umf)
CR_m.psiDC.pEff						<- occu(~Eff~ DisjCore , CR_umf)
CR_m.psiTempmin.pEff					<- occu(~Eff~ Avg.Min.Temp, CR_umf)
CR_m.psiTempmax.pEff					<- occu(~Eff~ Avg.Max.Temp, CR_umf)
CR_m.psiNDVI.pEff						<- occu(~Eff~ NDVI, CR_umf) 
CR_m.pEff.psiPrec						<- occu(~Eff~ Precip, CR_umf) #<^SINGLE
CR_m.psiElevTempmax.pEff				<- occu(~Eff~ Elev + Avg.Max.Temp, CR_umf)	
CR_m.psiElevPrecip.pEff				<- occu(~Eff~ Elev + Precip, CR_umf)	
CR_m.psiElevRoad.pEff					<- occu(~Eff~ Elev + d.Road, CR_umf)	
CR_m.psiElevNDVI.pEff					<- occu(~Eff~ Elev + NDVI, CR_umf)	
CR_m.psiAvgMaxTempPrecip.pEff			<- occu(~Eff~ Avg.Max.Temp + Precip, CR_umf)	
CR_m.psiAvgMaxTempRoad.pEff			<- occu(~Eff~ Avg.Max.Temp + d.Road, CR_umf)	
CR_m.psiAvgMaxTempNDVI.pEff			<- occu(~Eff~ Avg.Max.Temp + NDVI, CR_umf)	
CR_m.psiPrecipRoad.pEff				<- occu(~Eff~ Precip + d.Road, CR_umf)	
CR_m.psiPrecipNDVI.pEff				<- occu(~Eff~ Precip + NDVI, CR_umf)	
CR_m.psiNDVIRoad.pEff					<- occu(~Eff~ NDVI + d.Road, CR_umf)	#<^DOUBLE
CR_m.psiElevTempmaxPrecip.pEff			<- occu(~Eff~ Elev + Avg.Max.Temp + Precip, CR_umf)	
CR_m.psiElevTempmaxRoad.pEff			<- occu(~Eff~ Elev + Avg.Max.Temp + d.Road, CR_umf)	
CR_m.psiElevTempmaxNDVI.pEff			<- occu(~Eff~ Elev + Avg.Max.Temp + NDVI, CR_umf)	
CR_m.psiTempmaxPrecipRoad.pEff			<- occu(~Eff~ Avg.Max.Temp + Precip + d.Road, CR_umf)	
CR_m.psiAvgMaxTempPrecipNDVI.pEff		<- occu(~Eff~ Avg.Max.Temp + Precip + NDVI, CR_umf)	
CR_m.psiAvgMaxTempNDVIRoad.pEff		<- occu(~Eff~ Avg.Max.Temp + NDVI + d.Road, CR_umf)	
CR_m.psiPrecipRoadNDVI.pEff			<- occu(~Eff~ Precip + d.Road + NDVI, CR_umf)	
CR_m.psiPrecipElevNDVI.pEff			<- occu(~Eff~ Precip + Elev + NDVI, CR_umf)	
CR_m.psiPrecipElevRoad.pEff			<- occu(~Eff~ Precip + Elev + d.Road, CR_umf)	
CR_m.psiRoadNDVIElev.pEff				<- occu(~Eff~ d.Road + NDVI + Elev, CR_umf) #<^QUADRUPLE	
CR_m.psiTempElevNDVIRoad.pEff 			<- occu(~Eff~ Avg.Max.Temp + Elev + NDVI + d.Road, CR_umf)	
CR_m.psiElevNDVIPrecipRoad.pEff 		<- occu(~Eff~ Elev + NDVI + Precip + d.Road, CR_umf)	
CR_m.psiTempElevRoadPrecip.pEff 		<- occu(~Eff~ Avg.Max.Temp + Elev + d.Road + Precip, CR_umf)	
CR_m.psiTempNDVIRoadPrecip.pEff 		<- occu(~Eff~ Avg.Max.Temp + NDVI + d.Road + Precip, CR_umf)	
CR_m.psiTempElevNDVIPrecip.pEff 		<- occu(~Eff~ Avg.Max.Temp + Elev + NDVI + Precip, CR_umf)	


CR_detlist<- list(
          CR_mod0 							,	
          CR_m.psi1.pEff						,	
          CR_m.psiElev.pEff					,	
          CR_m.psiHFI.pEff					,	
          CR_m.psiFor.pEff					,	
          CR_m.psiNPP.pEff					,	
          CR_m.psiRoad.pEff					,	
          CR_m.psiRiver.pEff					,	
          CR_m.psiED.pEff						,	
          CR_m.psiPD.pEff						,	
          CR_m.psiDC.pEff						,	
          CR_m.psiTempmin.pEff				,	
          CR_m.psiTempmax.pEff				,	
          CR_m.psiNDVI.pEff					,	
          CR_m.pEff.psiPrec					,	
          CR_m.psiElevTempmax.pEff			,	
          CR_m.psiElevPrecip.pEff				,	
          CR_m.psiElevRoad.pEff				,	
          CR_m.psiElevNDVI.pEff				,	
          CR_m.psiAvgMaxTempPrecip.pEff		,	
          CR_m.psiAvgMaxTempRoad.pEff			,	
          CR_m.psiAvgMaxTempNDVI.pEff			,	
          CR_m.psiPrecipRoad.pEff				,	
          CR_m.psiPrecipNDVI.pEff				,	
          CR_m.psiNDVIRoad.pEff				,	
          CR_m.psiElevTempmaxPrecip.pEff		,	
          CR_m.psiElevTempmaxRoad.pEff		,	
          CR_m.psiElevTempmaxNDVI.pEff		,	
          CR_m.psiTempmaxPrecipRoad.pEff		,	
          CR_m.psiAvgMaxTempPrecipNDVI.pEff	,	
          CR_m.psiAvgMaxTempNDVIRoad.pEff		,	
          CR_m.psiPrecipRoadNDVI.pEff			,	
          CR_m.psiPrecipElevNDVI.pEff			,	
          CR_m.psiPrecipElevRoad.pEff			,	
          CR_m.psiRoadNDVIElev.pEff			,	
          CR_m.psiTempElevNDVIRoad.pEff 		,	
          CR_m.psiElevNDVIPrecipRoad.pEff 	,	
          CR_m.psiTempElevRoadPrecip.pEff 	,	
          CR_m.psiTempNDVIRoadPrecip.pEff 	,	
          CR_m.psiTempElevNDVIPrecip.pEff 		
          
  
)