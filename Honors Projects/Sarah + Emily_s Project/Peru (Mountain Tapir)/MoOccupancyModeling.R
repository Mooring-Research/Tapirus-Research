###------------------------------------------------------------------------------###
### Mountain Tapir Analysis
### Sarah Turcic
### Date: 3/12/22
###------------------------------------------------------------------------------###


rm(list=ls())
setwd("~/Downloads/Mountain")

library(unmarked)

#Read in Variables###################################################################

#read in tapir occurance records
tapir_t<- readRDS("Collapsed_Capture_Mountain_Tapir_revised_DR.rds") 
head(tapir_t)

#read in effort table
eff_t<- readRDS("Effort_Mountain_Tapir_revised_DR.rds")
head(eff_t)

#read in covariate table
sc_t<- read.csv("Mt_T_Covs4.csv") #only HFI, elev, d.Road (not all covs)
head(sc_t)

#edit covariate table
#sc_t <- sc_t[,c("Camr_Nm","Latitud","Longitd","S06W080.hgt","HFI_MountainTapir_EPSG4326")]
#colnames(sc_t)[colnames(sc_t) == "S06W080.hgt"] ="Elev"
#colnames(sc_t)[colnames(sc_t) == "HFI_MountainTapir_EPSG4326"] ="HFI"
sc_t <- sc_t[,-1] #get rid of random X column

#scale covariates and rename to match previous code
sc_t2<- cbind(sc_t[,1:3], round(scale(sc_t[,c(4,6,7,8)]),3)) #don't scale HFI because all the same
sc_t2 <- cbind(sc_t2, sc_t[c(5,10)]) #add HFI back
#colnames(sc_t2)[4]="Elev"
colnames(sc_t2)[1]="Station"

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == sc_t2$Station #they are the same but it still says false?

#effort
sum(rowSums(eff_t, na.rm = TRUE)) #9,456 effort
mean(rowSums(eff_t, na.rm = TRUE))	#111.25 mean camera active days
sd(rowSums(eff_t, na.rm = TRUE)) 	#sd mean camera active days 30.92
sum(rowSums(tapir_t, na.rm = TRUE)) #85 independent records


#Establish Unmarked Data Frame##############################################################

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf) #28 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( -0.595)  	# Probability of occupancy 0.35
plogis( -1.56)	# Probability of detection 0.17
28/85 #Naive occupancy - 0.3294

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
#Elev
m.psiElev.pEff<- occu(~Eff~Elev , umf)
summary(m.psiElev.pEff)
plogis( 0.299) # Probability of occupancy 0.574
plogis(0.405)	# Probability of detection 0.599

#HFI
m.psiHFI.pEff<- occu(~Eff~HFI , umf) #all have same HFI, so no use for this species
summary(m.psiHFI.pEff)
plogis(-0.1377) # Probability of occupancy 0.46
plogis(0.402)	# Probability of detection 0.60

#d.Road
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)
summary(m.psiRoad.pEff)
plogis( -0.0636) # Probability of occupancy 0.48
plogis(0.402)	# Probability of detection 0.60

#Precip
m.psiPrecip.pEff <- occu(~Eff~Precip, umf)
summary(m.psiPrecip.pEff)
plogis(0.0261) # Probability of occupancy 0.51
plogis(0.402)	# Probability of detection 0.60

#NDVI
m.psiNDVI.pEff <- occu(~Eff~NDVI, umf)
summary(m.psiNDVI.pEff)
plogis(-0.253) # Probability of occupancy 0.44
plogis(0.405) # Probability of detection 0.60

#Temp
m.psiTemp.pEff <- occu(~Eff~AvgMaxTemp, umf)
summary(m.psiTemp.pEff)


#select models using in paper
honlist <- fitList(mod0, 
                   m.psiElev.pEff, 
                   m.psiHFI.pEff,
                   m.psiRoad.pEff,
                   m.psiPrecip.pEff, 
                   m.psiNDVI.pEff,
                   m.psiTemp.pEff)

##do AIC model selection
modSel(honlist) 
#pdf("Tapir_unicovRegion_Mt_total.pdf")


###############################################################

# Plotting top Unicovariate model (Road) 
pred.psi.road<- predict(m.psiRoad.pEff, 
    newdata= data.frame(d.Road= sort(scale(sc_t$d.Road))), "state")

plot(sort(sc_t$d.Road), pred.psi.road$Predicted, type="l", xaxt="s", 
    cex.lab=1.3,	xlab="Distance to Road", ylab="Probability of Occupancy", 
    lwd=2, col="blue", xlim = c(750, 10000), ylim= c(0,1))
#axis(side = 1, at = sort(scale(sc_t2$d.Road)), labels = round(sort(sc_t$d.Road),0))
points(sort(sc_t$d.Road), pred.psi.road$lower, type="l", lty=2)
points(sort(sc_t$d.Road), pred.psi.road$upper, type="l", lty=2)
dev.off()
#pdf("Tapir_unicovRoad_Mt_total.pdf")


# Plotting top Unicovariate model (Elev)
pred.psi.elev<- predict(m.psiElev.pEff , 
	newdata= data.frame(Elev= sort(sc_t2$Elev)), "state")

plot(sort(sc_t$Elev), pred.psi.elev$Predicted, type="l", xaxt="s", 
	cex.lab=1.3,	xlab="Elevation (m)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
#axis(side = 1, at = sort(scale(sc_t2$Elev)), labels = round(sort(sc_t$Elev),0))
points(sort(sc_t$Elev), pred.psi.elev$lower, type="l", lty=2)
points(sort(sc_t$Elev), pred.psi.elev$upper, type="l", lty=2)
dev.off()
#pdf("Tapir_unicovElev_Mt_total.pdf")


# Plotting top Unicovariate model (NDVI)
pred.psi.NDVI<- predict(m.psiNDVI.pEff, 
                        newdata = data.frame(NDVI= sort(scale(sc_t2$NDVI)), "state"))

plot(sort(sc_t$NDVI), pred.psi.NDVI$Predicted, type="l", xaxt="s", 
     cex.lab=1.3,	xlab= "NDVI", ylab= "Probability of Occupancy", 
     main = "NDVI vs. Probability of Occupancy", lwd=2, col="blue", ylim= c(0,1))

# axis(side = 1, at = sort(scale(sc_t2$NDVI)), labels = round(sort(sc_t$NDVI)), 0)
points(sort(sc_t$NDVI), pred.psi.NDVI$lower, type="l", lty=2)
points(sort(sc_t$NDVI), pred.psi.NDVI$upper, type="l", lty=2)
dev.off()
#pdf("Tapir_unicovElev_Mt_total.pdf")

# Multivariate Models

# 2 covariates
m.psiTempElev.pEff <- occu(~Eff~AvgMaxTemp + Elev, umf)
m.psiTempNDVI.pEff <- occu(~Eff~NDVI + AvgMaxTemp, umf)
m.psiTempRoad.pEff <- occu(~Eff~d.Road + AvgMaxTemp, umf)
m.psiTempPrecip.pEff <- occu(~Eff~Precip + AvgMaxTemp, umf)
m.psiElevNDVI.pEff <- occu(~Eff~NDVI + Elev, umf)
m.psiElevRoad.pEff <- occu(~Eff~Elev + d.Road, umf)
m.psiElevPrecip.pEff <- occu(~Eff~Elev + Precip, umf)
m.psiNDVIroad.pEff <- occu(~Eff~NDVI + d.Road, umf)
m.psiNDVIprecip.pEff <- occu(~Eff~NDVI + Precip, umf)
m.psiPrecipRoad.pEff <- occu(~Eff~Precip + d.Road, umf)

detList2 <- fitList(mod0,
                    m.psiTempElev.pEff, 
                    m.psiTempNDVI.pEff,
                    m.psiTempRoad.pEff,
                    m.psiTempPrecip.pEff, 
                    m.psiElevNDVI.pEff, 
                    m.psiElevRoad.pEff, 
                    m.psiElevPrecip.pEff, 
                    m.psiNDVIroad.pEff, 
                    m.psiNDVIprecip.pEff, 
                    m.psiPrecipRoad.pEff)
modSel(detList2)

# 3 covariates
m.psiTempElevNDVI.pEff <- occu(~Eff~AvgMaxTemp + Elev + NDVI, umf) 
m.psiTempElevRoad.pEff <- occu(~Eff~Elev + AvgMaxTemp + d.Road, umf) 
m.psiTempElevPrecip.pEff <- occu(~Eff~Elev + AvgMaxTemp + Precip, umf) 
m.psiTempNDVIRoad.pEff <- occu(~Eff~NDVI + AvgMaxTemp + d.Road, umf) 
m.psiTempNDVIPrecip.pEff <- occu(~Eff~NDVI + Precip + AvgMaxTemp, umf) 
m.psiTempRoadPrecip.pEff <- occu(~Eff~AvgMaxTemp + d.Road + Precip, umf) 
m.psiElevRoadNDVI.pEff <- occu(~Eff~Elev + d.Road + NDVI, umf) 
m.psiElevPrecipNDVI.pEff <- occu(~Eff~NDVI + Elev + Precip, umf) 
m.psiElevRoadPrecip.pEff <- occu(~Eff~Elev + d.Road + Precip, umf) 
m.psiNDVIPrecipRoad.pEff <- occu(~Eff~Precip + d.Road + NDVI, umf) 

detList3 <- fitList(mod0,
                    m.psiTempElevNDVI.pEff,
                    m.psiTempElevRoad.pEff,
                    m.psiTempElevPrecip.pEff,
                    m.psiTempNDVIRoad.pEff,
                    m.psiTempNDVIPrecip.pEff,
                    m.psiTempRoadPrecip.pEff,
                    m.psiElevRoadNDVI.pEff,
                    m.psiElevPrecipNDVI.pEff,
                    m.psiElevRoadPrecip.pEff,
                    m.psiNDVIPrecipRoad.pEff)

modSel(detList3)

# 4 covariates 
m.psiTempElevNDVIRoad.pEff <- occu(~Eff~AvgMaxTemp + Elev + NDVI + d.Road, umf) 
m.psiTempElevNDVIPrecip.pEff <- occu(~Eff~Elev + AvgMaxTemp + NDVI + Precip, umf) 
m.psiTempElevRoadPrecip.pEff <- occu(~Eff~Elev + AvgMaxTemp + Precip + d.Road, umf) 
m.psiTempNDVIRoadPrecip.pEff <- occu(~Eff~NDVI + AvgMaxTemp + d.Road + Precip, umf) 
m.psiElevNDVIPrecipRoad.pEff <- occu(~Eff~NDVI + Precip + Elev + d.Road, umf) 

detList4 <- fitList(mod0,
                    m.psiTempElevNDVIRoad.pEff,
                    m.psiTempElevNDVIPrecip.pEff,
                    m.psiTempElevRoadPrecip.pEff,
                    m.psiTempNDVIRoadPrecip.pEff,
                    m.psiElevNDVIPrecipRoad.pEff)

modSel(detList4)

# 5 covariates 
m.psiTempElevNDVIRoadPrecip.pEff <- occu(~Eff~AvgMaxTemp + Elev + NDVI + d.Road + Precip, umf) 

detList5 <- fitList(mod0, m.psiTempElevNDVIRoadPrecip.pEff)

modSel(detList5)

# HEAT MAP
new_data = data.frame(Temp = sc_t2$AvgMaxTemp, d.Road = sc_t2$d.Road)
pred <- predict(m.psiTempRoad.pEff, newdata = new_data, "state")

occupancy_data = data.frame(Long = sc_t2$Long, Lat = sc_t2$Lat, Occupancy = pred$Predicted)

ggplot() +
  geom_tile(data = occupancy_data, aes(x = Long, y = Lat, fill = Occupancy), width = 0.01, height = 0.01) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(fill = "Occupancy") +
  coord_fixed(xlim = c(min(occupancy_data$Long), max(occupancy_data$Long)), 
              ylim = c(min(occupancy_data$Lat), max(occupancy_data$Lat))) +
  theme_bw()












