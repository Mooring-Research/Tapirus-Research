###=================================================================================###
### Combined Predicted Occupancy Graphics                                           ###
### Honor's Project - 2023	        							                                	###
### 12 April 2023 											                                            ###
### Sarah Turcic        										                                        ###
###=================================================================================###


#clear
rm(list=ls())

#set working directory
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Graphics")

#load libraries
library("ggpubr")
library("ggplot2")
library("unmarked")
library("grid")

#Define Theme
theme_tapir_Road <- function() {
  
  font <- "sans"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      # add border 1)
      # panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
      # color background 2)
      panel.background = element_rect(fill = NA),
      
      # modify text, axis and colour 4) and 5)
      axis.text.x = element_text(colour = "black", face = "italic"),
      axis.text.y = element_text(colour = "black", face = "italic"),
      axis.title.y = element_text(colour = "black", angle = 90),
      axis.ticks = element_line(colour = "black"),
      
      # legend at the bottom 6)
      legend.position = "bottom",
      
      #Title
      plot.title = element_text(colour = "black", face = "bold" , size = 10, hjust = 0.5, vjust = 3),
      
      #line
      line = element_line(colour = "palegreen4")
      
    )
}

#Define Interior Theme
theme_tapir2 <- function() {
  
  font <- "sans"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      # add border 1)
      # panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
      # color background 2)
      panel.background = element_rect(fill = NA),
      
      # modify text, axis and colour 4) and 5)
      axis.text.x = element_text(colour = "black", face = "italic"),
      axis.text.y = element_text(colour = "black", face = "italic"),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      axis.title = element_text(face = "bold", size = 15),
      
      
      # legend at the bottom 6)
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(colour = "black", size = 15),
      
      #Title
      plot.title = element_text(colour = "black", face = "bold" , size = 10, hjust = 0.5, vjust = 3),
      
      #line
      #line = element_line(colour = "palegreen4")
      
    )
}

############################################################################################################
##########################Lowland Tapir#####################################################################

###Read in Variables

#read in tapir occurance records
Am_tapir<- readRDS("tapir_AM.rds")

#read in effort table
Am_eff<- readRDS("eff_AM.rds")

#read in covariate table
Am_sc_t<- read.csv("Am_T_Covs(all).csv")

#scale covariates
Am_sc_t2<- cbind(Am_sc_t[,2:5], round(scale(Am_sc_t[,6:16]),3))

#read in precipitation covs
#precip <- read.csv("Lowland_Mean_Annual_Precipitation.csv")
#new <- cbind(Am_sc_t2, precip)
#new <- new[,c(1:14,20)]
#Am_sc_t <- new #set this to get unscaled version for graphs
#new <- cbind(Am_sc_t2, round(scale(new[,15]),3))
#colnames(new)[15] <- "Precip"
#colnames(Am_sc_t)[15] <- "Precip"
#Am_sc_t2 <- new

#ensure rownames match
#rownames(Am_tapir) == rownames(Am_eff)
#rownames(Am_eff) == Am_sc_t2$Station

###Establish Unmarked Data Frame

Am_umf<- unmarkedFrameOccu(y=Am_tapir, siteCovs=Am_sc_t2, obsCovs=list(Eff=Am_eff))
summary(Am_umf) #67 sites with detection


###Running Models

# Running Null model
Am_mod0 <- occu(~1~1, Am_umf)  # Null Model

# Running model with Eff as survey covariate
Am_m.psi1.pEff<- occu(~Eff~1, Am_umf) 

# Running unicovariate models
Am_m.psiElev.pEff<- occu(~Eff~Elev , Am_umf)
summary(Am_m.psiElev.pEff)
Am_m.psiHFI.pEff<- occu(~Eff~HFI , Am_umf)
summary(Am_m.psiHFI.pEff)
Am_m.psiRoad.pEff<- occu(~Eff~d.Road , Am_umf)   
summary(Am_m.psiRoad.pEff)
Am_m.psiPrecip.pEff<- occu(~Eff~Precip , Am_umf)
summary(Am_m.psiPrecip.pEff)

####predictions####

#HFI
Am_pred.psi.hfi<- predict(Am_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(Am_sc_t2$HFI))), "state")
#Distance to Road

Am_pred.psi.road<- predict(Am_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(Am_sc_t2$d.Road))), "state")

#Elev
Am_pred.psi.elev<- predict(Am_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Am_sc_t$Elev))), "state")

#Precip
Am_pred.psi.precip<- predict(Am_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Am_sc_t2$Precip))), "state")


############################################################################################################
##########################Baird's Tapir#####################################################################

#load in data
CR_tapir_t<- readRDS("tapir_CR.rds")
CR_eff_t<- readRDS("eff_CR.rds")
CR_sc_t<- read.csv("CR_cv_t3.csv")

#read in precip
precip <- read.csv("CR_Mean_Annual_Precipitation.csv")

#join data
new <- cbind(CR_sc_t[,2:14], precip[,6])
CR_sc_t <- new

#rename
colnames(CR_sc_t)[14] <- "Precip"

#scale
CR_sc_t2<- cbind(CR_sc_t[,1:4], round(scale(CR_sc_t[,5:14]),3))

rownames(CR_tapir_t) == rownames(CR_eff_t)
rownames(CR_eff_t) == CR_sc_t$Station

CR_umf<- unmarkedFrameOccu(y=CR_tapir_t, siteCovs=CR_sc_t2, obsCovs=list(Eff=CR_eff_t))
summary(CR_umf)

# Running Null model
CR_mod0 <- occu(~1~1, CR_umf)  # Null Model
summary(CR_mod0)
plogis( -0.129)  	# Probability of occupancy
plogis(  -0.832)	# Probability of detection
22/141	#Naive occupancy

# Running model with Eff as survey covariate
CR_m.psi1.pEff<- occu(~Eff~1, CR_umf) 
summary(CR_m.psi1.pEff)

# Running unicovariate models
CR_m.psiElev.pEff<- occu(~Eff~Elev , CR_umf)	# SIGNIFICANT!!
summary(CR_m.psiElev.pEff)
CR_m.psiHFI.pEff<- occu(~Eff~HFI , CR_umf) 	# SIGNIFICANT!!
summary(CR_m.psiHFI.pEff)
CR_m.psiRoad.pEff<- occu(~Eff~d.Road , CR_umf)	# SIGNIFICANT!!
summary(CR_m.psiRoad.pEff)
CR_m.psiPrecip.pEff<- occu(~Eff~Precip , CR_umf)
summary(CR_m.psiPrecip.pEff)


###predictions###

#distance to road
CR_pred.psi.road<- predict(CR_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(CR_sc_t2$d.Road))), "state")

#HFI
CR_pred.psi.hfi<- predict(CR_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(CR_sc_t$HFI))), "state")

#Elev
CR_pred.psi.elev<- predict(CR_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(CR_sc_t$Elev))), "state")

#Precip
CR_pred.psi.precip<- predict(CR_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(CR_sc_t$Precip))), "state")


############################################################################################################
##########################Mountain Tapir####################################################################


###Read in Variables

#read in tapir occurance records
Mt_tapir_t<- readRDS("Collapsed_Capture_Mountain_Tapir_revised_DR.rds") 
head(Mt_tapir_t)

#read in effort table
Mt_eff_t<- readRDS("Effort_Mountain_Tapir_revised_DR.rds")
head(Mt_eff_t)

#read in covariate table
Mt_sc_t<- read.csv("Mt_T_Covs4.csv") 
head(Mt_sc_t)

#edit covariate table
#Mt_sc_t <- Mt_sc_t[,c("Camr_Nm","Latitud","Longitd","S06W080.hgt","HFI_MountainTapir_EPSG4326")]
#colnames(Mt_sc_t)[colnames(Mt_sc_t) == "S06W080.hgt"] ="Elev"
#colnames(Mt_sc_t)[colnames(Mt_sc_t) == "HFI_MountainTapir_EPSG4326"] ="HFI"
Mt_sc_t <- Mt_sc_t[,-1] #get rid of random X column

#scale covariates and rename to match previous code
Mt_sc_t2<- cbind(Mt_sc_t[,1:3], round(scale(Mt_sc_t[,c(4,6,7)]),3)) #don't scale HFI because all the same
Mt_sc_t2 <- cbind(Mt_sc_t2, Mt_sc_t[5]) #add HFI back
#colnames(Mt_sc_t2)[4]="Elev"
colnames(Mt_sc_t2)[1]="Station"

#ensure rownames match
rownames(Mt_tapir_t) == rownames(Mt_eff_t)
sort(rownames(Mt_eff_t)) == sort(Mt_sc_t2$Station) #they are the same but it still says false?

#effort
sum(rowSums(Mt_eff_t, na.rm = TRUE)) #9,456 effort
mean(rowSums(Mt_eff_t, na.rm = TRUE))	#111.25 mean camera active days
sd(rowSums(Mt_eff_t, na.rm = TRUE)) 	#sd mean camera active days 30.92
sum(rowSums(Mt_tapir_t, na.rm = TRUE)) #85 independent records


#Establish Unmarked Data Frame##############################################################

Mt_umf<- unmarkedFrameOccu(y=Mt_tapir_t, siteCovs=Mt_sc_t2, obsCovs=list(Eff=Mt_eff_t))
summary(Mt_umf) #28 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
Mt_mod0 <- occu(~1~1, Mt_umf)  # Null Model
summary(Mt_mod0)
plogis( -0.595)  	# Probability of occupancy 0.35
plogis( -1.56)	# Probability of detection 0.17
28/85 #Naive occupancy - 0.3294

# Running model with Eff as survey covariate
Mt_m.psi1.pEff<- occu(~Eff~1, Mt_umf) 
summary(Mt_m.psi1.pEff)

# Running unicovariate models
#Elev
Mt_m.psiElev.pEff<- occu(~Eff~Elev , Mt_umf)
summary(Mt_m.psiElev.pEff)
#HFI
Mt_m.psiHFI.pEff<- occu(~Eff~HFI , Mt_umf) #all have same HFI, so no use for this species
summary(Mt_m.psiHFI.pEff)
#d.Road
Mt_m.psiRoad.pEff<- occu(~Eff~d.Road , Mt_umf)
summary(Mt_m.psiRoad.pEff)
#Precip
Mt_m.psiPrecip.pEff <- occu(~Eff~Precip, Mt_umf)
summary(Mt_m.psiPrecip.pEff)

###predictions###

#distance to road
Mt_pred.psi.road<- predict(Mt_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(Mt_sc_t2$d.Road))), "state")

#Elev
Mt_pred.psi.elev<- predict(Mt_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Mt_sc_t$Elev))), "state")

#Precip
Mt_pred.psi.precip<- predict(Mt_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Mt_sc_t$Precip))), "state")


#######################################################################################################################
########Malayan Tapir#################################################################################################


#Read in Variables###################################################################

#read in tapir occurance records
Ma_tapir_t<- readRDS("Collapsed_Capture_Malayan_Tapir.rds") 
head(Ma_tapir_t)

#read in effort table
Ma_eff_t<- readRDS("Effort_Malayan_Tapir.rds")
head(Ma_eff_t)

#read in covariate table
Ma_sc_t<- read.csv("Ma_T_Final_Covs.csv")
head(Ma_sc_t)

#scale covariates and rename to match previous code
Ma_sc_t2<- cbind(Ma_sc_t[,2:4], round(scale(Ma_sc_t[,c(5:8)]),3))


#Establish Unmarked Data Frame##############################################################

Ma_umf<- unmarkedFrameOccu(y=Ma_tapir_t, siteCovs=Ma_sc_t2, obsCovs=list(Eff=Ma_eff_t))
summary(Ma_umf) #150 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
Ma_mod0 <- occu(~1~1, Ma_umf)  # Null Model
summary(Ma_mod0)
plogis( 0.188)  	# Probability of occupancy 0.5468
plogis( -1.82)	# Probability of detection 0.1394
150/329 #Naive occupancy - 0.4559

# Running model with Eff as survey covariate
Ma_m.psi1.pEff<- occu(~Eff~1, Ma_umf) 
summary(Ma_m.psi1.pEff)

# Running unicovariate models
#Elev
Ma_m.psiElev.pEff<- occu(~Eff~Elev , Ma_umf)
summary(Ma_m.psiElev.pEff)
plogis( 1.019) # Probability of occupancy 0.734
plogis(0.342)	# Probability of detection 0.5846

#HFI
Ma_m.psiHFI.pEff<- occu(~Eff~HFI , Ma_umf) 
summary(Ma_m.psiHFI.pEff)
plogis(-0.0563) # Probability of occupancy 0.485928
plogis(0.339)	# Probability of detection 0.5839

#d.Road
Ma_m.psiRoad.pEff<- occu(~Eff~d.Road , Ma_umf)
summary(Ma_m.psiRoad.pEff)
plogis(0.421) # Probability of occupancy 0.604
plogis(0.339)	# Probability of detection 0.584

#Precip
Ma_m.psiPrecip.pEff <- occu(~Eff~Precip, Ma_umf)
summary(Ma_m.psiPrecip.pEff)
plogis(0.790) # Probability of occupancy 0.6878
plogis(0.341)	# Probability of detection 0.584433


###predictions###

#Elev
Ma_pred.psi.elev<- predict(Ma_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Ma_sc_t$Elev))), "state")

#Precip
Ma_pred.psi.precip<- predict(Ma_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Ma_sc_t$Precip))), "state")

# HFI
Ma_pred.psi.hfi<- predict(Ma_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(Ma_sc_t$HFI))), "state")

#distance to road
Ma_pred.psi.road<- predict(Ma_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(Ma_sc_t2$d.Road))), "state")


#############################################################################################################
#################################################################################################################
#all together now

#HFI
ggplot()+
  geom_ribbon(data = Am_pred.psi.hfi, aes(x = sort(Am_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.hfi, aes(x = sort(Am_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.hfi, aes(x = sort(CR_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.hfi, aes(x = sort(CR_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.hfi, aes(x = sort(Ma_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.hfi, aes(x = sort(Ma_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  labs(title = "Tapir Occupancy Probability and Human Footprint Index", x = "Human Footprint Index", y = "Occupancy Probability (𝜓)") + # axis labels
  theme_tapir2() +
  #scale_x_continuous(name = "Human Footprint Index", breaks = c(0, 25000, 50000, 75000, 100000), labels = c(0, 25000, 50000, 75000, 100000) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4"))
  

#d.Road
ggplot()+
  geom_ribbon(data = Am_pred.psi.road, aes(x = sort(Am_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.road, aes(x = sort(Am_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.road, aes(x = sort(CR_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.road, aes(x = sort(CR_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.road, aes(x = sort(Ma_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.road, aes(x = sort(Ma_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.road, aes(x = sort(Mt_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.road, aes(x = sort(Mt_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  labs(title = "Tapir Occupancy Probability and Distance to Road", x = "Distance to Road (m)", y = "Occupancy Probability (𝜓)") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Distance to Road (m)", breaks = c(0, 25000, 50000, 75000, 100000), labels = c(0, 25000, 50000, 75000, 100000) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))

#Elevation
ggplot()+
  geom_ribbon(data = Am_pred.psi.elev, aes(x = sort(Am_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.elev, aes(x = sort(Am_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.elev, aes(x = sort(CR_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.elev, aes(x = sort(CR_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.elev, aes(x = sort(Ma_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.elev, aes(x = sort(Ma_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.elev, aes(x = sort(Mt_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.elev, aes(x = sort(Mt_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  labs(title = "Tapir Occupancy Probability and Elevation", x = "Elevation (m)", y = "Occupancy Probability (𝜓)") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Elevation (m)", breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), labels = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))


#Precipitation
ggplot()+
  geom_ribbon(data = Am_pred.psi.precip, aes(x = sort(Am_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.precip, aes(x = sort(Am_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.precip, aes(x = sort(CR_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.precip, aes(x = sort(CR_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.precip, aes(x = sort(Ma_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.precip, aes(x = sort(Ma_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.precip, aes(x = sort(Mt_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.precip, aes(x = sort(Mt_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  labs(title = "Tapir Occupancy Probability and Precipitation", x = "Precipitation (mm)", y = "Occupancy Probability (𝜓)") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Precipitation (mm)", breaks = c(50, 100, 150, 200, 250, 300, 350), labels = c(50, 100, 150, 200, 250, 300, 350) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))



########################################################################################################################
### Adding all in one graphic ########################################################################
#HFI
hfi <- ggplot()+
  geom_ribbon(data = Am_pred.psi.hfi, aes(x = sort(Am_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.hfi, aes(x = sort(Am_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.hfi, aes(x = sort(CR_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.hfi, aes(x = sort(CR_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.hfi, aes(x = sort(Ma_sc_t$HFI), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.hfi, aes(x = sort(Ma_sc_t$HFI), y = Predicted), size = 1, colour = "black")+
  labs(x = "Human Footprint Index", y = " ") + # axis labels
  theme_tapir2() +
  #scale_x_continuous(name = "Human Footprint Index", breaks = c(0, 25000, 50000, 75000, 100000), labels = c(0, 25000, 50000, 75000, 100000) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4"))


#d.Road
road <- ggplot()+
  geom_ribbon(data = Am_pred.psi.road, aes(x = sort(Am_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.road, aes(x = sort(Am_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.road, aes(x = sort(CR_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.road, aes(x = sort(CR_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.road, aes(x = sort(Ma_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.road, aes(x = sort(Ma_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.road, aes(x = sort(Mt_sc_t$d.Road), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.road, aes(x = sort(Mt_sc_t$d.Road), y = Predicted), size = 1, colour = "black")+
  labs(x = "Distance to Road (m)", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Distance to Road (m)", breaks = c(0, 25000, 50000, 75000, 100000), labels = c(0, 25000, 50000, 75000, 100000) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))

#Elevation
elevation <- ggplot()+
  geom_ribbon(data = Am_pred.psi.elev, aes(x = sort(Am_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.elev, aes(x = sort(Am_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.elev, aes(x = sort(CR_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.elev, aes(x = sort(CR_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.elev, aes(x = sort(Ma_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.elev, aes(x = sort(Ma_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.elev, aes(x = sort(Mt_sc_t$Elev), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.elev, aes(x = sort(Mt_sc_t$Elev), y = Predicted), size = 1, colour = "black")+
  labs(x = "Elevation (m)", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Elevation (m)", breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), labels = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))


#Precipitation
precipitation <- ggplot()+
  geom_ribbon(data = Am_pred.psi.precip, aes(x = sort(Am_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Lowland Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Am_pred.psi.precip, aes(x = sort(Am_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = CR_pred.psi.precip, aes(x = sort(CR_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Baird's Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = CR_pred.psi.precip, aes(x = sort(CR_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Ma_pred.psi.precip, aes(x = sort(Ma_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Malayan Tapir"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Ma_pred.psi.precip, aes(x = sort(Ma_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  geom_ribbon(data = Mt_pred.psi.precip, aes(x = sort(Mt_sc_t$Precip), y = Predicted, ymin = lower, ymax = upper, fill = "Mountain Tapir*"), alpha = 0.6, linetype = "dashed")+
  geom_path(data = Mt_pred.psi.precip, aes(x = sort(Mt_sc_t$Precip), y = Predicted), size = 1, colour = "black")+
  labs(x = "Precipitation (mm)", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = "Precipitation (mm)", breaks = c(50, 100, 150, 200, 250, 300, 350), labels = c(50, 100, 150, 200, 250, 300, 350) )+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values=c("gray75", "thistle4", "skyblue4","darkslategray4"))

commonplot <- ggarrange(road, hfi, precipitation, elevation,
                        labels = c("A", "B", "C", "D"),
                        ncol = 2, nrow = 2,
                        legend = "right",
                        common.legend = TRUE)

#commonplot - plot everything together
annotate_figure(commonplot, 
                left = text_grob("Occupancy Probability (𝜓)", rot = 90, size = 15, face = "bold" ))