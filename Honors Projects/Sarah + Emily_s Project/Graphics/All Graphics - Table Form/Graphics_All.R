###=================================================================================###
### Complete Graphic Table of Predicted Occupancy for all Four Tapir Species        ###
### Honor's Project - 2023	        							                                	###
### 9 March 2023 											                                              ###
### Sarah Turcic        										                                        ###
###=================================================================================###


rm(list=ls())

#set working directory
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Graphics")

#load libraries
library("ggpubr")
library("ggplot2")
library("unmarked")
library("grid")

#Define two themes to account for exterior and interior graphs/ axis labels
#Road will be the graphic with y-axis labels
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

#Define Interior Theme - all other graphics
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
      axis.text.y = element_blank(), #no y-axis labels for interior graphs
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      
      
      # legend at the bottom 6)
      legend.position = "bottom",
      
      #Title
      plot.title = element_text(colour = "black", face = "bold" , size = 10, hjust = 0.5, vjust = 3),
      
      #line
      line = element_line(colour = "palegreen4")
      
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
Am_sc_t<- read.csv("cv_t_AM_v2.csv")

#scale covariates
Am_sc_t2<- cbind(Am_sc_t[,1:4], round(scale(Am_sc_t[,5:14]),3))

#read in precipitation covs
precip <- read.csv("Lowland_Mean_Annual_Precipitation.csv")
new <- cbind(Am_sc_t2, precip)
new <- new[,c(1:14,20)]
new <- cbind(Am_sc_t2, round(scale(new[,15]),3))
colnames(new)[15] <- "Precip"
Am_sc_t2 <- new

#ensure rownames match
rownames(Am_tapir) == rownames(Am_eff)
rownames(Am_eff) == Am_sc_t2$Station

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

####plots####

#HFI
Am_pred.psi.hfi<- predict(Am_m.psiHFI.pEff, 
                          newdata= data.frame(HFI= sort(scale(Am_sc_t2$HFI))), "state")

#Plot the results
Am_HFI <- ggplot(Am_pred.psi.hfi, aes(x = sort(scale(Am_sc_t2$HFI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "gray75") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.068", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #geom_text(x=Inf,y=Inf,hjust=1,vjust=1,label="P=0.05", font="italic") +
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Human Footprint Index")+
  theme_tapir2() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-0.93, 0.445, 1.82, 3.195, 4.3), labels = c(0, 2, 4, 6, 8))+
  coord_cartesian(ylim = c(0,1))
#-0.93 - 4.57
#0 - 9

### Distance to Road ###

Am_pred.psi.road<- predict(Am_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(Am_sc_t2$d.Road))), "state")

#Plot the results
Am_Road <- ggplot(Am_pred.psi.road, aes(x = sort(scale(Am_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") +
  #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.055", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Distance to Road (m)")+
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.021, -0.3995, 0.222, 0.8435, 1.465), labels = c(300, 25000, 50000, 75000, 100000))+
  coord_cartesian(ylim = c(0,1))
#-1.021 to 1.465
#324 - 106,043

#Elev
# Plotting top Unicovariate model (Elev)
Am_pred.psi.elev<- predict(Am_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Am_sc_t$Elev))), "state")

#Plot the results
Am_Elev <- ggplot(Am_pred.psi.elev, aes(x = sort(scale(Am_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.22", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Elevation (m)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.45, -0.3033, 0.8433, 1.99), labels = c(50, 100, 150, 200))+
  coord_cartesian(ylim = c(0,1))
#-1.45 - 1.99
#48.14 - 204.00

#Precip
#Plotting top Unicovariate model (Precip)
Am_pred.psi.precip<- predict(Am_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Am_sc_t2$Precip))), "state")

#Plot the results
Am_Precip <- ggplot(Am_pred.psi.precip, aes(x = sort(scale(Am_sc_t2$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.20", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Precipitation (mm)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.07, -0.724, -0.379, -0.034, 0.311, 0.656, 1.00, 1.347), labels = c(125, 150, 175, 200, 225,250,275, 300))+
  coord_cartesian(ylim = c(0,1))


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


###plots###

#distance to road
# Plotting top Unicovariate model (d.Road)
CR_pred.psi.road<- predict(CR_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(CR_sc_t2$d.Road))), "state")

#Plot the results
CR_Road <- ggplot(CR_pred.psi.road, aes(x = sort(scale(CR_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.09, -0.1175,  0.855,  1.8275, 2.7), labels = c(100, 5000, 10000, 15000, 20000))+
  coord_cartesian(ylim = c(0,1))
#-1.09 - 2.80
#94.14-24410.9

# HFI
CR_pred.psi.hfi<- predict(CR_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(CR_sc_t$HFI))), "state")

#Plot the results
CR_HFI <- ggplot(CR_pred.psi.hfi, aes(x = sort(scale(CR_sc_t$HFI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "gray75") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.002", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "HFI", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - HFI")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1, 0, 1, 2,  2.9), labels = c(3, 6, 9, 12, 18))+
  coord_cartesian(ylim = c(0,1))

#Elev
# Plotting top Unicovariate model (Elev)
CR_pred.psi.elev<- predict(CR_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(CR_sc_t$Elev))), "state")

#Plot the results
CR_Elev <- ggplot(CR_pred.psi.elev, aes(x = sort(scale(CR_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.004", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.85, -1.0175, -0.185, 0.6475, 1.4), labels = c(20, 900, 1700, 2500, 3400))+
  coord_cartesian(ylim = c(0,1))
#-1.85-1.487
#23-2443

#Precip
# Plotting top Unicovariate model (Precip)
CR_pred.psi.precip<- predict(CR_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(CR_sc_t$Precip))), "state")

#Plot the results
CR_Precip <- ggplot(CR_pred.psi.precip, aes(x = sort(scale(CR_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.61", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.599, -0.352, 0.895, 2.142, 3.389), labels = c(150, 200, 250, 300, 350))+
  coord_cartesian(ylim = c(0,1)) 
#-1.599433  3.389610
#152.25 353.75


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

###Plots

#distance to road
# Plotting top Unicovariate model (d.Road)
Mt_pred.psi.road<- predict(Mt_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(Mt_sc_t2$d.Road))), "state")

#Plot the results
Mt_Road <- ggplot(Mt_pred.psi.road, aes(x = sort(scale(Mt_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.80", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-2.3, -1.2, -0.117, 0.9745, 2), labels = c(750, 2500, 5000, 7500, 10000))+
  coord_cartesian(ylim = c(0,1))
#750 to 10345m from road
#-2.366 - 2.021

#Elev
# Plotting top Unicovariate model (Elev)
Mt_pred.psi.elev<- predict(Mt_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Mt_sc_t$Elev))), "state")

#Plot the results
Mt_Elev <- ggplot(Mt_pred.psi.elev, aes(x = sort(scale(Mt_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.23", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.6, -0.7, 0.2, 1.1, 1.8), labels = c(1500, 2000, 2500, 3000, 3500))+
  coord_cartesian(ylim = c(0,1))
#1615 m to 3678m
#-1.59 - 2.00

#Precip
# Plotting top Unicovariate model (Precip)
Mt_pred.psi.precip<- predict(Mt_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Mt_sc_t$Precip))), "state")

#Plot the results
Mt_Precip <- ggplot(Mt_pred.psi.precip, aes(x = sort(scale(Mt_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.91", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.78, -0.8575, 0.065, 0.98, 1.8), labels = c(80, 90, 100, 110, 120))+
  coord_cartesian(ylim = c(0,1))
#-1.78 - 1.91
#82 - 118


# Unable to use HFI, all the same so only one dot on a graph


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


###Plots

#Elev
# Plotting top Unicovariate model (Elev)
Ma_pred.psi.elev<- predict(Ma_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Ma_sc_t$Elev))), "state")

#Plot the results
Ma_Elev <- ggplot(Ma_pred.psi.elev, aes(x = sort(scale(Ma_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.3,-0.315, 0.81, 1.935, 3.0), labels = c(300, 650, 1000, 1350, 1750))+
  coord_cartesian(ylim = c(0,1))
#289 1763m
#-1.441897  3.060600

#Precip
# Plotting top Unicovariate model (Precip)
Ma_pred.psi.precip<- predict(Ma_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Ma_sc_t$Precip))), "state")

#Plot the results
Ma_Precip <- ggplot(Ma_pred.psi.precip, aes(x = sort(scale(Ma_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.423, -0.567, 0.288, 1.14, 2.00), labels = c(180, 200, 220, 240, 260))+
  coord_cartesian(ylim = c(0,1))
#-1.423178  2.005328
#186.3333 253.3333

# HFI
Ma_pred.psi.hfi<- predict(Ma_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(Ma_sc_t$HFI))), "state")

#Plot the results
Ma_HFI <- ggplot(Ma_pred.psi.hfi, aes(x = sort(scale(Ma_sc_t$HFI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "gray75") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.68", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "HFI", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - HFI")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-0.209,0.759, 1.727, 2.69, 3.6647, 4.633, 5.601, 6.57), labels = c(0,1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,1))
#-0.2090092  6.5705417
#0-7

#distance to road
# Plotting top Unicovariate model (d.Road)
Ma_pred.psi.road<- predict(Ma_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(Ma_sc_t2$d.Road))), "state")

#Plot the results
Ma_Road <- ggplot(Ma_pred.psi.road, aes(x = sort(scale(Ma_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.003", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.48, -0.49, 0.435, 1.36, 2.27), labels = c(10, 5000, 10000, 15000, 18000))+
  coord_cartesian(ylim = c(0,1))
#7.9041525 18060.96 from road
#-1.424047  2.294052


###########################################################################################################
############Adding all plots together######################################################################


commonplot <- ggarrange(Am_Road, Am_Elev, Am_Precip, Am_HFI, CR_Road, CR_Elev, CR_Precip, CR_HFI, Ma_Road, Ma_Elev, Ma_Precip, Ma_HFI, Mt_Road, Mt_Elev, Mt_Precip,
                        labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P"),
                        ncol = 4, nrow = 4)

#commonplot
#ploat titles are manually spaced, so you'll have to get the correct pixel dimensions for everything to line up correctly
#I didn't add the labels as y or x labels because it messed with the spacing of the text

annotate_figure(commonplot, 
                left = text_grob("          Mountain Tapir 𝜓           Malayan Tapir 𝜓            Baird's Tapir 𝜓            Lowland Tapir 𝜓", rot = 90, size = 14, face = "bold" ),
                bottom = text_grob("    Distance to Road (m)                             Elevation (m)                               Precipitation (mm)                       Human Footprint Index", color = "black", face = "bold", size = 14))
