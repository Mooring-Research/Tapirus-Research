###Overlap density plot for two species###
require(overlap)

overlapPlot(hms2rad(MalayanCirc$Time), 
            hms2rad(Mountaincirc$Time), 
            xcenter = c("noon"),
            main = "Malayan and Mountain Tapir Overlap Plot",
            rug = TRUE)

#Add a legend
legend(x = "top", 
       legend = c("Malayan", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Determine the coefficient of overlap (∆)
require(bayestestR)

overlap(hms2rad(MalayanCirc$Time), hms2rad(Mountaincirc$Time))

#Add this value (∆) to the plot
mtext("∆=0.66", side=3, line=0, at=-0.03, adj=-8, cex=1)



###Density plots for single species###

#Extracting desired tapir species from a multispecies dataset (ex. shown is Baird's)
bairdssubm <- subset(Master_5_31_22_, Master_5_31_22_$Common == "Baird's Tapir")

#hms2rad function converts hours/mins/secs to radians#

#For Malayan
densityPlot(hms2rad(MalayanCirc$Time), 
            xcenter = c("noon"), 
            main = "Malayan Tapir Circadian Plot", 
            extend = NULL,
            rug = TRUE)

#For Mountain
densityPlot(hms2rad(Mountaincirc$Time), 
            xcenter = c("noon"), 
            main = "Mountain Tapir Circadian Plot", 
            extend = NULL,
            rug = TRUE)

#For Lowland
densityPlot(hms2rad(lowsub$hour),
            xcenter = c("noon"),
            main = "Lowland Tapir Circadian Plot",
            extend = NULL,
            rug = TRUE)

#For Baird's
densityPlotCustom(bairdSunData$solar, 
            xcenter = c("noon"), 
            main = "Baird's Tapir Circadian Plot", 
            extend = NULL,
            xaxt = "n",
            yaxt = "n",
            rug = TRUE)
