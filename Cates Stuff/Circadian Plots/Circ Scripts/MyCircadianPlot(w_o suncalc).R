###Overlap plot###
overlapPlot(hms2rad(Malayan_hour$hour), hms2rad(Mountain_hour$hour), xcenter = c("noon"))

###Density plot for single species###
#For Malayan
densityPlot(hms2rad(Malayan_hour$hour), 
            xcenter = c("noon"), 
            main = "Malayan Tapir Circadian Plot", 
            extend = NULL)

#For Mountain
densityPlot(hms2rad(Mountain_hour$hour), 
            xcenter = c("noon"), 
            main = "Mountain Tapir Circadian Plot", 
            extend = NULL)

#For Lowland
densityPlot(hms2rad(Lowland_hour$hour), 
            xcenter = c("noon"), 
            main = "Lowland Tapir Circadian Plot", 
            extend = NULL)

#For Baird's (we first have to extract only the Baird's Tapir among the other species in the data set, use the subset() function)
bairdssub <- subset(Bairds_hour, Bairds_hour$Common == "Baird's Tapir")

densityPlot(hms2rad(bairdssub$Time), 
            xcenter = c("noon"), 
            main = "Baird's Tapir Circadian Plot", 
            extend = NULL)
