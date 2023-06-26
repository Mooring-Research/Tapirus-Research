###Single Species Solar Circadian Plots###

library(astroFns)
#Make the density plot
densityPlot(bairdSunData$solar,
            xscale = NA,
            xlab = "Solar Position",
            xaxt = "n",
            yaxt = "n",
            main = "Baird's Circadian Rhythm",
            extend= NULL,
            xcenter = c("noon"),
            rug = TRUE)

rise<- pi/2
set<- 3*pi/2
midnight<- 2* pi


#Labeling axis
axis(
  side = 1,
  at = c(0, pi/2, pi, 3*pi/2, 2*pi),
  labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
)

#Add rug
rug(bairdSunData$solar)

#Add RAI value
mtext(expression(bold("RAI = 46.63")), side=3, line=0, at=0.3, adj=0.5, cex=1.1)
