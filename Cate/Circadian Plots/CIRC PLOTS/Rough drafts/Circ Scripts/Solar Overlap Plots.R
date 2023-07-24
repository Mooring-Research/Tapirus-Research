###Making overlap plots for Tapir using solar time###
require(overlap)
require(bayestestR)

#Make the plot using solar time
p(bairdSunData$hour, 
            mountainSunData$hour,
            xscale = NA,
            xlab = "Solar Position",
            xaxt = "n",
            main = "Baird's and Mountain Temporal Density",
            extend= NULL,
            xcenter = c("noon"),
            rug = TRUE
)

#night shading
polygon(c(0,rise, rise, 0), c(0, 0, 1, 1), col = rgb(0.77, 0.86, 0.92, alpha = 0.6), border = NA)
polygon(c(set,midnight, midnight, set), c(0, 0, 1, 1), col = rgb(0.77, 0.86, 0.92, alpha = 0.6), border = NA)

rise<- pi/2
set<- 3*pi/2
midnight<- 2* pi


#Labeling axis
axis(
  side = 1,
  at = c(0, pi/2, pi, 3*pi/2, 2*pi),
  labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
)


#Add a legend
legend(x = "top", 
       legend = c("Baird's", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Calculate coefficient of overlap (∆)
overlap(bairdSunData$solar, mountainSunData$solar)

#Add this value (∆) to the plot
mtext("∆=0.76", side=3, line=0, at=-0.03, adj=-8.5, cex=1)
