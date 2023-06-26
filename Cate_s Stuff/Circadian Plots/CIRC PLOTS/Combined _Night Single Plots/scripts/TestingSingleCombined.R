#Set margin sizes
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#Create a matrix layout and add each graph
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plotActivitySingleCombined(animal1, yAxis=FALSE,letter="Baird's")
mtext(expression(bold("%Night = 0.82")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5)
plotActivitySingleCombined(animal2, yAxis=FALSE,letter="Lowland")
mtext(expression(bold("%Night = 0.83")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5)
plotActivitySingleCombined(animal3, yAxis=TRUE,letter="Malayan")
mtext(expression(bold("%Night = 0.88")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5)
densityPlotCustom(
  animal4
  ,main=""
  ,xlab=""
  ,ylab=""
  ,rug=TRUE
  #,xaxt="n"#remove axis labels so rewrite with custom
  ,yaxt="n"#remove axis labels so rewrite with custom
)
axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=0) 
axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=2, hadj=0.5) 
axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=1)

#add y-axis so all tickmarks and choose which labels
axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=2)#all tick marks
axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=2)
par(xpd=TRUE)
mtext("Mountain",side=3,font=2,cex=0.5,at=2,line=0.4,xpd=TRUE,col="blue")
mtext(expression(bold("%Night = 0.63")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5)

#Add overall axis labels
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)


#####
#####
densityPlotCustom(
  animal4
  ,main="Mountain Tapir Circadian Plot"
  ,xlab="Time of Day"
  ,ylab="Temporal Density"
  ,rug=TRUE
  #,xaxt="n"#remove axis labels so rewrite with custom
  ,yaxt="n"#remove axis labels so rewrite with custom
)
#add x-axis so labels within boundaries of plot and spaced
axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)

#add y-axis so all tickmarks and choose which labels
axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)