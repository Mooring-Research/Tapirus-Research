#Single Plots Combined ----
library(overlap)

#Identify sizes
xaxisSize <- 0.6
yaxisSize <- 0.7
legendSize <- 0.8
legendplacement <- 1.5
textSize=0.7

#New plot function for combined plots
plotActivitySingleCombined <- function(animal1, yAxis, letter) {
  
  par(ps=30, lwd=1.5, cex.axis=0.85)
  densityPlotCustom(
    animal1,
    main="",
    rug=TRUE
    ,font.lab=2
    ,extend=NULL
    ,xlab=""
    ,ylab=""
    ,yaxt="n"
  )
  if(yAxis){
    axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=0) 
    axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=2, hadj=0.5) 
    axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=1) 
  }
  else{
    axis(1, at=c(0), labels=c(""), cex.axis=xaxisSize, font=2, hadj=0) 
    axis(1, at=c(6, 12, 18), labels=c("", "", ""), cex.axis=xaxisSize, font=2, hadj=0.5) 
    axis(1, at=c(24), labels=c(""), cex.axis=xaxisSize, font=2, hadj=1) 
  }
  
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=2)
  
  #axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=2)
  
  axis(2,at=axTicks(2)[c(2,length(axTicks(2))-1)],labels=axTicks(2)[c(2,length(axTicks(2))-1)],cex.axis=yaxisSize,font=2)
  
  #mtext(paste0(ind.data[ind.data$Species==animalname1, "Common"][1]," (",animalname1,")"), side=3,adj=0, line=0.4, cex=1, font=2) 
  #mtext(bairdSunData$solar[1], side=3,adj=0, cex=textSize, font=2) #line=0.4
  #mtext(paste0("Day:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],2),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  # mtext(paste0("Night:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],2),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  # mtext(paste0("D/N:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],0),"/",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],0),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  #mtext(paste0("%D,N:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],0),",",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],0)), side=3,adj=1, line=0.4, cex=0.7, font=2)
  mtext(letter,side=3,font=2,cex=0.5,at=2,line=0.4,xpd=TRUE,col="blue")
}

#Set margin sizes
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#Create a matrix layout and add each graph
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plotActivitySingleCombined(animal1, yAxis=FALSE,letter="Baird's")
plotActivitySingleCombined(animal2, yAxis=FALSE,letter="Lowland")
plotActivitySingleCombined(animal3, yAxis=TRUE,letter="Malayan")
plotActivitySingleCombined(animal4, yAxis=TRUE,letter="Mountain")
par(xpd=TRUE)

#Add overall axis labels
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)
