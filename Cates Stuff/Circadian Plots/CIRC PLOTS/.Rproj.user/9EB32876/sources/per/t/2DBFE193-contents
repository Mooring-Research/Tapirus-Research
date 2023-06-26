#Set margin sizes
library(overlap)

#Identify sizes
xaxisSize <- 0.6
yaxisSize <- 0.7
legendSize <- 0.8
legendplacement <- 1.5
textSize=0.7

animal1 <- bairdSunData$solar
animal2 <- lowlandSunData$solar
animal3 <- malayanSunData$solar
animal4 <- mountainSunData$solar

#Editing density plot function so it includes nighttime shading 
densityPlotCustom <- function (A, xscale = 24, 
                               xcenter = c("noon", "midnight"), 
                               add = FALSE, 
                               rug = FALSE, 
                               extend = NULL, 
                               n.grid = 128, 
                               kmax = 3, 
                               adjust = 1, 
                               lwd=2, 
                               nightShade=TRUE, 
                               xaxsSelect="i", ...) {
  
  isMidnt <- match.arg(xcenter) == "midnight"
  bw <- getBandWidth(A, kmax = kmax)/adjust
  if (is.na(bw)) 
    stop("Bandwidth estimation failed.")
  if (is.null(extend)) {
    xx <- seq(0, 2 * pi, length = n.grid)
  }
  else {
    xx <- seq(-pi/4, 9 * pi/4, length = n.grid)
  }
  if (isMidnt) 
    xx <- xx - pi
  
  densA <- densityFit(A, xx, bw)
  xsc <- if (is.na(xscale)) 
    1
  else xscale/(2 * pi)
  toPlot <- cbind(x = xx * xsc, y = densA/xsc)
  dots <- list(...)
  if (length(dots) == 1 && class(dots[[1]]) == "list") 
    dots <- dots[[1]]
  defaultArgs <- list(main = deparse(substitute(A)), 
                      bty = "o", 
                      type = "l", 
                      xlab = "Time", 
                      ylab = "Density", 
                      lwd=2, 
                      font.axis=2, 
                      ylim =c(0, max(toPlot[, "y"])))
  
  useArgs <- modifyList(defaultArgs, dots)
  if (!add) {
    selPlot <- names(useArgs) %in% c(names(as.list(args(plot.default))), 
                                     names(par(no.readonly = TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- toPlot
    plotArgs$y <- NULL
    plotArgs$type <- "n"
    plotArgs$xaxt <- "n"
    plotArgs$xaxs <- xaxsSelect #custom addition. "i" means no white space between data and xaxis
    do.call(plot, plotArgs, quote = TRUE)
    abline(h = 0, col = "grey")
    edge <- par("usr")
    if(nightShade){
      rect(edge[1],edge[3],6,edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours before dawn. 0.6 transparency (changed to 0.4 for overlap)
      rect(18,edge[3],edge[2],edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours after sunset. 0.6 transparency (changed to 0.4 for overlap)
    }
    
    if (!is.null(extend)) {
      if (isMidnt) {
        wrap <- c(-pi, pi) * xsc
      }
      else {
        wrap <- c(0, 2 * pi) * xsc
      }
      rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], 
                                                   edge[2]), rep(edge[4], 2), border = NA, col = extend)
      box(bty = useArgs$bty)
    }
  }
  selPlot <- names(useArgs) %in% names(par(no.readonly = TRUE))
  plotArgs <- useArgs[selPlot]
  plotArgs$x <- toPlot
  plotArgs$y <- NULL
  do.call(lines, plotArgs, quote = TRUE)
  if (rug) {
    if (isMidnt) 
      A <- ifelse(A < pi, A, A - 2 * pi)
    rug(A * xsc, ...)
  }
  return(invisible(as.data.frame(toPlot)))
}

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

par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#Create a matrix layout and add each graph
#Mountain was being weird so I just added it as a densityPlotCustom instead of plotActivitySingleCombined
# - (because of this the steps are all more manual for Mountain)
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
#Adding axis labels for Mountain
axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=0) 
axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=2, hadj=0.5) 
axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=1)

#For Mountain: add y-axis so all tickmarks and choose which labels
axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=2)#all tick marks
axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=2)
par(xpd=TRUE)
#Add blue title and %Night for Mountain
mtext("Mountain",side=3,font=2,cex=0.5,at=2,line=0.4,xpd=TRUE,col="blue")
mtext(expression(bold("%Night = 0.63")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5)

#Add overall axis labels
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)
