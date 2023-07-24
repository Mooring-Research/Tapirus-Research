#Individual Single Plots ----
#adjust these to change text size of labels on the axis and species name
library(overlap)
xaxisSize = 0.7
yaxisSize = 0.7
textSize = 0.5

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

plotDensity <- function(animal4) {
  par(ps=20, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))#3rd 3
  densityPlotCustom(
    animal1
    ,main="Baird's Tapir Circadian Plot"
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
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
 
  #Add % Night based species and calculated values (see below for how to calculate)
  mtext("%Night=0.82", side=3, line=0, at=-0.03, adj=-4.5, cex=1)

  ###Calculating %Night for each species###
  #Baird's
  bairdNight<-(length(bairdSunData$solar[bairdSunData$solar < (pi/2) | bairdSunData$solar > ((3*pi)/2)]))/(length(bairdSunData$solar))
  #Lowland
  lowlandNight<-(length(lowlandSunData$solar[lowlandSunData$solar < (pi/2) | lowlandSunData$solar > ((3*pi)/2)]))/(length(lowlandSunData$solar))
  #Malayan
  malayanNight<-(length(malayanSunData$solar[malayanSunData$solar < (pi/2) | malayanSunData$solar > ((3*pi)/2)]))/(length(malayanSunData$solar))
  #Mountain
  mountainNight<-(length(mountainSunData$solar[mountainSunData$solar < (pi/2) | mountainSunData$solar > ((3*pi)/2)]))/(length(mountainSunData$solar))
  
  
  ##
  mtext(paste(animal4[1]," n=",length(animal4)),3,cex=textSize, adj=0,font=2)
  
}
