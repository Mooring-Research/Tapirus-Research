###Circadian plots using suncalc###

#Find your time zone in the following list
OlsonNames()

#or use lat/lon to find it
require(sf)
require(lutz)

lltz <- tz_lookup_coords(lat = Lowland_hour$lat, lon = Lowland_hour$long, method = "accurate")

btz <- tz_lookup_coords(lat = bairdssub$Latitud, lon = bairdssub$Longitd, method = "accurate")

lltz2 <- as.data.frame(lltz)
lltz2

#Get sunrise and sunset times and put them in a table titled 'sundata'
sundata <- getSunlightTimes(date = as.Date(Lowland_hour$date), lat = -13.32805, lon = -61.95775, tz = "America/Porto_Velho")
sundata <- getSunlightTimes(date = as.Date(bairdssub$Date), lat = 9.7753, lon = -84.6010, tz = "America/Costa_Rica")

#Convert sunrise and sunset to hms format
sundata$sunrise <- as_hms(sundata$sunrise)
sundata$sunset <- as_hms(sundata$sunset)

sunRise<- sundata$sunrise
sunSet <- sundata$sunset

tzone <- "America/Porto_Velho"
tzone <- "America/Costa_Rica"

#get rid of any NA values
sundata <- na.omit(sundata)


sunRise <- time_length(interval(
  start = ymd_hms(paste(as.Date(sundata$sunrise), "00:00:00"), 
                  tz = tzone),
  end = force_tz(time = sundata$sunrise, tzone = tzone)),
                  unit = "day")

sunSet <- time_length(interval(start = ymd_hms(paste(as.Date(sundata$sunset), "00:00:00"), tz = tzone),
                               end = force_tz(time = sundata$sunset, tzone = tzone)),
                      unit = "day")

sunRise <- sunRise * 2 * pi
sunSet <- sunSet * 2 * pi

clockTime <- dat[["time"]]
solar <- rep(NA, times = length(clockTime))

for (i in 1:length(clockTime)) {
  if (clockTime[i] <= sunRise[i]) {
    solar[i] <- ((1/2)*pi) * (clockTime[i]/sunRise[i]) #Predawn observations
  } else if (clockTime[i] <= sunSet[i]) {
    solar[i] <- (((clockTime[i] - sunRise[i])/(sunSet[i] - sunRise[i]))*pi) + ((1/2)*pi) #Daylight observations
  } else {
    solar[i] <- (((clockTime[i] - sunSet[i])/((2*pi) - sunSet[i]))*(1/2)*pi) + ((3/2)*pi) #Postdusk observations
  }
}

sundata <- data.frame("date" = as.Date(Lowland_hour$date),
                      "lat" = Lowland_hour$lat,
                      "lon" = Lowland_hour$long,
                      "time" = hms2rad(Lowland_hour$hour))

sundata <- data.frame("date" = as.Date(bairdssub$Date),
                      "lat" = bairdssub$Latitud,
                      "lon" = bairdssub$Longitd,
                      "time" = hms2rad(bairdssub$Time))

ind.data["Solar"] <- solarTime(sunData)

'densityPlot(
            A = subset(solarTime(sundata)
            xcenter = c("noon"), 
            xscale = NA,
            xlab = "Solar Position",
            xaxt = "n",
            main = "Lowland Tapir Circadian Plot", 
            rug = TRUE,
            extend = NULL,
            axis(side = 1, 
                 at = c(0, pi/2, pi, 3*pi/2, 2*pi),
                 labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight"))))

overlapPlot(hms2rad(sundata$sunrise), hms2rad(sundata$sunset), 
            xcenter = c("noon"), 
            xscale = NA,
            xlab = "Solar Position",
            xaxt = "n",
            main = "Lowland Tapir Circadian Plot", 
            rug = TRUE,
            extend = NULL,
            axis(side = 1, 
                 at = c(0, pi/2, pi, 3*pi/2, 2*pi),
                 labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")))
densityPlot(
  A = subset(hms2rad(solarTime(sundata))),
  xscale = NA,
  xlab = "Solar Position",
  xaxt = "n",
  main = "Lowland Circadian Plot"
)
rug(
  x = subset(solarTime(sundata)),
  side = 1
)
axis(
  side = 1,
  at = c(0, pi/2, pi, 3*pi/2, 2*pi),
  labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")

              )'