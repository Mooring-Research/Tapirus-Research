###Attempting to make a circadian plot with sunrise and sunset###

rm(list = ls())

#Required packages
require("overlap")
require("suncalc")
require("lubridate")

patternAnimal <- "Mountain tapir"
timeZone<- "America/Lima"
ind.data <- Mountain_hour[Mountain_hour$hour == "Yes",]

#Format date objects
dateFormat = "%m/%d/%Y"
ind.data$date <- base::as.Date(ind.data$date, format = dateFormat)

#Convert time to radians
max.time <- max(ind.data$hour)
if (max.time > 12 & max.time <= 24) {
  ind.data["TimeRad"] <- ind.data$hour*2*pi/24
} else if (max.time <= 1) {
  ind.data["TimeRad"] <- ind.data$hour*2*pi
} else print("Unknown time format.")

#Get solar data
solarTime <- function(dat, tzone = timeZone)

#Trim so that data sets have the same number of rows
mounttrim <- Mountain_hour[-c(86, 87, 88, 89, 90, 91, 92, 93, 94, 95),]

#Get data as Date
mountDate <- as.Date(mounttrim$date, "%Y-%m-%d")

#Define dat
dat <- data.frame("date" = mountDate, "lat" = Mountain_covariates$Latitud, "lon" = Mountain_covariates$Longitd, "time" = mounttrim$hour)

#Get sunrise and sunset as date-time objects
sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz="America/Lima")
sunRise <- sunData$sunrise
sunSet <- sunData$sunset

##HAVING TROUBLE HERE -- Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
sunRise <- time_length(interval(start = ymd_hms(paste(date(sunRise), "00:00:00"), tz = "America/Lima"),
                                end = force_tz(time = sunRise, tzone = "America/Lima")), unit = "day")
sunSet <- time_length(interval(start = ymd_hms(paste(date(sunSet), "00:00:00"), tz = "America/Lima"),
                               end = force_tz(time = sunSet, tzone = "America/Lima")),
                      unit = "day")

#Convert sunrise/sunset to radians
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

return(solar)
}

sunData <- data.frame(
  "date" = Mountain_hour$date,
  "lat" = Mountain_covariates$Latitud,
  "lon" = Mountain_covariates$Longitd,
  "time" = Mountain_hour$hour
)
