# Calculating solar time for Tapir temporal behavior analysis
Solar time standardizes the time of day to a value (in radians) that relates the location and therefore intensity of the sun. 
For example, the sun’s location at 07:00 in January will be different than 07:00 in July, and will vary even more when comparing different latitudes. 
To adjust for the variability in the sun's path and the camera trap's location on the Earth, this function utilizes the study site's time zone, longitude and latitude, 
and detection time for each sighting. 

Function below calculates the 'solar time' for a given location, date, and time. After finding the intervals from 00:00 to sunrise and sunset to 00:00 using the location, time, and timezone, the function creates a new field and then, given what interval the record's timestamp (in radians) lies in (00:00 - sunrise, sunrise - 
sunset, or sunset -00:00), calculates the timestamp's ratio of that standardized interval. The product of that ratio and the length of the interval in which it lies places the timestamp on that interval. Next, the added pi term adjusts for where that interval lies in relation to the other intervals. The result is a vector of 
timestamps which range from 0 - 2pi. 
```R 
solarhour <- function(dat, tzone) {
	#inputs:
		#'dat' is a data.frame with the following columns: 
		#"date" (the POSIXct date), 
		#"lat" (the Latitude),
		#"lon" (the Longitude), 
		#"hour" (the hour of day in RADIANS)
	#'tzone' one of Olsontimes timezones for the location of interest. 
  #ouptuts: 
  #'solar' is a vector of "solar hours" (in RADIANS) where (1/2)pi is sunrise and (3/2)pi is sunset
  
  #Get sunrise and sunset as date-hour objects
  sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz = tzone)
  sunRise <- sunData$sunrise
  sunSet <- sunData$sunset
  
  #Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
  sunRise <- time_length(interval(start = 
                                    ymd_hms(
                                      paste(
                                        as.Date(sunRise), "00:00:00"), tz = tzone),
                                  end = 
                                    force_tzs(sunRise, tzone)),
                         unit = "day")
  sunSet <- time_length(interval(start = 
                                   ymd_hms(
                                     paste(
                                       as.Date(sunSet), "00:00:00"), tz = tzone),
                                 end = 
                                   force_tzs( sunSet, tzone)),
                        unit = "day")
  
  #Convert sunrise/sunset to radians
  sunRise <- sunRise * 2 * pi
  sunSet <- sunSet * 2 * pi  
  clockhour <- dat[["hour"]]
  solar <- rep(NA, hours = length(clockhour))
  
  for (i in 1:length(clockhour)) {
    if (clockhour[i] <= sunRise[i]) {
      solar[i] <- ((1/2)*pi) * (clockhour[i]/sunRise[i]) #Predawn observations
    } else if (clockhour[i] <= sunSet[i]) {
      solar[i] <- (((clockhour[i] - sunRise[i])/(sunSet[i] - sunRise[i]))*pi) + ((1/2)*pi) #Daylight observations 
					#^^ the differences standardize the times to zero. the ratio of those differences * pi put it on the interval from surise to sunset (pi hours)
					#then the 1/2 pi is added to put it in correct relation to the previous interval (00:00 - sunset).
    } else {
      solar[i] <- (((clockhour[i] - sunSet[i])/((2*pi) - sunSet[i]))*(1/2)*pi) + ((3/2)*pi) #Postdusk observations
    }
  }
  
  return(solar)
}

```