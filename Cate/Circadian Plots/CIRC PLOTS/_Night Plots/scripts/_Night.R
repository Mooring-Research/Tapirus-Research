###Calculating %Night for each species###

#Baird's
bairdNight<-(length(bairdSunData$solar[bairdSunData$solar < (pi/2) | bairdSunData$solar > ((3*pi)/2)]))/(length(bairdSunData$solar))

#Lowland
lowlandNight<-(length(lowlandSunData$solar[lowlandSunData$solar < (pi/2) | lowlandSunData$solar > ((3*pi)/2)]))/(length(lowlandSunData$solar))

#Malayan
malayanNight<-(length(malayanSunData$solar[malayanSunData$solar < (pi/2) | malayanSunData$solar > ((3*pi)/2)]))/(length(malayanSunData$solar))

#Mountain
mountainNight<-(length(mountainSunData$solar[mountainSunData$solar < (pi/2) | mountainSunData$solar > ((3*pi)/2)]))/(length(mountainSunData$solar))
