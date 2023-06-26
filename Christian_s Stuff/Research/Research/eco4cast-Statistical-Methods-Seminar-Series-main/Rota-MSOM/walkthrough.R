#Rota multi-species occupancy walkthrough, Christian anderson 5/24/2023

#Read in detection/non-detection data
bob <- read.csv('data/bobcat_3wk.csv', header = F)
coy <- read.csv('data/Coyote_3wk.csv', header = F)
fox <- read.csv('data/RedFox_3wk.csv', header = F)

head(fox, 10)


#site level covariate data (covariates that only vary by site but stay constant for duration of survey)
occ_covs <- read.csv('data/psi_cov3wk.csv')
    ##Dist_5km = proportion disturbance within 5km
    ##HDens_5km = housing density within 5km
    ##latitude / longitude = latitude and longitude (div by 100)
    ##People_site = total people recorded / 1000
    ## trail = binary indicator of wheter camera was on trail (1) or not (0)

head(occ_covs)

#read in detection level covariate
det_covs <- read.csv('data/detection data.csv')
  #on column for every repliacte survey; average temperature over one week replicate period
    #example of covariate that changes from detection to detection
head(det_covs) 
