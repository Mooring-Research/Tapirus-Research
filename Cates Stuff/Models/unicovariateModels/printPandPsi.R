#this script takes all the models in the environment and grabs their p and psi values
objs<- ls()
models<- objs[grepl("^MT_m\\.|^MA_m\\.|^CR_m\\.|^AM_m\\.", objs)]


#function for psi value
pf <- function(x) {
  occu <- 0
  if(length(x@estimates@estimates$state@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$state@estimates)) {
      occu <- (occu + plogis(x@estimates@estimates$state@estimates[i])) 
    }
    occu <- occu/(length(x@estimates@estimates$state@estimates)-1)
  } else {
    occu <- plogis(x@estimates@estimates$state@estimates[2])
  }
  print(paste("𝜓= ", signif(occu, digits = 4)))
}

# Function to give detection probabilities (p) for models 
pd <- function(x) {
  detp <- 0
  if(length(x@estimates@estimates$det@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$det@estimates)) {
      detp <- (detp + plogis(x@estimates@estimates$det@estimates[i])) 
    }
    detp <- detp/(length(x@estimates@estimates$det@estimates)-1)
  } else {
    detp <- plogis(x@estimates@estimates$det@estimates[2])
  }
  print(paste("p= ", signif(detp, digits=4)))
}

#function of both funcs
pfpd<- function(x){
  #print(x@formula)
  pf(x)
  pd(x)
}

#sink("C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModselAll.txt", append = TRUE)
for (model in models){
  print(model)
  pfpd(get(model))
  
}
#sink()


