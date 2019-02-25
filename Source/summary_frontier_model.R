summary.frontier_model <- function(object, ...){
  ## Display the numbers of frontiers
  
}

?summary

##  We justed need the outputed model matrix

##  Extract the estimated random effects which tells us how much the BSU departs
##  from the average prop of foreigners
phi <- mod.inla$phi[, 1] %>% as.numeric
data$mod_phi <- phi
