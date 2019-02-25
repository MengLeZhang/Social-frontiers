summary.frontier_model <- function(object, ...){
  ## Display the numbers of frontiers
  borders.tab <- tabulate(object$W.estimated_cleaned + 1) # so table will convert integer to
  #factor first so the quicker way is to use tabulate which will only display
  ##  coutners of positive intergers
  
  print('Total N. of borders')
  print(sum(borders.tab))
  print('N. of borders: non-frontier vs frontier')
  print(borders.tab) 
  
  
}

summary(mod.inla)
(mod.inla$W.estimated_cleaned + 1) %>% tabulate
mod.inla$W.estimated_cleaned %>% str # huge matrid




##  We justed need the outputed model matrix

##  Extract the estimated random effects which tells us how much the BSU departs
##  from the average prop of foreigners
phi <- mod.inla$phi[, 1] %>% as.numeric
data$mod_phi <- phi
