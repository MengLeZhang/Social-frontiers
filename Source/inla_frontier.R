##  Output:
##  list object of class: frontier_model
##


pkgs <- c('sf', 'tidyverse', 'tmap',
          'spdep',
          # 'foreach',
          # 'doParallel',
          'INLA')

lapply(pkgs, library, character.only = T)


##  Suggested break down
##  Input: formula, data, additiona arguments (i.e. n.trials)
##  Output: Sf file frontiers and borders, INLA model outputs
##  Suggestion: Brreak into 2 parts: Running the INLA model as a function
##  and extract frontiers
##  A) Outputs the original spatial continguity matrix and model related data?
##  This is basically to save 


### TEST version --- 
threshold <- 1.96 
data <- readRDS('Data/londondata_LSOA.rds')
y <- 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)
###


##  Section 1: We are expecting a data file to be defined elsewhere
# So this is entirely for Iva's analysis's sake
data$prop.foreign <- data[[y]] / data[[n.trials]] # prop.foreign is % foreign
data$id_sf <- 1:nrow(data) #creating a consistent id variable
(data[[y]] > data[[n.trials]]) %>% table # check if y is bigger than trials == prob encountered - shouldn't matter for london data
#data$log.crime <- log(data[[crime.rate]] + 0.01) ## so greens are low
data$log.prop <- log(data[["prop.foreign"]] + 0.01) # red = high
data$log.pop <- log(data[[n.trials]] + 0.01)

queen = F
inla_frontier <- function(y, data, n.trials, queen = F, ...){
  ## Check format
  data.class <- class(data)
  if(!('SpatialPolygonsDataFrame' %in% data.class) & !('sf' %in% data.class)) stop ('Data not sf or SpatialPolygons')
  
  if('sf' %in% class(data)){data2 <- data %>% as('Spatial')} else {data2 <- data}
  
  ##  We do not touch data
  ##  We work with data2 and other versions copied (for just keep track of things)
  
  ##  Creating the contiguity matrix
  W.nb <- poly2nb(data2,
                  queen = queen,#more than just a single point touching to be neighbours
                  row.names = 1:nrow(data2)) #Sadly we have to convert the sf object to spatial dataframe using as() here
  count.no.neighours <- sum(unlist(W.nb) == 0)
  if(count.no.neighours > 0){warning( count.no.neighours %>% paste('zone(s) have no neighbours!'))}

  W <- nb2mat(W.nb, style = "B") # B = binary
  
  
  ##  INLA routine
  source("Source/binomial.localisedINLA.R") ## This needs to be loaded
  
  x <- proc.time()
  
  mod.inla <- binomial.localisedINLA(
    formula = data2[[y]] ~ 1,# y is variable name for number of foreigners
    W = W,
    Ntrials = data2[[n.trials]],# how many lived in zone
    fix.rho = TRUE,
    rho = 0.99
  )
  
  print(proc.time() - x)
  
  
  
  ##  So the inla output is just a list not a proper class object..
  mod.inla$W.frontiers <- mod.inla$W.estimated #extract estimated matrix
  # So basically we are putting NAs where there was no neighbour in the original
  # W matrix. Remainder are geographical neighbours with 0s and 1s denoting
  # sig correlation or not
  mod.inla$W.frontiers[W == 0] <- -1 ## Put NAs where there was orginally not a border
  mod.inla$W.frontiers[lower.tri(mod.inla$W.frontiers, diag = T)] <- -1 ## gets rid of the upper part of the sym. matrix
  
  class(mod.inla) <- 'frontier_model' #changes it's class allowing for custom routines
  
  return(mod.inla)
}


#saveRDS(mod.inla, 'temp junk/Output of inla_frontier.RDS')
summary(mod.inla)


##  To include in summary
tabulate(W) / 2
#Double-check/confirm whether we have any zones with no neighbours? No.
sum(unlist(W.nb) == 0)


