##  Test scripts for different routine
library(tidyverse)
library(sf)

##  Loading Global values  ----
threshold <- 1 #threshold is basically how many standard deviation above the average for
##  phi we are willing to tolerate as being significant.
##  Phi is a statistic related to proportion of foreigners created by the model

##  Load in data
merged.sf <- './Data/Cleaned Pardubice sf.RDS' %>% readRDS 
merged.sf <- merged.sf %>% subset(Pop11 > 10) # filter to valid BSU; leaves us with 61 zones

##  Test 1: The basic foreign only ----

##  Define variables
y <- 'For11.v2' # Number of foreign
n.trials <- 'Pop11' #total population
crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

##  Place to save results
save.frontier.to <- './Results/Pardubice frontiers all results A.RDS'
save.grid.to <- './Results/Pardubice frontiers all results B.RDS'

##  Run frontiers routine
source('frontiers creation source.R') #


##  Run the poisson grid square routine
source('Grid and poisson source.R') #Stat sig but hmmm



##  Test 2: Testing using cultural distance -- closer ----
y <- 'closer' # Number of foreign
n.trials <- 'Pop11' #total population
crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

### Running and saving results
save.frontier.to <- './Results/Pardubice frontiers closer results A.RDS'
save.grid.to <- './Results/Pardubice frontiers closer results B.RDS'

source('frontiers creation source.R') #okay Prop crim per pop is good here
source('Grid and poisson source.R') #okay Prop crim per pop is good here


##  Test 3: running w/ further----
##  !This is where we just need to name the variables for later on!
y <- 'further' # Number of foreign
n.trials <- 'Pop11' #total population
crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

save.frontier.to <- './Results/Pardubice frontiers further results A.RDS'
save.grid.to <- './Results/Pardubice frontiers further results B.RDS'

source('frontiers creation source.R') #okay Prop crim per pop is good here
source('Grid and poisson source.R') #okay Prop crim per pop is good here

##  Done
##  Test 4: Running w/ resident permanent ----
##  Define variables
names(merged.sf)
y <- 'permanent' # Number of foreign
n.trials <- 'Pop11' #total population
crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

##  Place to save results
save.frontier.to <- './Results/Pardubice frontiers permanent results A.RDS'
save.grid.to <- './Results/Pardubice frontiers permanent results B.RDS'

##  Run frontiers routine
source('frontiers creation source.R') #

##  Run the poisson grid square routine
source('Grid and poisson source.R') #Stat sig but hmmm

##  Test 5: Running w/ resident temporary ----
##  Define variables
names(merged.sf)
y <- 'temporary' # Number of foreign
n.trials <- 'Pop11' #total population
crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

##  Place to save results
save.frontier.to <- './Results/Pardubice frontiers temporary results A.RDS'
save.grid.to <- './Results/Pardubice frontiers temporary results B.RDS'

##  Run frontiers routine
source('frontiers creation source.R') #

##  Run the poisson grid square routine
source('Grid and poisson source.R') 

##  End ----