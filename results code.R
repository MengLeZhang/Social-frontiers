##  Test scripts for different routine
library(tidyverse)
library(sf)

##  Loading Global values  ----
threshold <- 1.96 #threshold is basically how many standard deviation above the average for
##  phi we are willing to tolerate as being significant.
##  Phi is a statistic related to proportion of foreigners created by the model

##  Load in data. This shapefile assembled in 'sf creation.R'
#merged.sf <- st_read('Data/londonlsoas_nonclipped_nongeneralised_w_nonUK_cob.shp')
merged.sf <- readRDS('Data/londondata_LSOA.rds')

##  Test 1: The basic foreign only ----

##  Define variables
y <- 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)
#crime.rate <- 'Prop_crimes_km2' # the name of the crime rate variable which per square km2

##  Place to save results
save.frontier.to <- './Results/London frontiers all results A.RDS'
#save.grid.to <- './Results/London frontiers all results B.RDS'

##  Run frontiers routine
source('Source/frontiers creation source.R') 