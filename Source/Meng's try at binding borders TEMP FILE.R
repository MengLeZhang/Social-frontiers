##  Meng's temp soultion for the Rbind issue
##  This starts from the frontiers creation source; just after we created
##  a index of borders and whether they are frontiers or not (windex)

library(foreach)
library(doParallel)

## Assume we have a objects called data, w.index and mod.inla
c('data', 'w.index', 'mod.inla') %in% ls() ## should all be true
## Do it parallel?



##  Step 1: Create a empty sf file with what we want
data.for.borders <-
  data %>%
  mutate(phi = mod.inla$phi[['Median']]) %>%
  select(LSOA, percentunemployed, crimesperperson, phi)

##  Take 1 intersection to get names off
fornames <- 
  data.for.borders[w.index$col[1],] %>% 
  st_intersection(data.for.borders[w.index$col[1],]) # now we are intersecting polys to get borders

#Create df of correct size with those names
borders.sf <- data.frame(1:ncol(fornames) %>%
                           rep(nrow(w.index)) %>% matrix(ncol = ncol(fornames)))
names(borders.sf) <- names(fornames)

##  Make back into sf
borders.sf <- st_as_sf(borders.sf, 
                       geometry = st_sfc(lapply(1:nrow(w.index), function(x) st_geometrycollection())))


##  Step 2a: Do a for loop for the intersection
##  SUPER VITAL STEP: We must register the clusters:
detectCores()
registerDoParallel(detectCores() - 1) #use all but 1
## on vm its 5

x <- proc.time()

saved.sf <- 
  foreach (i=1:nrow(w.index) #,.combine = rbind
           ) %dopar% {
  #i <- 1 # for testing
  
  library(sf)
  library(tidyverse)
  
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  
  ##  Some reason doesn't work -- in parallel but does in single --
  # borders.sf[i,] <- data.for.borders[zone1,] %>% 
  #   st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  #borders.sf$frontier[i] <- w.index$frontier[i]
  
  data.for.borders[zone1,] %>% 
    st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  }

proc.time() - x
stopImplicitCluster()
#

## http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/ ## so best would be to assign?
##  Results:
##  SMI3 (6 cores/ 6 threads: 2.00ghz) 
##  No parallel: 5399 seconds - 89 minutes -- 
##  5 threads: 3406 sec -- 56 min --- faster but not huge (w/ .combine = rbind)
##  5 thread (same as above but no rbind): 88 sec!!! So huge -- now we have a list

borders.sf %>% head

?foreach
