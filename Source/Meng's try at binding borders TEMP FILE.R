##  Meng's temp soultion for the Rbind issue
##  This starts from the frontiers creation source; just after we created
##  a index of borders and whether they are frontiers or not (windex)

library(foreach)
library(doParallel)

## Assume we have a objects called data, w.index and mod.inla
c('data', 'w.index', 'mod.inla') %in% ls() ## should all be true
## Do it parallel?
##  See the frontier creation source


##  Step 1: Create a empty sf file with what we want
data.for.borders <-
  data %>%
  mutate(phi = mod.inla$phi[['Median']]) %>%
  select(LSOA, percentunemployed, crimesperperson, phi)


##  Step 2a: Do a for loop for the intersection
##  SUPER VITAL STEP: We must register the clusters:
detectCores()
registerDoParallel(detectCores() - 1) #use all but 1
## on vm its 5

x <- proc.time()
##  In parallel just extract intersections
saved.sf <- 
  foreach (i=1:nrow(w.index) #,.combine = rbind
           ) %dopar% {
  #i <- 1 # for testing
  
  library(sf)
  library(tidyverse)
  
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  
  temp <- data.for.borders[zone1,] %>% 
    st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  
  st_geometry(temp)
  }

proc.time() - x
stopImplicitCluster()
# saved.sf is a list


##  Then use c not rbind in do.call
system.time(
  test <- do.call(c, saved.sf)
)
test %>% head
test[1:100] %>% plot

##  Done!


##  Other test results -- using worse methods
## http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/ ## so best would be to assign?
##  Results:
##  SMI3 (6 cores/ 6 threads: 2.00ghz) 
##  No parallel: 5399 seconds - 89 minutes -- old code using rbind
##  5 threads: 3406 sec -- 56 min --- old code in paralle faster but not huge (w/ .combine = rbind)

##  Indexing :::
# x <- proc.time()
# for (i in 1:nrow(w.index)){
#   borders.sf[['geometry']][i] <- saved.sf[[i]]$geometry
# }
# proc.time() - x
##  Let's see how long this takes
##  List : 4484 -- so ages

