##  Meng's temp soultion for the Rbind issue
##  This starts from the frontiers creation source; just after we created
##  a index of borders and whether they are frontiers or not (windex)

library(foreach)
library(doParallel)
## Assume we have a objects called data, w.index and mod.inla
c('data', 'w.index', 'mod.inla') %in% ls() ## should all be true

fornames <- 
  data.for.borders[w.index$col[1],] %>% 
  st_intersection(data.for.borders[w.index$col[1],]) # now we are intersecting polys to get borders


#Create df of correct size with those names
borders.sf <- data.frame(1:ncol(fornames) %>%
                           rep(nrow(w.index)) %>% matrix(ncol = ncol(fornames)))
names(borders.sf) <- names(fornames)
borders.sf %>% head

borders.sf <- st_as_sf(borders.sf, 
                       geometry = st_sfc(lapply(1:nrow(w.index), function(x) st_geometrycollection())))

##  SUPER VITAL STEP: We must register the clusters:
detectCores()
registerDoParallel(detectCores() - 1) #use all but 1

x <- proc.time()
foreach (i=1:nrow(w.index), .combine = rbind) %dopar% {
  #i <- 1 # for testing
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  
  borders.sf[i,] <- data.for.borders[zone1,] %>% 
    st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  #borders.sf$frontier[i] <- w.index$frontier[i]
  
  if(i %% 10 == 0){
    print(i)
  }
  
}
proc.time() - x

stopImplicitCluster()
