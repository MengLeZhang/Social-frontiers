##  Script that only builds the frontiers for checking results and saving it to save.to:
##  Section one: Loading in pkgs and specifying data paths (change to suit) ----
##  NOTE: We do not specify a location for saving to (save.to) in this script
##  Load pkgs

pkgs <- c('sf', 'CARBayes', 'tidyverse', 'tmap',
          'spdep',
          'INLA')
lapply(pkgs, library, character.only = T)

##  Load other soruce files -- we really ought to credit nema dean here
source("./Data/binomial.localisedINLA.R") ## This needs to be loaded

##  Section 1: We are expecting a merged.sf file to be defined elsewhere
merged.sf$prop.foreign <- merged.sf[[y]] / merged.sf[[n.trials]]
merged.sf$id_sf <- 1:nrow(merged.sf) #creating a consistent id variable
(merged.sf[[y]] > merged.sf[[n.trials]]) %>% table #check if y is bigger than trials
merged.sf$log.crime <- log(merged.sf[[crime.rate]] + 0.01) ## so greens are low
merged.sf$log.prop <- log(merged.sf[["prop.foreign"]] + 0.01) # red = high
merged.sf$log.pop <- log(merged.sf[[n.trials]] + 0.01)


##  Section 2: The routine for calculating frontiers ----

##  Creating the contiguity matrix
W.nb <- poly2nb(merged.sf %>% as('Spatial'),
                row.names = merged.sf$id_sf) #Sadly we have to convert the sf object to spatial dataframe using as() here
W <- nb2mat(W.nb, style = "B")
n <- nrow(W)

formula = merged.sf[[y]] ~ 1
W = W
Ntrials = merged.sf[[n.trials]]
fix.rho = TRUE
rho = 0.99

mod.inla <- binomial.localisedINLA(
  formula = merged.sf[[y]] ~ 1,
  W = W,
  Ntrials = merged.sf[[n.trials]],
  fix.rho = TRUE,
  rho = 0.99
)

##  This bit isn't essentialt it's just error checking
W %>% table ## Number of 1s (this number is double the neighbours) in original data
mod.inla$W.estimated %>% table ## Number estimated 1s (i.e. stat sig frontiers)

W.estimated <- mod.inla$W.estimated
W.estimated[W == 0] <-
  NA ## Put NAs where there was orginally not a border
W.estimated %>% table(useNA = 'always')

## Extract the estimated random effects which tells us how much the BSU departs
##  from the average prop of foreigners
phi <- mod.inla$phi[, 1] %>% as.numeric
merged.sf$mod_phi <- phi

##  Step four: Exracting the frontiers and all boundaries for tests ----
##  Tried this :https://stackoverflow.com/questions/47760033/r-gis-identify-inner-borders-between-polygons-with-sf
##   but doesn't work as smoothly or really at all
##  But yet merged.sf %>% st_intersection can accomplish a similar end results (after we appened the frontier info)
##  Here we are doing the frontier info first and pairwise finding borders

upper.w.est <- W.estimated
upper.w.est[upper.tri(upper.w.est, diag = T)] <-
  NA ## gets rid of the upper part of the sym. matrix


w.index0 <-
  which(upper.w.est == 0, arr.ind = T) %>% data.frame(frontier = T) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

w.index1 <-
  which(upper.w.est == 1, arr.ind = T) %>% data.frame(frontier = F) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

w.index <- w.index0 %>% rbind(w.index1)

borders.sf <- list(NULL)
for (i in 1:nrow(w.index)) {
  #i <- 1 # for testing
  bsu1 <- w.index$col[i]
  bsu2 <- w.index$row[i]
  temp <- merged.sf[bsu1,] %>% st_intersection(merged.sf[bsu2,])
  temp$frontier <- w.index$frontier[i]
  
  borders.sf[[i]] <- temp
}
#warnings() ##  ignore warnings about if about attributes etc

borders.sf <- do.call(rbind, borders.sf)
borders.sf$geometry %>% summary

##  Now to calculate differences in phi
borders.sf$abs_phi <- abs(borders.sf$mod_phi - borders.sf$mod_phi.1)

borders.sf$abs_phi_scaled <-
  borders.sf$abs_phi %>% scale ## scaled and centred (mean = 0, sd = 1)

borders.sf$abs_phi_rank <-
  borders.sf$abs_phi %>% order ## scaled and centred

borders.sf$crime.diff <-
  (borders.sf[[crime.rate]] - borders.sf[[paste(crime.rate, 1, sep = '.')]]) %>% abs

borders.sf$sig.frontier <-
  ifelse(borders.sf$frontier == T &
           borders.sf$abs_phi_scaled >= threshold,
         T,
         F)

##  Step five: Graphing the social frontiers ----
##  We want to keep borders.sf as a pairwise file
##  Create borders.gfx which extracts/ changes the file to just lines for graphing only!
borders.gfx <-
  st_collection_extract(borders.sf, type = 'LINE') # lots more objects now but still has the frontier feature in the right place

##  Step six: Comparisons of crime or other stats over frontiers and boundaries----
##  We already have a list of pairwise neighbours using w.est
##  it is the w.index df which also records whether something is a social frontier or not
aggregate(crime.diff ~ frontier, borders.sf, summary) #frontiers have larger crimes per pop by alot


##  Permutation test
##  Basically we are comparing mean diff in crime rates between frontiers and
##  non-frontiers if the two groups had no diff (i.e. they were assigned at random)
##  Then we compare our real diff to get the correct p values
iter <- 10000
null.dist <- rep(NA, iter)


frontier.n <- sum(borders.sf$sig.frontier)
for (i in 1:iter) {
  draws <-
    sample.int(borders.sf %>% nrow, size = frontier.n, replace = F)
  null.dist[i] <-
    mean(borders.sf$crime.diff[draws]) - mean(borders.sf$crime.diff[-draws])
}

null.dist %>% summary #right so should be zero
##  p values for getting the actual data
real.diff <-
  mean(borders.sf$crime.diff[borders.sf$sig.frontier == T]) - mean(borders.sf$crime.diff[borders.sf$sig.frontier == F])
all.diff <- c(real.diff, null.dist)
rank <- rank(all.diff)[1] #selects rank
pval <- punif((iter - rank + 2) / (iter + 1))

##  Two side p val?
##  Saving the results into a list with important stuff-----
##  data is the merged original bsu data
##  frontier.gfx is the line file of borders
##  frontiers.sf is the intersection file that has pairwair overlaps between neighbours
##  pval is the p valure of the permutation test for significant frontiers

saved.frontiers <- list(
  data = merged.sf,
  frontier.gfx = borders.gfx,
  frontier.sf = borders.sf,
  pval = pval
)

saveRDS(saved.frontiers, save.frontier.to)
print(
  'Results saved to' %>% paste(save.frontier.to) %>% paste('total size (bytes) =') %>% paste (saved.frontiers %>% object.size)
)


##  End script -----
