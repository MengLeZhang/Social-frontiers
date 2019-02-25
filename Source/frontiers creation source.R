##  Script that only builds the frontiers for checking results and saving it to save.to:
##  Section one: Loading in pkgs and specifying data paths (change to suit) ----
##  NOTE: We do not specify a location for saving to (save.to) in this script
##  Load pkgs
pkgs <- c('sf', 'tidyverse', 'tmap',
          'spdep',
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


##  Section 2: The routine for calculating frontiers ----

##  Creating the contiguity matrix
W.nb <- poly2nb(data %>% as('Spatial'),
                #queen = F,#more than just a single point touching to be neighbours
                row.names = data$id_sf) #Sadly we have to convert the sf object to spatial dataframe using as() here

#Check some neighbours look sensible
#Summary will show table for number of neighbours
#Check for any zones with zero neighbours - shouldn't have any of those
#(unless working with some islands but then perhaps those should be excluded)
summary(W.nb)

zonetolookat = 5381
plot(st_geometry(data[W.nb[[zonetolookat]],]), col = 'blue')
plot(st_geometry(data[zonetolookat,]), add = T, col = 'red')

#Double-check/confirm whether we have any zones with no neighbours? No.
table(unlist(W.nb) == 0)

#So fine to go ahead with getting matrix
W <- nb2mat(W.nb, style = "B") # B = binary


# actual running inla routine
# Time it... ~11 mins for London LSOAs
source("./Source/binomial.localisedINLA.R") ## This needs to be loaded
x <- proc.time()

mod.inla <- binomial.localisedINLA(
  formula = data[[y]] ~ 1,# y is variable name for number of foreigners
  W = W,
  Ntrials = data[[n.trials]],# how many lived in zone
  fix.rho = TRUE,
  rho = 0.99
)

proc.time() - x

##  Basically up to this point it ought to be self contained

##  So the inla output is just a list not a proper class object..
mod.inla$W.estimated_cleaned <- mod.inla$W.estimated #extract estimated matrix
# So basically we are putting NAs where there was no neighbour in the original
# W matrix. Remainder are geographical neighbours with 0s and 1s denoting
# sig correlation or not
mod.inla$W.estimated_cleaned[W == 0] <- -1 ## Put NAs where there was orginally not a border
mod.inla$W.estimated_cleaned[lower.tri(mod.inla$W.estimated_cleaned, diag = T)] <- -1 ## gets rid of the upper part of the sym. matrix

class(mod.inla) <- 'frontier_model' #changes it's class allowing for custom routines


##  Got rid of phi here -- no need to append

##  Step four: Exracting the frontiers and all boundaries for tests ----
##  Tried this :https://stackoverflow.com/questions/47760033/r-gis-identify-inner-borders-between-polygons-with-sf
##   but doesn't work as smoothly or really at all
##  But yet data %>% st_intersection can accomplish a similar end results (after we appened the frontier info)
##  Here we are doing the frontier info first and pairwise finding borders

upper.w.est <- W.estimated
upper.w.est[lower.tri(upper.w.est, diag = T)] <- NA ## gets rid of the upper part of the sym. matrix
#upper.w.est %>% head

# Now we want to find the pairs where there are 0 (frontiers) and 1 (non-frontier) in the estimated
# inla matrix. = 
w.index0 <-
  which(upper.w.est == 0, arr.ind = T) %>% # finds non-NA values (which row and col) and arr.ind returns it as a matrix 
  data.frame(frontier = T) # takes the table and turns it into data.frame and adds a row called frontier

w.index1 <-
  which(upper.w.est == 1, arr.ind = T) %>% 
  data.frame(frontier = F) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

#One row for every unique neighbour pair, saying where frontier or not
w.index <- w.index0 %>% rbind(w.index1) # I want all the indicies for social frontiers and non-frontiers in
## one data.frame
w.index %>% head 


#Attempt 2, create dataframe directly
#rbind playing up for large list
data.for.borders <- data %>% 
  select(LSOA,percentunemployed,crimesperperson,mod_phi)


#Avoiding the 2nd circle of growing df hell
#https://stackoverflow.com/questions/14693956/how-can-i-prevent-rbind-from-geting-really-slow-as-dataframe-grows-larger
#rbind becomes hugely slow as df grows.
#Pre-create it and update directly

#Cheat: get column names from single intersect operation
fornames <- data.for.borders[w.index$col[1],] %>% st_intersection(data.for.borders[w.index$col[1],]) # now we are intersecting polys to get borders

#Create df of correct size with those names
borders.sf <- data.frame(id = 1:nrow(w.index))

#Fills with single value but now we have correct column types and rows
for(i in names(fornames)){
  borders.sf[,i] <- fornames[,i]
}

borders.sf <- borders.sf %>% select(-id)
borders.sf <- st_as_sf(borders.sf)

#Column for frontier flag
#borders.sf$frontier <- F


x <- proc.time()

for (i in 1:nrow(w.index)) {
  #i <- 1 # for testing
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  
  borders.sf[i,] <- data.for.borders[zone1,] %>% st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  #borders.sf$frontier[i] <- w.index$frontier[i]
  
  if(i %% 10 == 0){
    print(i)
  }
  
}

#40 mins. Whut??
proc.time() - x

#Add frontier flag
borders.sf$frontier <- w.index$frontier



#Growing version
x <- proc.time()

for (i in 1:nrow(w.index)) {
  #i <- 1 # for testing
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  
  #First row becomes sf dataframe we'll rbind to.
  if(i==1){
    borders.sf <- data.for.borders[zone1,] %>% st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
    borders.sf$frontier <- w.index$frontier[i]
  } else {
    temp <- data.for.borders[zone1,] %>% st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
    temp$frontier <- w.index$frontier[i]
    borders.sf <- rbind(borders.sf,temp)
  }
  
  if(i %% 10 == 0){
    print(i)
  }
  
  
}

proc.time() - x






# now a for loop to basically create intersections for pairs of polygons
# who are neighbours and say if they are frontier or not
borders.sf <- list()#Start empty...

#Reduce data field numbers to reduce border file size.
data.for.borders <- data %>% 
  select(LSOA,percentunemployed,crimesperperson,mod_phi)

for (i in 1:nrow(w.index)) {
  #i <- 1 # for testing
  zone1 <- w.index$col[i]
  zone2 <- w.index$row[i]
  temp <- data.for.borders[zone1,] %>% st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
  
  #Don't use empty intersections. Slight hack...
  # if(nrow(temp)!=0){
  #   temp$frontier <- w.index$frontier[i]
  #   borders.sf[[length(borders.sf)+1]] <- temp
  # }
  
  temp$frontier <- w.index$frontier[i]
  borders.sf[[length(borders.sf)+1]] <- temp
  
}
#warnings() ##  ignore warnings about if about attributes etc
# x <- proc.time()
# borders.sf.df <- do.call(rbind, borders.sf)
# proc.time() - x

borders.sf$geometry %>% summary # not that it's not all proper lines
borders.sf %>% head
# end of making border.sf object that contains borders only for graphs etc

#Save that and the INLA run
saveRDS(borders.sf,'Data/local/london_borders_sf2.rds')
saveRDS(mod.inla,'Data/local/london_inlarun1.rds')

borders.sf <- readRDS('Data/local/london_borders_sf2.rds')
mod.inla <- readRDS('Data/local/london_inlarun1.rds')

#One border per neighbour pair?
#I think there should be 16978 neighbour pairs...
summary(W.nb)

#No? Hmm, something about row and column names...
#isSymmetric(W)
library(matrixcalc)
is.symmetric.matrix(W)#Yes
is.symmetric.matrix(mod.inla$W.estimated)#Yes. Doesn't work if NAs present.




#We seem to have many polygons in here...?
# i = 5
# plot(borders.sf[i,'UK'])
# st_geometry_type(borders.sf[i,])
# 
# #check on those polygons from the original
# plot(data[borders.sf$id_sf[i],'UK'])
# plot(data[borders.sf$id_sf.1[i],'UK'])
# plot(data[c(borders.sf$id_sf[i],borders.sf$id_sf.1[i]),'UK'])
# 
# #But the intersect is producing something messy?
# intz <- data[borders.sf$id_sf[i],] %>% st_intersection(data[borders.sf$id_sf.1[i],])
# plot(intz[,'UK'])

##  Now to calculate differences in phi
borders.sf$abs_phi <- 
  abs(borders.sf$mod_phi - borders.sf$mod_phi.1)

borders.sf$abs_phi_scaled <-
  borders.sf$abs_phi %>% scale ## scaled and centred (mean = 0, sd = 1)

borders.sf$abs_phi_rank <-
  borders.sf$abs_phi %>% order ## scaled and centred

#borders.sf$crime.diff <-
# (borders.sf[[crime.rate]] - borders.sf[[paste(crime.rate, 1, sep = '.')]]) %>% abs

borders.sf$sig.frontier <-
  ifelse(borders.sf$frontier == T &
           borders.sf$abs_phi_scaled >= threshold,
         T,
         F)


#save to check in QGIS
#Doesn't like multilinestring (there's only one)
st_write(borders.sf %>%
           filter(!st_geometry_type(.) %in% c("POINT")),
         'Data/local/borders_sf_london_all.shp', delete_layer = T)




##  Step five: Graphing the social frontiers ----
##  We want to keep borders.sf as a pairwise file
##  Create borders.gfx which extracts/ changes the file to just lines for graphing only!
borders.gfx <-
  st_collection_extract(borders.sf, type = 'LINE') # lots more objects now but still has the frontier feature in the right place

##  Step six: Comparisons of crime or other stats over frontiers and boundaries----
##  We already have a list of pairwise neighbours using w.est
##  it is the w.index df which also records whether something is a social frontier or not
#aggregate(crime.diff ~ frontier, borders.sf, summary) #frontiers have larger crimes per pop by alot


##  Permutation test (not necessary for frontiers)---------------






#Use Nema's function
source('Source/permutation_test.R')

#debugonce(permutation.test)
#Ah yes, London is very large! Let's try 100 to start with.
ec_act.test <- permutation.test(variates=data$crimesperperson,W0=W,W1=W.estimated, iter=100)

ec_act.test$pvalue

plot(density(ec_act.test$all.diff),main="",lwd=1.5,lty="dashed")
abline(v=ec_act.test$all.diff[1],lwd=2)

#Why this one different value? That matches the actual value? Err.
ec_act.test$all.diff[ec_act.test$all.diff > 0.02]


#And for unemployment?
ec_act.test.ea <- permutation.test(variates=data$percentunemployed,W0=W,W1=W.estimated, iter=100)

#Err. exactly same value. 
ec_act.test.ea$pvalue

#Consistently negative differences...?
plot(density(ec_act.test.ea$all.diff),main="",lwd=1.5,lty="dashed")
abline(v=ec_act.test.ea$all.diff[1],lwd=2)







##  Basically we are comparing mean diff in crime rates between frontiers and
##  non-frontiers if the two groups had no diff (i.e. they were assigned at random)
##  Then we compare our real diff to get the correct p values
#  This is not how I would do it but it's more right than what I would have done
#  so revisit this

#Find absolute contiguous differences
borders.sf <- borders.sf %>%
  mutate(diff = abs(crimesperperson - crimesperperson.1))

borders.sf <- borders.sf %>%
  mutate(diff = abs(percentunemployed - percentunemployed.1))

iter <- 10000
null.dist <- rep(NA, iter)

frontier.n <- sum(borders.sf$sig.frontier)
for (i in 1:iter) {
  draws <-
    sample.int(borders.sf %>% nrow, size = frontier.n, replace = F)
  null.dist[i] <-
    mean(borders.sf$diff[draws]) - mean(borders.sf$diff[-draws])
}

null.dist %>% summary #right so should be zero
##  p values for getting the actual data
real.diff <-
  mean(borders.sf$diff[borders.sf$sig.frontier == T]) - mean(borders.sf$diff[borders.sf$sig.frontier == F])
all.diff <- c(real.diff, null.dist)
rank <- rank(all.diff)[1] #selects rank
pval <- punif((iter - rank + 2) / (iter + 1))

plot(density(null.dist),main="",lwd=1.5,lty="dashed")
abline(v=real.diff,lwd=2)



#On reflection, that seems like a strange test to me.
#It seems intuitive there will be a higher absolute contiguous difference for any variable
#between zones with a higher cliff edge of social difference
#Isn't that just saying that the two things are correlating?

#The interesting thing to find would be: is crime higher for both zones
#Compared to random pairs of zones?
#May have to scribble this.


# ##  Two side p val?
# ##  Saving the results into a list with important stuff-----
# ##  data is the merged original bsu data
# ##  frontier.gfx is the line file of borders
# ##  frontiers.sf is the intersection file that has pairwair overlaps between neighbours
# ##  pval is the p valure of the permutation test for significant frontiers

saved.frontiers <- list(
  data = data,
  frontier.gfx = borders.gfx,
  frontier.sf = borders.sf#,
  #  pval = pval
)

saveRDS(saved.frontiers, save.frontier.to)
print(
  'Results saved to' %>% paste(save.frontier.to) %>% paste('total size (bytes) =') %>% paste (saved.frontiers %>% object.size)
)


##  End script -----
