#Testing maps - does generalisation level affect st_intersection's getting borders from intersects?
#They don't seem to be working well for generalised. Is that the problem?
library(sf)
library(tidyverse)
library(spdep)

#generalised
lsoa.gen <- st_read('C:/Data/MapPolygons/England/2011/England_lsoa_2011_gen/england_lsoa_2011_gen.shp')

#full
lsoa.full <- st_read('C:/Data/MapPolygons/England/2011/England_lsoa_2011_full/england_lsoa_2011.shp')

#Check they're in the same order... newp, they're not currently
#table(lsoa.gen$code == lsoa.full$code)

#Reorder both, hopefully then will be
lsoa.gen <- lsoa.gen %>% arrange(code)
lsoa.full <- lsoa.full %>% arrange(code)

#Tick!
table(lsoa.gen$code == lsoa.full$code)


#Get neighbours. Can be the same for both now.
W.nb <- poly2nb(lsoa.gen %>% as('Spatial'), row.names = lsoa.full$code)

W.nb[[3]]
W.nb[[32584]]

#How can some of them *not* have neighbours??
W.nb[[1]]
plot(lsoa.gen[1,'code'])

#Well that definitely *does* have neighbours. 


#Run separately for the full one, see if a different list comes up.
W.nb.full <- poly2nb(lsoa.full %>% as('Spatial'), row.names = lsoa.full$code)

#save that, takes a while to run. Although it's the full one - we only actually need London.
saveRDS(W.nb.full, 'Data/neighbourlist_LSOA_full.rds')

#Check...
W.nb.full[[1]]
W.nb.full[[3]]
W.nb.full[[32584]]

#Ah, that is now looking sensible. I might save it as it takes a while to run.


#Now what about intersects?
intz.full <- lsoa.full[1,] %>% st_intersection(lsoa.full[2,])

#A line!
plot(st_geometry(intz.full))

#Whereas...
intz.gen <- lsoa.gen[1,] %>% st_intersection(lsoa.gen[2,])

#Not a line!
plot(st_geometry(intz.gen))


#Which is good news though, overall: it means we can use the full data and get what we need.
#So how many neighbours does gen miss?
#First-up: how many apparently have no neighbours?

#Total number of neighbours detected
length(unlist(W.nb))
length(unlist(W.nb.full))

#how many zones with zero neighbours detected?
table(unlist(W.nb) == 0)
table(unlist(W.nb.full) == 0 )



#Hang on. I can put these side by side if I just count. Err.
#Except length 1 can be zero
length(W.nb[[2]])


W <- nb2mat(W.nb, style = "B", zero.policy = T)
W <- nb2mat(W.nb, style = "B")


