#The goal:
#total number of crimes per LSOA in London
library(tidyverse)
library(lubridate)
library(tmap)
library(sf)
library(spdep)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD POLICE FORCE BOUNDARIES, COMBINE INTO SINGLE MAP----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Why on earth don't they provide this??
filelist <- list.files(path = "Data/local/forceboundaries/force kmls/", recursive = T, pattern = '*.kml', full.names = T)

#check
filelist

loadedfiles <- lapply(filelist, 
                      function(x){
                        st_read(x)
                        x$name = tools::file_path_sans_ext(basename(x))
                      }
                      )

loadedfiles <- lapply(filelist,st_read)
loadedfiles <- do.call(rbind, loadedfiles)#bind_rows doesn't work. Huh.

plot(st_geometry(loadedfiles), col='blue')

#Names plz! Order will be same, so...
loadedfiles$Name <- lapply(filelist, function(x) tools::file_path_sans_ext(basename(x))) %>% unlist
head(loadedfiles)

#Get neighbours for London.
#https://rdrr.io/cran/sf/man/st_zm.html - dropping 3D
police.neigh <- poly2nb(loadedfiles %>% st_zm() %>% as('Spatial'), row.names = loadedfiles$Name) 

#Unsurprisingly, not working.
summary(police.neigh)

#Maybe can export if get rid of 3D
st_write(loadedfiles %>% st_zm(), 'Data/local/policemap.shp')

#OK! So, surrounding London, we have:
#Essex
#Hertfordshire
#Kent
#Surrey
#Thames Valley

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD  CRIME DATA INTO MEMORY AND COMBINE INTO SINGLE DATAFRAME----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Having downloaded the London crime data
#(Which is in separate folders for each month)
#We want to:
#a) find the file names in each folder
#b) load all those files
#c) combine them into a single file

#Downloaded from:
#https://data.police.uk/data/

#Downloaded Metropolitan + City of London + all surrounding forces
#Last two years (Jan 2017 to Dec 2018)

filelist <- list.files(path = "Data/local/crime", recursive = T, pattern = '*.csv', full.names = T)

#check
filelist

loadedfiles <- lapply(filelist, read_csv)

#And now to combine into single dataframe...
#bind_rows is a dplyr function.
crime.data <- bind_rows(loadedfiles)


#While we're here: we want R to know that the 'month' column is a date.
#https://github.com/tidyverse/lubridate/issues/630
crime.data$Month <- ymd(crime.data$Month, truncated = 1)

#Quick look
# ggplot(crime.data, aes(x = Month)) +
#   geom_bar()
# 
# #Or break down by crime type
ggplot(crime.data, aes(x = Month, fill = `Crime type`)) +
  geom_bar()

#Sum of crime by type?
ggplot(crime.data, aes(x = `Crime type`)) +
  geom_bar() +
  coord_flip()



#Save a copy of that single file
saveRDS(crime.data, 'Data/local/London_and_surrounds_crimes_combined.rds')
crime.data <- readRDS('Data/local/London_and_surrounds_crimes_combined.rds')

#How many duplicated pairs of coordinates?
x <- distinct(crime.data %>% select(Longitude,Latitude))
#That's ~20 using the same location points.
nrow(x)/nrow(crime.data)

#Look in QGIS.
write.csv(x, 'Data/local/crimelocationpoints.csv')


#It's already got an LSOA code. Load in London LSOA data to check what kind of match we've got
merged.sf <- st_read('Data/londonlsoas_nonclipped_nongeneralised_w_nonUK_cob.shp')
#set CRS for later
merged.sf <- st_set_crs(merged.sf,value = 27700)


#We got all but four.
table(merged.sf$code %in% crime.data$`LSOA code`)

#I bet the four are wrongly coded. Or could actually have no crimes...
#Take a look at where we don't have a match
merged.sf$match <- merged.sf$code %in% crime.data$`LSOA code`

tm_shape(merged.sf) + tm_fill('match')

#Can't see there. QGIS!
st_write(merged.sf, 'Data/local/checkpolice.shp')

#Yeah, I don't believe that. We need to do a full point-in-polygon to check for those four not matching.
#Wrong LSOA code in police data, I predict.
#lsoa <- st_read('C:/Data/MapPolygons/England/2011/England_lsoa_2011_full/england_lsoa_2011.shp')
#Doesn't seem to have projection... well, has proj4string.
#lsoa <- st_set_crs(lsoa,value = 27700)

#Can use merged.sf for that, will be the correct LSOAs in London.

#Make a spatial version of the police data, just keep what we need
#Actually, let's keep all the data - can use it directly to replace the old one.
#crime.spatial <- crime.data %>% select(Longitude,Latitude,`LSOA code`)

#Drop any with missing location values.
table(is.na(crime.data$Longitude) & is.na(crime.data$`LSOA code`))
#It's never true that longitude is NA if LSOA code is not NA.
table(is.na(crime.data$Longitude) & !is.na(crime.data$`LSOA code`))

#Get crime data, drop NAs. OK to filter, we're not losing LSOA codes
crime.spatial <- crime.data %>% filter(!is.na(Longitude))
crime.spatial <- st_as_sf(crime.spatial, coords = c('Longitude','Latitude'), crs = 4326)
crime.spatial <- st_transform(crime.spatial, crs = 27700)

#That took some time. Save.
saveRDS(crime.spatial, 'Data/local/crimespatial_BNG.rds')
crime.spatial <- readRDS('Data/local/crimespatial_BNG.rds')



#check in QGIS


#OK, should be ready for overinz
#both <- st_join(lsoa,crime.spatial,join = st_intersects)
both <- st_join(merged.sf %>% select(code,name,match),crime.spatial,join = st_intersects)

#Reminder of what we've got / what we're looking for:
#Police `LSOA code` field may be wrong:
#There are four London LSOAs with no crime data, if matching by LSOA code.
#So check by point-in-polygon the actual crime locations.
#Then check on the four that apparently had no crime data in - what's the police data look like?

#We should get unique LSOAs.
#Duplicated seems vastly faster than distinct.
both.unique <- both[!duplicated(both$code),]

#Those four were... 
#Noting that we'll have dropped a bunch of police data, so it might not have got what we need.
#May need to get unique police LSOA. Let's see.
#Ah, the police version of the LSOA is NA... 
View(both.unique %>% filter(match == F))

#They are:
#Lambeth 024E       Westminster 002E   Tower Hamlets 031F Islington 006F
both.unique$name[both.unique$match == F]

#Except, no, there's no data there at all. There must be some with data to have intersected
#Check on one LSOA
View(both %>% filter(name == 'Lambeth 024E'))
View(both %>% filter(name %in% both.unique$name[both.unique$match == F]))


#Hum. I think it's telling me there are *still* no crimes in those zones.
#I have looked on a map, I do not believe this!

#Well for at least two it seems likely: river (tower hamlets), some kind of complex (islington)
#The remaining two are odd but with no geocoded crimes there, will have to believe it.



#Back to processing crime data.
#Sum total crimes and by type.
#Check on what kind of numbers we get. May have to group.

#This initially is total crime count for the two years we have data for.
#May subset to single year.
crime.counts.total <- crime.data %>% 
  filter(!is.na(Longitude)) %>% #Filter out NAs again.
  group_by(`LSOA code`) %>% 
  summarise(totalcrimes = n()) 

#%>% 
# ungroup() %>% 
# complete(`LSOA code`, fill = list(totalCrimes = 0))

#Any LSOAs with zero counts won't be listed
#As each row is a single crime.
#So need to add in missing LSOAs with a zero count
crime.counts.type <- crime.data %>% 
  filter(!is.na(Longitude)) %>% #Filter out NAs again.
  group_by(`LSOA code`,`Crime type`) %>% 
  summarise(totalCrimes = n())


#Make crimes wide so I can correlate-matrix them. Wanna see if spatial patterns similar for all.
types.wide <- crime.counts.type %>% 
  spread(key = `Crime type`, value = totalCrimes, fill = 0)

pairs(types.wide %>% ungroup() %>% select(-`LSOA code`))

#Bit hard to see from that but some share same spatial patterning.
#Could look into it more but might just go for two groups:
#All crimes and all crimes minus ASB (and violence? No, let's keep that.)

#So that's easy enough:
crime.counts.total.minus.asb <- crime.data %>% 
  filter(!is.na(Longitude),`Crime type`!='Anti-social behaviour') %>% #Filter out NAs again.
  group_by(`LSOA code`) %>% 
  summarise(totalcrimes_minusASB = n()) 

#Can merge into merged.sf and then fill in NA blanks for crime counts with zeros.
merged.sf.crime <- left_join(merged.sf, crime.counts.total, by = c('code' = 'LSOA code'))
merged.sf.crime <- left_join(merged.sf.crime, crime.counts.total.minus.asb, by = c('code' = 'LSOA code'))

#They're both actually highly correlated
plot(merged.sf.crime$totalcrimes ~ merged.sf.crime$totalcrimes_minusASB)
summary(lm(merged.sf.crime$totalcrimes ~ merged.sf.crime$totalcrimes_minusASB))

#Oh wait, they would be: it's a count of crimes, not adjusted for population
#Let's sort that right now
merged.sf.crime <- merged.sf.crime %>% 
  mutate(crimesperperson = totalcrimes/totalPop, crimes_minusASB_perperson = totalcrimes_minusASB/totalPop)

#Well, that made little difference!
plot(merged.sf.crime$crimesperperson ~ merged.sf.crime$crimes_minusASB_perperson)
summary(lm(merged.sf.crime$crimesperperson ~ merged.sf.crime$crimes_minusASB_perperson))


#Fill in any missings with zero crime count (those four LSOAs)
table(is.na(merged.sf.crime$crimesperperson))
table(is.na(merged.sf.crime$crimes_minusASB_perperson))

merged.sf.crime$crimesperperson[is.na(merged.sf.crime$crimesperperson)] <- 0
merged.sf.crime$crimes_minusASB_perperson[is.na(merged.sf.crime$crimes_minusASB_perperson)] <- 0

#Let's check in map.
st_write(merged.sf.crime,'Data/local/london_w_crimedata.shp')

#ASB doesn't make a whole lot of difference, spatial pattern wise. Well!
#Might as well just keep the one field in then.

#Keep it all just in case
saveRDS(merged.sf.crime,'Data/local/merged_sf_w_allcrimedata.rds')
merged.sf.crime <- readRDS('Data/local/merged_sf_w_allcrimedata.rds')

#reduce to single crime column. crimes_pp will survive in shapefile. Err, could have RDS'd it?
merged.sf.crime <- merged.sf.crime %>% select(-totalcrimes,-totalcrimes_minusASB,-crimes_minusASB_perperson,-match)

#Save with a slightly nicer name
#st_write(merged.sf.crime,'Data/londondata_lsoa.shp', delete_layer = T)

#Actually, RDS it to keep good names
saveRDS(merged.sf.crime,'Data/londondata_LSOA.rds')




