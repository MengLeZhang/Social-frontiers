library(tidyverse)
library(sf)
library(tmap)
library(pryr)

#~~~~~~~~~~~~~~~~~
#COMBINE LONDON LSOAS WITH CENSUS DATA----
#~~~~~~~~~~~~~~~~~

#NOTE: ONLY USE FULL LSOA DATA, NOT GENERALISED
#Generalised doesn't work for neighbour detection or boundary extraction

#Census data:
#Country of birth
cob <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')

#employment data
ea <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/2011/GreatBritain/2011_econActive/GB_2011_econActive_tidied.csv')

#f243: total
#f244: econ active
#f248: unemployed
#f251: econ inactive
ea <- ea %>% dplyr::select(code = GEO_CODE, econActive = F244, unemployed = F248, econInactive = F251)


#We have no area codes for the OAs. So need to decide on a subset. London TTWA?
ttwas <- st_read('C:/Data/MapPolygons/England/2001/England_ttwa_2001/england_ttwa_2001.shp')

#projs match it looks like.
london <- ttwas %>% filter(NAME == 'London')

lookup <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Misc/CrimeDataProjectAug2018/filesForCrimeDataProject/data/oa_lookup2011.csv')

#Check OA match in CoB data. Lookup full match. I think lookup is just England.
table(lookup$OA11CD %in% cob$geography)

#Merge LSOA code in, can then aggregate by LSOA.
#Keep only a single unique OA/LSOA pair.
cob <- left_join(cob,lookup %>% select(OA11CD,LSOA11CD) %>% distinct(), by = c('geography' = 'OA11CD'))
ea <- left_join(ea,lookup %>% select(OA11CD,LSOA11CD) %>% distinct(), by = c('code' = 'OA11CD'))

#Sum up COB values per LSOA
cob.lsoa <- cob %>% 
  group_by(LSOA11CD) %>% 
  summarise_at(vars(England:`Rest of world`), funs(sum))

#Sum up EA values per LSOA
ea.lsoa <- ea %>% 
  group_by(LSOA11CD) %>% 
  summarise_at(vars(econActive:unemployed), funs(sum))

#Include % unemployed per zone
ea.lsoa <- ea.lsoa %>% 
  mutate(percentunemployed = round((unemployed/econActive)*100,2))




#The GENERALISED VERSION DOESN'T WORK
#For neighbour detection or border extraction. Soooo the full version needed:
lsoa.nonclipped <- st_read('C:/Data/MapPolygons/England/2011/England_lsoa_2011_full/england_lsoa_2011.shp')

#Row ID order is not the same
london.lsoaintersect <- st_intersects(lsoa.nonclipped,london)

#Which gives us row IDs to keep... 
london.lsoa.rowIDs <- data.frame(london.lsoaintersect)

london.lsoas.nonclipped <- lsoa.nonclipped %>%
  slice(london.lsoa.rowIDs$row.id)

#plot(st_geometry(london.lsoas))

#save and check. TICK.
st_write(london.lsoas.nonclipped,'Data/local/londonlsoasnonclipped_nongeneralised.shp')

#Reload that for re-running
london.lsoas.nonclipped <- st_read('Data/local/londonlsoasnonclipped.shp')


#SUMMARISE COB DATA TO GET JUST UK / NON-UK
#THEN MERGE INTO MAP
cob.lsoa.nonUK <- cob.lsoa %>% 
  mutate(UK = rowSums(.[2:5]), nonUK = rowSums(.[6:45]), totalPop = rowSums(.[2:45])) %>% 
  select(LSOA11CD,UK,nonUK,totalPop)


#Add cob to map data
london.lsoas.nonclipped <- left_join(london.lsoas.nonclipped,cob.lsoa.nonUK,by = c('code' = 'LSOA11CD'))

#add ea to map data
london.lsoas.nonclipped <- left_join(london.lsoas.nonclipped,ea.lsoa,by = c('code' = 'LSOA11CD'))


#Looks sensible...
tm_shape(london.lsoas.nonclipped) +
  tm_polygons('nonUK', border.alpha = 0)


#save!
#st_write(london.lsoas.nonclipped,'Data/londonlsoas_nonclipped_nongeneralised_w_nonUK_cob.shp')
saveRDS(london.lsoas.nonclipped,'Data/local/londonLSOAs_w_cob_ea.rds')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINE LSOA+CENSUS WITH CRIME DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Collated in crimeDataCollate.R
#That script also explored various crime types. 
#Using all crimes over a two year period was the conclusion - 
#Leaving out e.g. anti-social behaviour makes little difference to spatial pattern.

#Also: four zones don't have crimes. Checked thoroughly, it's not a geocoding error.
#So setting those to zero crimes
#For two of the zones, seems implausible...
crime.data <- readRDS('Data/local/London_and_surrounds_crimes_combined.rds')

crime.counts.total <- crime.data %>% 
  filter(!is.na(Longitude)) %>% #Filter out NAs. Deal with shortly.
  group_by(`LSOA code`) %>% 
  summarise(totalcrimes = n()) 

london.lsoas.nonclipped <- left_join(london.lsoas.nonclipped, crime.counts.total, by = c('code' = 'LSOA code'))

#Crimes per person
london.lsoas.nonclipped <- london.lsoas.nonclipped %>% mutate(crimesperperson = totalcrimes/totalPop)

#Fill in any missings with zero crime count (those four LSOAs)
table(is.na(london.lsoas.nonclipped$crimesperperson))
london.lsoas.nonclipped$crimesperperson[is.na(london.lsoas.nonclipped$crimesperperson)] <- 0

#Some renaming and dropping
london.lsoas.nonclipped <- london.lsoas.nonclipped %>% 
  select(-label) %>% 
  rename(LSOA = code, LSOAname = name)
  

#RDS it to keep good names rathen than let shapefile format reduce them
saveRDS(london.lsoas.nonclipped,'Data/londondata_LSOA.rds')












