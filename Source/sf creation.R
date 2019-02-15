#Process London data
library(tidyverse)
library(sf)
library(tmap)
#library(nomisr)
library(pryr)


#https://cloud.r-project.org/web/packages/nomisr/vignettes/introduction.html
# chk <- nomis_search(name = '*birth*')
# 
# #QS203EW - Country of birth (detailed)
# #nomis_overview("NM_524_1") %>% tidyr::unnest(name) %>% glimpse()
# 
# #What geography types are available?
# x <- nomis_get_metadata(id = "NM_524_1", concept = "geography", type = "type")
# 
# #we want info on LSOA... slow. 34753 obs - all LSOAs.
# x <- nomis_get_metadata(id = "NM_524_1", concept = "geography", type = "TYPE298")
# 
# #How many of those contain London in the label or description? Err, only six??
# table(grepl(pattern = 'London',x = x$description.en))
# 
# 
# #Oh well, let's do it geographically then. Get the LSOA country of birth data.
# cob.lsoa <- nomis_get_data(id = "NM_524_1", time = "latest", geography = "TYPE298")

#Oh, that's going to be too slow.

#~~~~~~~~~~
#LONDON OA----
#~~~~~~~~~~

# cob <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')
# 
# #Get London OA geog data. What part of London are we subsetting to I wonder?
# oa <- st_read('C:/Data/MapPolygons/England/2011/England_outputareas_2011_gen/England_oa_2011_gen.shp')
# 
# #Check we have zone match... tick.
# table(oa$CODE  %in% cob$geography)
# 
# #We have no area codes for the OAs. So need to decide on a subset. London TTWA?
# ttwas <- st_read('C:/Data/MapPolygons/England/2001/England_ttwa_2001/england_ttwa_2001.shp')
# 
# #projs match it looks like.
# london <- ttwas %>% filter(NAME == 'London')
# 
# #Which oas intersect?
# london.oa <- st_intersects(oa,london)
# 
# #Which gives us row IDs to keep... 
# london.oa2 <- data.frame(london.oa)
# 
# london.oas <- oa %>%
#   slice(london.oa2$row.id)
# 
# plot(st_geometry(london.oas))
# 
# #save and check. That's acting weird
# st_write(london.oas,'Data/temp/londonoas.shp')
# 
# 
# 
# #Hmm, feel like maybe LSOA would be better.
# 
# 
# 
# #~~~~~~~~~~~~~~~
# #LONDON LSOA----
# #~~~~~~~~~~~~~~~
# 
# lsoa <- st_read('C:/Data/MapPolygons/England/2011/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')
# 
# #I can aggregate OA cob values with a lookup. Where's the lookup...?
# 
# lookup <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Misc/CrimeDataProjectAug2018/filesForCrimeDataProject/data/oa_lookup2011.csv')
# 
# #Check OA match in CoB data. Lookup full match. I think lookup is just England.
# table(lookup$OA11CD %in% cob$geography)
# 
# #Merge LSOA code in, can then aggregate by LSOA.
# #Keep only a single unique OA/LSOA pair.
# cob <- left_join(cob,lookup %>% select(OA11CD,LSOA11CD) %>% distinct(), by = c('geography' = 'OA11CD'))
# 
# #Sum up COB values per LSOA
# cob.lsoa <- cob %>% 
#   group_by(LSOA11CD) %>% 
#   summarise_at(vars(England:`Rest of world`), funs(sum))
# 
# 
# 
# #Keep only LSOAs overlapping the London TTWA
# london.lsoaintersect <- st_intersects(lsoa,london)
# 
# #Which gives us row IDs to keep... 
# london.lsoa.rowIDs <- data.frame(london.lsoaintersect)
# 
# #Keep those ones!
# london.lsoas <- lsoa %>%
#   slice(london.lsoa.rowIDs$row.id)
# 
# #plot(st_geometry(london.lsoas))
# 
# #save and check.
# st_write(london.lsoas,'Data/maps/londonlsoas.shp')



#~~~~~~~~~~~~~~~~~
#USE LSOA FULL----
#~~~~~~~~~~~~~~~~~

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

#Sum up COB values per LSOA
cob.lsoa <- cob %>% 
  group_by(LSOA11CD) %>% 
  summarise_at(vars(England:`Rest of world`), funs(sum))



#Actually it turns out the GENERALISED VERSION DOESN'T WORK
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


#Add to map data
london.lsoas.nonclipped <- left_join(london.lsoas.nonclipped,cob.lsoa.nonUK,by = c('code' = 'LSOA11CD'))

#Looks sensible...
tm_shape(london.lsoas.nonclipped) +
  tm_polygons('nonUK', border.alpha = 0)


#save!
st_write(london.lsoas.nonclipped,'Data/londonlsoas_nonclipped_nongeneralised_w_nonUK_cob.shp')

