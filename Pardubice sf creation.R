##  BSU dataset creation: This is the file that will join the data on crime and
##  ethnicity to a BSU file to be used for future analysis.
##  Start: 9/8/2018

library(tidyverse)
library(sf)
library(tmap)

##  1) Load in the original BSU ----
bsu.sf <-
  st_read(dsn = './Data/shp BSU', layer = 'shp') #change to where you data is
bsu.sf$KOD_ZSJ_D <-
  bsu.sf$KOD_ZSJ_D %>% as.character %>% as.numeric #convert to numeric (i.e w/leading zeros)
head(bsu.sf)


##  2) Load in the ethnicity, crime, and cultural distance file

##  Cultural distance
cul.df <-
  './Data/culturallydistant_n_close_BSUcounts.rds' %>% readRDS
cul.wide <-
  cul.df %>% spread(key = cd_flag, value = count) #just convert to wide format
cul.wide$KOD_ZSJ_D <-
  cul.wide$BSU_CODE %>% as.character %>% as.numeric
cul.wide[is.na(cul.wide)] <- 0

##  ethnicity
ethnicity.df <-
  read.csv("./Data/ethnicity pardubice.csv", row.names = 1)
ethnicity.df

##  Crime
crime <-
  read.csv("./Data/crime1.csv", row.names = 1) # change to path for crime data
head(crime)

##  resident status
residence <- read.csv("./Data/residency.csv")
residence[is.na(residence)] <- 0 #getting rid of nas
residence$KOD_ZSJ_D <-
  residence$BSU_CODE %>% as.character %>% as.numeric
##  ignore warning -- this is to do with converting text to numeric

##  Merge all the auxiliary info
aux.df <-
  ethnicity.df %>% merge(cul.wide, by = 'KOD_ZSJ_D', all.x = T) %>% 
  merge(crime, by = 'KOD_ZSJ_D') %>%
  merge(residence, by = 'KOD_ZSJ_D', all.x = T)

sel.vars <- c('closer', 'further', 'permanent', 'temporary') #variables where NA = 0 (NA caused by no match)
##  Fix nas and turn 
for (i in 1:length(sel.vars)){
  temp.col <- aux.df[[sel.vars[i]]]
  aux.df[[sel.vars[i]]][temp.col %>% is.na] <- 0
}

aux.df[sel.vars]

##  Check the difference created by different varaibles

aux.df$out.by <-
  aux.df[, c('closer', 'further')] %>% rowSums - aux.df$For11
aux.df[c('closer', 'further', 'For11', 'out.by')]

aux.df$For11.v2 <- aux.df[, c('closer', 'further')] %>% rowSums
##  Check where its has gone wrong -- some cases are out but not by a lot

##  3) Check the map of pardubice
merged.sf <- bsu.sf %>% merge(aux.df, by = 'KOD_ZSJ_D')
merged.sf$prop.temp <- merged.sf$For11.v2 / merged.sf$Pop11

##  Data check
qtm(merged.sf %>% subset(Pop11 > 0), fill = 'prop.temp') #includes wierd zones like the hospital where prop foreigners is high
qtm(merged.sf %>% subset(Pop11 > 10), fill = 'prop.temp') # take out pops with less than 10?
qtm(merged.sf %>% subset(Pop11 > 10 &
                           prop.temp < 0.7), fill = 'prop.temp') #just to see the variation w/o the dorms
qtm(merged.sf %>% subset(Pop11 > 10 &
                           prop.temp < 0.7), fill = 'Pop11') # some high prop zones have low pop overall

##  Save the data
merged.sf %>% saveRDS(file = './Data/Cleaned Pardubice sf.RDS')
