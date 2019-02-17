library(tidyverse)
library(sf)
library(tmap)
library(pryr)

#Some tests
borders.sf <- readRDS('Data/local/london_borders_sf2.rds')

#How closely does rank of phi match rank of difference in the original variable?

#Add proportion non-UK back in again.
merged.sf <- readRDS('Data/londondata_LSOA.rds')

merged.sf$proportion_nonUK <- merged.sf$nonUK/merged.sf$totalPop

#One side of border...
borders.sf.chk <- left_join(borders.sf,merged.sf %>% select(LSOA,proportion_nonUK) %>% st_set_geometry(NULL), by = 'LSOA')
#Then the other.
borders.sf.chk <- left_join(borders.sf.chk,merged.sf %>% select(LSOA,proportion_nonUK) %>% st_set_geometry(NULL), by = c('LSOA.1' = 'LSOA'))

#Absolute diff in proportions
borders.sf.chk$propnonUKabsdiff <- abs(borders.sf.chk$proportion_nonUK.x-borders.sf.chk$proportion_nonUK.y)

#Yup, strong match.
cor(borders.sf.chk$abs_phi_scaled,borders.sf.chk$propnonUKabsdiff, method = 'spearman')

#Though looking at the actual pattern...
#So INLA method will rarely raise rank position, relative to if you'd ranked just by absolute contig difference
#But it will often lower it.
plot(rank(borders.sf.chk$abs_phi_scaled),rank(borders.sf.chk$propnonUKabsdiff))




