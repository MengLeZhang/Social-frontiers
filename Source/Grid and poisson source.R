##  Grid square source file
##  NOTE: I assume the borders.sf and borders.gfx file is already defined and
##  the crime grid sq files are laoded in
##  The variable save.grid.to is where the results are saved to

##  Load in packages ----

pkgs <- c('sf', 'tidyverse', 'tmap', 'nngeo', 'stargazer')
lapply(pkgs, library, character.only = T)

##  The dist to nearest frontier
print('Running distance to nearest frontier... do not stop')
grid2frontier <-
  st_nn(crime.grid,
        borders.gfx %>% subset(sig.frontier == T),
        returnDist = T) # If this takes really long (> 1 hour) parallelise
print('Distance to nearest frontier... done')

# print('Running distance to nearest border... do not stop')
# grid2border <- st_nn(crime.grid, borders.gfx, returnDist = T) # If this takes really long (> 1 hour) parallelise
# print('Distance to nearest border... done')

##  Append to the crime.grid data
crime.grid$min_frontier <- unlist(grid2frontier$dist)
# crime.grid$min_border <- unlist(grid2border$dist)

## Step three: Now to go and get results from a few regressions
head(crime.grid)
mod.fe <- glm(
  totalcrimepersquare ~ I(min_frontier < 100) +
    I(min_frontier < 200) +
    #                I(min_border < 100) + I(min_border < 200) +
    as.factor(OBJECTID),
  crime.grid,
  family = quasipoisson
)

##  Save results ----

saved.grid <- list(model = mod.fe, grid.sf = crime.grid)

saveRDS(saved.grid, save.grid.to)
print(
  'Results saved to' %>% paste(save.grid.to) %>% paste('total size (bytes) =') %>% paste (saved.grid %>% object.size)
)


#  Results tables output: NOT USED----
##  There are two ways to get tables: if you use markdown then we can directly just
##  get html tables printed
##  Else we can copy and paste a table

##  method 1
# stargazer(mod.fe, keep = 'min',
#           type = 'text',
#           title = 'FE model results',
#           dep.var.labels = 'Crime rate per grid',
#           add.lines = list(c('Dispersion parameter',
#                              (mod.fe %>% summary)$dispersion %>% round(3)),
#                            c('No. BSU', unique(crime.grid$OBJECTID))),
#           no.space = T
# )

##  Crime rate per grid