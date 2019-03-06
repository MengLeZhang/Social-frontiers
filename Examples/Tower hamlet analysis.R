
pkgs <- c('sf', 'tidyverse', 'tmap',
          'spdep',
          # 'foreach',
          # 'doParallel',
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

tower.sf <- readRDS('Data/towerhamlets_LSOA.rds')
y <- 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)
###

source('Source/inla_frontier.R')
source('Source/summary_frontier_model.R')

frontier_model <-
  inla_frontier(data = tower.sf, y = 'nonUK', n.trials = 'totalPop')
summary(frontier_model)


frontier_model$W.frontiers %>% as('sparseMatrix')
object.size(frontier_model$W.frontiers)
