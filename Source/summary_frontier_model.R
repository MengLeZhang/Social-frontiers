summary.frontier_model <- function(object, ...){
  ## Display the numbers of frontiers
  borders.tab <- tabulate(object$W.estimated_cleaned + 1) # so table will convert integer to
  #factor first so the quicker way is to use tabulate which will only display
  ##  coutners of positive intergers
  
  print('Total N. of borders')
  print(sum(borders.tab))
  print('N. of borders: frontier vs non-frontier')
  print(borders.tab) 
  
  
}


