
permutation.test <- function(variates,
                           W0,
                           W1,
                           iter) {
  
  ## variates---variables to test--- a numeric vertor without NAs
  ## W0---the geographical continguity weights matrix
  ## W1---the (usually) estimated boundary matrix
  ## iter--the number of permutation
  ## save the permutation results
  x <- variates
  n <- length(x)
  x.diff.matrix <- outer(x,x,FUN="-")
  x.diff.matrix <- abs(x.diff.matrix)
  
  ## paired differences amongst geographical neighbours
  x.diff.W0 <- x.diff.matrix[W0 == 1]
  x.diff.W0 <- unique(x.diff.W0)
  
  # paired differences on opposite sides of a social boundary
  x.diff.W1 <- x.diff.matrix[W1 == 0]
  x.diff.W1 <- unique(x.diff.W1)
  x.diff.W1 <- x.diff.W1[-1]
  
  # get the difference in means of x
  mean.diff <- mean(x.diff.W1) - mean(x.diff.W0)

  ## compute mean of differences for random permutation
  perm.diff <- rep(NA,iter)
  
  for(i in 1:iter){
    
    x.perm <- sample(x,n,replace=FALSE)
    
    x.perm.diff.matrix <- outer(x.perm,x.perm,FUN="-")
    x.perm.diff.matrix <- abs(x.perm.diff.matrix)
    
    x.perm.diff.W0 <- x.perm.diff.matrix[W0 == 1]
    x.perm.diff.W0 <- unique(x.perm.diff.W0)
    
    x.perm.diff.W1 <- x.perm.diff.matrix[W1 == 0]
    x.perm.diff.W1 <- unique(x.perm.diff.W1)
    x.perm.diff.W1 <- x.perm.diff.W1[-1]
    
    perm.diff[i] <- mean(x.perm.diff.W1) - mean(x.perm.diff.W0)
    
  }
  
  ## Compute the p-value
  all.diff <- c(mean.diff,perm.diff)
  rank <- rank(all.diff)[1]
  pval <- punif((iter-rank + 2)/(iter + 1))
  
  #### Return the results
  results <- list(statistic=mean.diff, rank=rank, pvalue=pval,all.diff=all.diff)
  return(results)
  
}

