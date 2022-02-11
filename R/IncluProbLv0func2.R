IncluProbLv02 <- function(popsize, firstorder) {
  
  secondorder <- matrix(0, ncol = popsize, nrow = popsize)
  
  #loop over all i and id combinations
  for (i in 1:popsize) {
    for (id in 1:popsize) {
      if (i != id) {
        secondorder[i, id] <- 1 - prod(1 - firstorder[, i]) -
          prod(1 - firstorder[, id]) +
          prod(1 - firstorder[, i] - firstorder[, id])
      }
    }
  }
  return(secondorder)
}
