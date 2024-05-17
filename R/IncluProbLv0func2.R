IncluProbLv02 <- function(popsize, firstorder) {

  secondorder <- matrix(0, ncol = popsize, nrow = popsize)

  #loop over all i and j combinations
  for (i in 1:popsize) {
    for (j in 1:popsize) {
      if (i != j) {
        secondorder[i, j] <- 1 - prod(1 - firstorder[, i]) -
          prod(1 - firstorder[, j]) +
          prod(1 - firstorder[, i] - firstorder[, j])
      }
    }
  }
  return(secondorder)
}


IncluProbLv02v2 <- function(popsize, firstorder) {

    # secondorder <- matrix(0, ncol = popsize, nrow = popsize)

    prodmat <- apply(1-firstorder, 2, prod)
    #loop over all i and j combinations
    secondorder <- secondmat(firstorder, prodmat, popsize)

    # secondorder[lower.tri(secondorder)] <- t(secondorder)[lower.tri(secondorder)]
    return(secondorder)
}
