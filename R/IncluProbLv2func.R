IncluProbLv2 <- function(popsize, sampsize, set, rank) {
  `%dopar%` <- foreach::`%dopar%`

  cl <- parallel::makeCluster(7)
  doParallel::registerDoParallel(cl)

  i <- NULL
  firstorder <- foreach::foreach(i = 1:popsize, .combine = "cbind") %dopar% {
    # do the calculations
    a <- choose(i - 1, rank - 1)
    b <- choose(popsize - i, set - rank)
    c <- choose(popsize, set)
    # put total into alpha matrix
    (a * b / c)
  }
  return(unlist(firstorder))
  parallel::stopCluster(cl)
}
