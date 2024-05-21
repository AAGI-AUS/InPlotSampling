IncluProbLv1 <- function(popsize, sampsize, set, rank) {
  `%dopar%` <- foreach::`%dopar%`

  cl <- parallel::makeCluster(7)
  doParallel::registerDoParallel(cl)

  i <- firstorder <- NULL
  # Looping over all members of the population
  firstorder <- foreach::foreach(i = 1:popsize) %dopar% {
    # start with building the intermediate U matrix
    U <- matrix(0, ncol = popsize + 1, nrow = popsize)
    # First entry hardcoded
    U[1] <- 1

    # Running down rows of the U matrix
    for (j in 0:(i - 1)) {
      J <- j + 1

      # When on the 0th row
      if (j == 0) {
        # Running along the columns of the U matrix
        for (k in 0:(sampsize - 1)) {
          K <- k + 1
          start <- i + 1
          finish <- popsize - k
          # Lambda sum condition
          if (start <= finish) {
            L <- start:finish
            a <- choose(L - 1, rank[K] - 1)
            b <- choose(popsize - k - L, set - rank[K])
            c <- choose(popsize - k, set)
            # Calculate lambda summation
            lambdasum <- sum(a * b / c)
          } else {
            lambdasum <- 0
          }
          # Iteratively fill 0th row of U matrix
          U[J, K + 1] <- U[J, K] * lambdasum
        }
      } else {
        # Running along the columns of the U matrix
        for (k in 0:(sampsize - 1)) {
          K <- k + 1
          start1 <- i + 1 - j
          finish1 <- popsize - k
          start2 <- 1
          finish2 <- ((i - 1) - (j - 1))
          # Lambda sum condition
          if (start1 <= finish1) {
            L1 <- start1:finish1
            a1 <- choose(L1 - 1, rank[K] - 1)
            b1 <- choose(popsize - k - L1, set - rank[K])
            c1 <- choose(popsize - k, set)

            # Calculate lambda summation
            lambdasum1 <- sum(a1 * b1 / c1)
          } else {
            lambdasum1 <- 0
          }
          # Lambda sum condition
          if (start2 <= finish2) {
            L2 <- start2:finish2
            a2 <- choose(L2 - 1, rank[K] - 1)
            b2 <- choose(popsize - k - L2, set - rank[K])
            c2 <- choose(popsize - k, set)

            # Calculate lambda summation
            lambdasum2 <- sum(a2 * b2 / c2)
          } else {
            lambdasum2 <- 0
          }

          # Iteratively fill all other rows of U matrix
          U[J, K + 1] <- U[J, K] * lambdasum1 + U[J - 1, K] * lambdasum2
        }
      }
    }
    # Calculate ith inclusion probability from U matrix
    1 - sum(U[1:i, sampsize + 1])
  }
  return(unlist(firstorder))
  parallel::stopCluster(cl)
}
