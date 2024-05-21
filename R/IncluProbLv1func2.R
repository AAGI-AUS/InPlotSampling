IncluProbLv12 <- function(popsize, sampsize, set, rank, pi) {
  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  cl <- parallel::makeCluster(7)
  parallel::registerDoParallel(cl)

  # secondorder <- matrix(rep(0, popsize ^ 2), ncol = popsize, nrow = popsize)

  # Looping over all i < id
  i <- id <- NULL
  secondorder <- foreach::foreach(i = 1:popsize, .combine = rbind) %:%
    # Looping over all id > i (adjusted indice)
    foreach::foreach(id = 1:popsize) %dopar% {
      # Initiate 3D array
      V <- array(0, c(i, id, (sampsize + 1)))
      # Skip unncessary indices
      if (i < id) {
        # running down "x" coordinate
        for (j in 0:(i - 1)) {
          # Shifted index for R vector positioning (no 0)
          J <- j + 1
          # running down "y" coordinate
          for (jd in 0:(id - i - 1)) {
            # Shifted index for R vector positioning (no 0)
            JD <- jd + 1

            # First gate, when both x,y are zer0
            if (j == 0 && jd == 0) {
              # First entry hardcoded
              V[J, JD, 1] <- 1
              # running down "z" coordinate
              for (k in 0:(sampsize - 1)) {
                # Shifted index for R vector positioning (no 0)
                K <- k + 1
                # summation limits
                start <- (id + 1)
                finish <- (popsize - k)
                # summation is zero if a > b
                if (start <= finish) {
                  # summing over lambda
                  L <- start:finish
                  a <- choose(L - 1, rank[K] - 1)
                  b <- choose(popsize - k - L, set - rank[K])
                  c <- choose(popsize - k, set)

                  lambdasum <- sum(a * b / c)
                } else {
                  lambdasum <- 0
                }
                # Calculate V matrix entry
                V[J, JD, K + 1] <- V[J, JD, K] * lambdasum
              }
            }
            # Calculate rest of y coordinate for x = 0
            else if (j == 0 && 1 <= jd && jd <= (id - i - 1)) {
              # running down "z" coordinate
              for (k in 0:(sampsize - 1)) {
                # Shifted index for R vector positioning (no 0)
                K <- k + 1
                # summation limits
                start1 <- (id + 1 - jd)
                finish1 <- (popsize - k)
                start2 <- (i + 1)
                finish2 <- ((id - 1) - (jd - 1))

                # summation is zero if a > b
                if (start1 <= finish1) {
                  L1 <- start1:finish1
                  a1 <- choose(L1 - 1, rank[K] - 1)
                  b1 <- choose(popsize - k - L1, set - rank[K])
                  c1 <- choose(popsize - k, set)

                  lambdasum1 <- sum(a1 * b1 / c1)
                } else {
                  lambdasum1 <- 0
                }
                # summation is zero if a > b
                if (start2 <= finish2) {
                  L2 <- start2:finish2
                  a2 <- choose(L2 - 1, rank[K] - 1)
                  b2 <- choose(popsize - k - L2, set - rank[K])
                  c2 <- rep(choose(popsize - k, set), length(L2))

                  lambdasum2 <- sum(a2 * b2 / c2)
                } else {
                  lambdasum2 <- 0
                }
                # Calculate V matrix from previous jd entry
                V[J, JD, K + 1] <-
                  V[J, JD, K] * lambdasum1 + V[J, JD - 1, K] * lambdasum2
              }
            }

            # Calculate rest of x coordinate for y = 0
            else if (jd == 0 && 1 <= j && j <= i) {
              # running down "z" coordinate
              for (k in 0:(sampsize - 1)) {
                # Shifted index for R vector positioning (no 0)
                K <- k + 1
                # summation limits
                start3 <- (id + 1 - j)
                finish3 <- (popsize - k)
                start4 <- 1
                finish4 <- ((i - 1) - (j - 1))
                # summation is zero if a > b
                if (start3 <= finish3) {
                  L3 <- start3:finish3
                  a3 <- choose(L3 - 1, rank[K] - 1)
                  b3 <- choose(popsize - k - L3, set - rank[K])
                  c3 <- choose(popsize - k, set)

                  lambdasum3 <- sum(a3 * b3 / c3)
                } else {
                  lambdasum3 <- 0
                }
                # summation is zero if a > b
                if (start4 <= finish4) {
                  L4 <- start4:finish4
                  a4 <- choose(L4 - 1, rank[K] - 1)
                  b4 <- choose(popsize - k - L4, set - rank[K])
                  c4 <- choose(popsize - k, set)

                  lambdasum4 <- sum(a4 * b4 / c4)
                } else {
                  lambdasum4 <- 0
                }
                # Calculate V matrix entry from previous j entry
                V[J, JD, K + 1] <-
                  V[J, JD, K] * lambdasum3 + V[J - 1, JD, K] * lambdasum4
              }
            }
            # Finishing off array when neither j nor jd is 0
            else {
              for (k in 0:(sampsize - 1)) {
                # running down "z" coordinate
                K <- k + 1
                # summation limits
                start5 <- (id + 1 - j - jd)
                finish5 <- (popsize - k)
                start6 <- (i + 1 - j)
                finish6 <- ((id - 1) - j - (jd - 1))
                start7 <- 1
                finish7 <- ((i - 1) - (j - 1))
                # summation is zero if a > b
                if (start5 <= finish5) {
                  L5 <- start5:finish5
                  a5 <- choose(L5 - 1, rank[K] - 1)
                  b5 <- choose(popsize - k - L5, set - rank[K])
                  c5 <- choose(popsize - k, set)

                  lambdasum5 <- sum(a5 * b5 / c5)
                } else {
                  lambdasum5 <- 0
                }
                # summation is zero if a > b
                if (start6 <= finish6) {
                  L6 <- start6:finish6
                  a6 <- choose(L6 - 1, rank[K] - 1)
                  b6 <- choose(popsize - k - L6, set - rank[K])
                  c6 <- choose(popsize - k, set)

                  lambdasum6 <- sum(a6 * b6 / c6)
                } else {
                  lambdasum6 <- 0
                }
                # summation is zero if a > b
                if (start7 <= finish7) {
                  L7 <- start7:finish7
                  a7 <- choose(L7 - 1, rank[K] - 1)
                  b7 <- choose(popsize - k - L7, set - rank[K])
                  c7 <- choose(popsize - k, set)

                  lambdasum7 <- sum(a7 * b7 / c7)
                } else {
                  lambdasum7 <- 0
                }
                # Calculate V matrix entry from previous j and jd entry
                V[J, JD, K + 1] <- V[J, JD, K] * lambdasum5 +
                  V[J, JD - 1, K] * lambdasum6 +
                  V[J - 1, JD, K] * lambdasum7
              }
            }
          }
        }
        # Calculating probabilitiesj <- 0:i-1 is 1:i
        1 - (1 - pi[i]) - (1 - pi[id]) +
          sum(sum(V[1:(i), 1:(id - i), (sampsize + 1)]))
      }
    }

  return(secondorder)
  parallel::stopCluster(cl)
}
