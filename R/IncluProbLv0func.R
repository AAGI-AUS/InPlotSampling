IncluProbLv0 <- function(popsize,sampsize,rank,set){

  #initialise alpha matrix nxn
  alphamat <- matrix(0, ncol = popsize, nrow = sampsize)

  #calculate the combinatorix (put into function later)
  for (i in 1:popsize) {
    a <- choose(i - 1, rank - 1)
    b <- choose(popsize - i, set - rank)
    c <- choose(popsize, set)
    alphamat[, i] <- a * b / c
  }
  return(alphamat)
}


inclusion_prob_lv0 <- function(popsize, sampsize, rank, set) {
    alphamat <- matrix(
        c(1, rep(0, set - 1)),
        ncol = set,
        nrow = popsize,
        byrow = TRUE
    )

    for (i in 2:popsize) {
        for (j in 2:set) {
            alphamat[i, j] <- alphamat[i-1, j-1] + alphamat[i-1, j]
        }
    }
    alphamat <- alphamat * rev(alphamat) / choose(popsize, set)

    output <- rep(alphamat, length(rank)/set)
    output <- matrix(output, ncol = popsize, byrow = TRUE)
}

#' @importFrom Rcpp sourceCpp
#' @useDynLib RankedSetSampling, .registration = TRUE
inclusion_prob_lv0_cpp <- function(popsize, sampsize, rank, set) {
    alphamat <- matrix(
        c(1, rep(0, set - 1)),
        ncol = set,
        nrow = popsize,
        byrow = TRUE
    )

    alphamat <- pascal(alphamat, popsize, set)

    alphamat <- alphamat * rev(alphamat) / choose(popsize, set)

    output <- rep(alphamat, length(rank)/set)
    output <- matrix(output, ncol = popsize, byrow = TRUE)
}
#
# x1 <- IncluProbLv0(popsize = 100000, sampsize = 100, rank = rep(1:5, length.out = 100), set = 5)
# x2 <- inclusion_prob_lv0(1000000, 100, rep(1:5, length.out = 100), 5)
# x3 <- inclusion_prob_lv0_cpp(1000000, 100, rep(1:5, length.out = 100), 5)
# all.equal(x2, x3)


# library(microbenchmark)
#
# microbenchmark(
#     IncluProbLv0(100000, 100, rep(1:5, length.out = 100), 5),
#     inclusion_prob_lv0(100000, 100, rep(1:5, length.out = 100), 5),
#     inclusion_prob_lv0_cpp(100000, 100, rep(1:5, length.out = 100), 5), times = 20
# )
