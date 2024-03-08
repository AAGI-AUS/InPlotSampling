#' Generate JPS sampling without replacement on the provided population.
#'
#' @inheritParams RSS
#' @param tau A parameter which controls ranking quality.
#'
#' @return
#' @keywords internal
#'
JPSD2F <- function(pop, n, H, tau, K, with_replacement) {
  sampling_matrix <- matrix(sample(pop, n * H, replace = with_replacement), ncol = H, nrow = n)

  # construct rank for each SRS unit post experimentally
  JPS <- matrix(0, ncol = K + 1, nrow = n) # store JPS sample
  for (i in (1:n)) {
    Set <- sampling_matrix[i, ] # select comparison set i
    ranks <- rep(0, K) # initialize to store ranks of the rankers for comparison set i
    for (k in (1:K)) {
      DCSet <- Set + tau[k] * rnorm(H, 0, 1) # adjust ranking quality using Dell-Clutter
      # model
      RankSet <- rank(DCSet) # ranks the units in the comparison set i by ranker k
      ranks[k] <- RankSet[1] #  the rank of the i-th measured unit by ranker k
    }
    JPS[i, ] <- c(Set[1], ranks) # measured value of unit i and ranks by k rankers
  }
  colnames(JPS) <- c("Y", paste("R", 1:K, sep = ""))
  return(JPS)
}
