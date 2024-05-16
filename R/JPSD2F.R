#' Generate JPS sampling without replacement on the provided population.
#'
#' @inheritParams RSS
#' @param tau A parameter which controls ranking quality.
#'
#' @return A matrix with ranks from each ranker.
#'
JPSD2F <- function(pop, n, H, tau, K, with_replacement = FALSE) {
  verify_jps_params(pop, n, H, tau, K, with_replacement)

  sampling_matrix <- matrix(sample(pop, n * H, replace = with_replacement), ncol = H, nrow = n)

  # rank each SRS unit post experimentally
  jps_matrix <- matrix(0, ncol = K + 1, nrow = n)
  for (i in (1:n)) {
    comparison_set <- sampling_matrix[i, ]
    ranks <- rep(0, K)
    for (k in (1:K)) {
      # adjust for ranking, Dell and Clutter
      adjusted_set <- comparison_set + tau[k] * rnorm(H, 0, 1)
      ranks[k] <- rank(adjusted_set)[1]
    }
    jps_matrix[i, ] <- c(comparison_set[1], ranks)
  }

  colnames(jps_matrix) <- c("Y", paste0("R", 1:K))
  return(jps_matrix)
}
