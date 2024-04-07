#' Calculate Agreement Weights
#'
#' @param RV Ranking values
#' @param set_size Set size
#'
#' @return A vector of agreement weights
#' @keywords internal
#'
calculate_agreement_weights <- function(ranks, set_size) {
  unique_ranks <- unique(ranks)
  weights <- rep(0, set_size)
  n_rankers <- length(ranks)
  for (h in (seq_along(unique_ranks))) {
    rank_ <- unique_ranks[h]
    weights[rank_] <- length(ranks[ranks == rank_]) / n_rankers
  }
  return(weights)
}
