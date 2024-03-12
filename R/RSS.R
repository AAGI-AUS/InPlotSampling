#' Generate ranked set sampling (RSS) on the population provided.
#'
#' @inheritParams RSSDF
#' @param with_replacement A boolean which specifies whether to sample with replacement or not.
#'
#' @return A matrix with ranks from each ranker.
#'
RSS <- function(pop, n, H, K, with_replacement = FALSE) {
  verify_boolean(with_replacement)

  if (with_replacement) {
    return(RSSDF(pop, n, H, K))
  }
  return(RSSNRF(pop, n, H, K))
}
