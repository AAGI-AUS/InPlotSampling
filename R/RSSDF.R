#' Generate ranked set sampling (RSS) with replacement on the population provided.
#'
#' @param pop Population that will be sampled with an auxiliary parameter in the second column.
#' @param n Sample size to be sampled.
#' @param H Set size for each raking group.
#' @param K Number of rankers.
#'
#' @return A matrix with ranks from each ranker.
#'
RSSDF <- function(pop, n, H, K) {
  verify_rss_params(pop, n, H, K)

  n_sets <- n / H
  popY <- pop[, 1]
  popAux <- pop[, 2]
  N <- length(popY)
  RSSM <- matrix(0, ncol = (K + 1), nrow = n)
  ic <- 1
  for (j in (1:n_sets)) {
    for (h in (1:H)) {
      sampled_id <- sample(1:N, H)
      setY <- popY[sampled_id]
      setX <- popAux[sampled_id]
      auxiliary_order <- order(setX)
      ordered_setY <- setY[auxiliary_order]
      ordered_setX <- setX[auxiliary_order]
      ordered_sample_id <- sampled_id[auxiliary_order]
      RSSM[ic, c(1, 2)] <- c(ordered_setY[h], h)
      k1obs <- ordered_setX[h]
      redAux <- popAux[-ordered_sample_id[h]]
      if (K > 1) {
        for (k in (2:K)) {
          kset <- c(k1obs, sample(redAux, (H - 1)))
          OKS <- order(kset)
          koset <- kset[OKS]
          kR <- which(koset == k1obs)
          if (length(kR) > 1) kR <- sample(kR, 1)
          RSSM[ic, (k + 1)] <- kR
        }
      }
      ic <- ic + 1
    }
  }
  return(RSSM)
}
