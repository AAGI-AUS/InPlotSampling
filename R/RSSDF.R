#' Generate ranked set sampling (RSS) with replacement on the population provided.
#'
#' @param pop Population that will be sampled with an auxiliary parameter in the second column.
#' @param n Sample size.
#' @param H Set size for each raking group.
#' @param K Number of rankers.
#'
#' @return A matrix with ranks from each ranker.
#'
RSSDF <- function(pop, n, H, K) {
  verify_rss_params(pop, n, H, K)

  n_sets <- n / H
  y <- pop[, 1]
  auxiliary_param <- pop[, 2]
  n_population <- length(y)
  rss_matrix <- matrix(0, ncol = (K + 1), nrow = n)
  sample_index <- 1
  for (j in (1:n_sets)) {
    for (h in (1:H)) {
      sampled_ids <- sample(1:n_population, H)
      sampled_y <- y[sampled_ids]
      sampled_auxiliary <- auxiliary_param[sampled_ids]

      auxiliary_order <- order(sampled_auxiliary)
      ordered_sampled_y <- sampled_y[auxiliary_order]
      ordered_sampled_auxiliary <- sampled_auxiliary[auxiliary_order]
      ordered_sample_ids <- sampled_ids[auxiliary_order]

      rss_matrix[sample_index, c(1, 2)] <- c(ordered_sampled_y[h], h)
      base_auxiliary <- ordered_sampled_auxiliary[h]
      auxiliary_wo_base <- auxiliary_param[-ordered_sample_ids[h]]
      if (K > 1) {
        for (k in (2:K)) {
          comparison_set <- c(base_auxiliary, sample(auxiliary_wo_base, (H - 1)))
          ordered_set <- comparison_set[order(comparison_set)]
          rank_k <- which(ordered_set == base_auxiliary)
          if (length(rank_k) > 1) {
            rank_k <- sample(rank_k, 1)
          }
          rss_matrix[sample_index, (k + 1)] <- rank_k
        }
      }
      sample_index <- sample_index + 1
    }
  }
  return(rss_matrix)
}
