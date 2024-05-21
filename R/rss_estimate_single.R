#' This function Computes RSS estimator and its variance without replacement sampling
#'
#' @param first_ranker_ranks Ranks from the first ranker.
#' @param y Response variable.
#' @param set_size Set size.
#' @param pop_size Population size.
#'
#' @return
#' @keywords internal
#'
rss_estimate_single <- function(first_ranker_ranks, y, set_size, pop_size) {
  n <- length(y)
  indices <- 1:n

  # i, j index for pair-wise differences
  ij <- expand.grid(indices, indices)
  df_rank <- data.frame(first_ranker_ranks)
  estimated_mean <- mean(aggregate(y, df_rank, mean)$x)

  # Y_i, Y_j pairs
  yiyj <- expand.grid(y, y)[ij[, 1] != ij[, 2], ]
  rank_count <- aggregate(first_ranker_ranks, df_rank, length)$x
  # create the pairs of ranks h, h' that corresponds to Y_{[h]i} and Y_{[h']j}
  rank_pairs <- expand.grid(first_ranker_ranks, first_ranker_ranks)[ij[, 1] != ij[, 2], ]

  rss_y <- (yiyj[, 1] - yiyj[, 2])^2
  rss_by_rank <- aggregate(rss_y, rank_pairs, mean)

  rss_equal_h <- rss_by_rank[rss_by_rank[, 1] == rss_by_rank[, 2], ]$x
  total_rss_equal_h <- sum(rss_equal_h / rank_count) / (2 * set_size^2)

  estimated_variance <- total_rss_equal_h - sum(rss_by_rank$x) / (2 * set_size^2) / pop_size
  if (estimated_variance < 0) {
    estimated_variance <- total_rss_equal_h / 2
  }

  return(c(estimated_mean, estimated_variance))
}
