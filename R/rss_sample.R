#' Generate ranked set sampling (RSS) on the population provided.
#'
#' @inheritParams rss_sample_w_replacement
#' @param replace A boolean which specifies whether to sample with replacement or not.
#'
#' @return A matrix with ranks from each ranker.
#' @export
#'
#' @examples
#' set.seed(112)
#' population_size <- 600
#' # the number of samples to be ranked in each set
#' H <- 3
#'
#' replace <- FALSE
#' sigma <- 4
#' mu <- 10
#' n_rankers <- 3
#' # sample size
#' n <- 9
#'
#' population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)
#' rho <- 0.75
#' tau <- sigma * sqrt(1 / rho^2 - 1)
#' x <- population + tau * rnorm(population_size, 0, 1)
#'
#' population <- cbind(population, x)
#' rss_sample(population, n, H, n_rankers, replace)
#' #>            [,1] [,2] [,3] [,4]
#' #>  [1,]  8.910625    1    3    1
#' #>  [2,] 12.317145    2    2    1
#' #>  [3,] 11.619746    3    3    2
#' #>  [4,]  8.307549    1    2    1
#' #>  [5,]  5.089992    2    2    3
#' #>  [6,]  7.233575    3    3    3
#' #>  [7,] 11.601654    1    2    2
#' #>  [8,]  9.134107    2    1    1
#' #>  [9,] 12.960431    3    3    1
#'
rss_sample <- function(pop, n, H, K, replace = FALSE) {
  verify_boolean(replace)

  if (replace) {
    return(rss_sample_w_replacement(pop, n, H, K))
  }
  return(rss_sample_wo_replacement(pop, n, H, K))
}

#' Generate ranked set sampling (RSS) with replacement on the population provided.
#'
#' @param pop Population that will be sampled with an auxiliary parameter in the second column.
#' @param n Sample size.
#' @param H Set size for each ranking group.
#' @param K Number of rankers.
#'
#' @return A matrix with ranks from each ranker.
#' @keywords internal
#'
rss_sample_w_replacement <- function(pop, n, H, K) {
  verify_rss_params(pop, n, H, K)

  n_sets <- n / H
  y <- pop[, 1]
  auxiliary_param <- pop[, 2]
  n_population <- dim(pop)[[1]]
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

#' Generate ranked set sampling (RSS) without replacement on the population provided.
#'
#' @inheritParams rss_sample_w_replacement
#'
#' @return A matrix with ranks from each ranker.
#' @keywords internal
#'
rss_sample_wo_replacement <- function(pop, n, H, K) {
  verify_rss_wo_replace_params(pop, n, H, K)

  n_sets <- n / H
  n_cols <- K + 1
  rseq <- rep((1:H), times = n_sets)
  y <- pop[, 1]
  n_population <- dim(pop)[[1]]
  auxiliary_param <- pop[, 2]
  all_indices <- 1:n_population
  rss_matrix <- matrix(0, ncol = (n_cols), nrow = n)

  sample_indices <- sample(all_indices, n * H)
  sample_y <- matrix(y[sample_indices], ncol = H, nrow = n)
  sample_x <- matrix(auxiliary_param[sample_indices], ncol = H, nrow = n)
  sample_index_matrix <- matrix(sample_indices, ncol = H, nrow = n)
  sample_matrix <- cbind(rseq, sample_y, sample_x, sample_index_matrix)
  yx_order <- t(apply(sample_matrix, 1, ORD))
  rss_matrix[, c(1, 2)] <- c(yx_order[, 1], rseq)

  # TODO: refactor
  y_indices <- yx_order[, 3]
  redpopind <- all_indices[-y_indices]
  Xh <- yx_order[, 2]
  if (n_cols > 2) {
    for (k in (3:n_cols)) {
      indk <- sample(redpopind, n * (H - 1))
      setX <- matrix(auxiliary_param[indk], ncol = (H - 1), nrow = n)
      indexM <- matrix(indk, ncol = (H - 1), nrow = n)
      indexM <- cbind(y_indices, indexM)
      setX <- cbind(Xh, setX)
      setYX <- cbind(Xh, setX, indexM)
      RankK <- apply(setYX, 1, ORDk)
      rss_matrix[, k] <- RankK
    }
  }

  return(rss_matrix)
}
