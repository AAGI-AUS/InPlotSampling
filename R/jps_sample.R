#' Generate JPS sampling on the provided population.
#'
#' @inheritParams rss_sample
#' @param tau A parameter which controls ranking quality.
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
#' with_replacement <- FALSE
#' sigma <- 4
#' mu <- 10
#' n_rankers <- 3
#' # sample size
#' n <- 10
#'
#' rhos <- rep(0.75, n_rankers)
#' taus <- sigma * sqrt(1 / rhos^2 - 1)
#'
#' population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)
#' jps_sample(population, n, H, taus, n_rankers, with_replacement)
#' #>               Y R1 R2 R3
#' #>  [1,]  6.384461  1  2  2
#' #>  [2,]  1.485141  1  1  1
#' #>  [3,] 13.640711  2  3  2
#' #>  [4,] 15.809136  3  3  2
#' #>  [5,]  6.769463  2  2  1
#' #>  [6,] 14.355524  3  3  3
#' #>  [7,] 10.729740  2  1  3
#' #>  [8,]  6.152453  1  1  1
#' #>  [9,]  8.701285  2  1  2
#' #> [10,] 13.323884  3  3  3
#'
jps_sample <- function(pop, n, H, tau, K, with_replacement = FALSE) {
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
#' @export
