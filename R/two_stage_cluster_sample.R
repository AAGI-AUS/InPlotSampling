#' Generate two-stage cluster sampling on the population provided.
#'
#' @param pop Population that will be sampled with these ordered columns:
#' 1. Parent id: an index to denotes the parent of the record
#' 2. Parent auxiliary parameter: an auxiliary parameter for ranking parents
#' 3. Child auxiliary parameter: an auxiliary parameter for ranking children
#' @param sampling_strategies (first stage sampling strategy, second stage sampling strategy)
#' - `'srs'`: simple random sampling without replacement
#' - `'jps'`: JPS sampling without replacement
#' - `'jps_replace'`: JPS sampling with replacement
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
two_stage_cluster_sample <- function(pop, sampling_strategies, n, H, ni, Hi) {
  verify_boolean(replace)
}
