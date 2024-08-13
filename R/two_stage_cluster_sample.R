#' Generate two-stage cluster sampling on the population provided.
#'
#' @param pop Population that will be sampled with these ordered columns:
#' 1. Parent id: an index to denotes the parent of the record
#' 2. Parent auxiliary parameter: an auxiliary parameter for ranking parents
#' 3. Child auxiliary parameter: an auxiliary parameter for ranking children
#' @param sampling_strategies (first stage sampling strategy, second stage sampling strategy), e.g.,
#'   `c('srs', 'jps')`.
#' - `'srs'`: simple random sampling without replacement
#' - `'jps'`: JPS sampling
#' @param n Number of samples in the first stage.
#' @param H Set size for each ranking group in the first stage.
#' @param replace A boolean which specifies whether to sample with replacement or not in the first stage
#'   (applicable only for JPS sampling).
#' @param ni Number(s) of samples in the second stage. Can be a single number or a vector of `n` numbers.
#' @param Hi Set size for each ranking group in the second stage. Can be a single number or a vector of `n`
#'   numbers.
#' @param replace_i A boolean which specifies whether to sample with replacement or not in the second stage
#'   (applicable only for JPS sampling).
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
two_stage_cluster_sample <- function(pop, sampling_strategies, n, H, replace, ni, Hi, replace_i) {
  # TODO: verify params and add tests

  pop <- cbind(pop, seq_len(dim(pop)[1]))
  parent <- unique(pop[, c(1, 2)])
  first_strategy <- sampling_strategies[1]
  second_strategy <- sampling_strategies[2]

  # first stage
  if (first_strategy == "srs") {
    first_stage_indices <- sample(seq_len(dim(parent)[1]), n)
    first_stage_sample <- cbind(parent[first_stage_indices, ], 0)
  } else if (first_strategy == "jps") {
    first_stage_sample <- jps_sample(parent[, 2], n, H, 0, 1, replace, TRUE)

    first_stage_indices <- first_stage_sample[, 1]
    first_stage_sample[, 1] <- parent[first_stage_indices, 1]
  }
  first_stage_sample <- first_stage_sample[, c(1, 3)]
  # first_sampled_pop <- pop[pop[, 1] %in% first_stage_indices, ]

  if (length(ni) == 1) {
    ni <- rep(ni, n)
  }
  if (length(Hi) == 1) {
    Hi <- rep(Hi, n)
  }

  sampling_matrix <- matrix(nrow = 0, ncol = 5)
  for (i in 1:n) {
    parent_filter <- pop[, 1] == first_stage_sample[i, 1]
    children <- pop[parent_filter, c(4, 3)]

    if (second_strategy == "srs") {
      second_stage_indices <- sample(seq_len(dim(children)[1]), ni[i])
      second_stage_sample <- cbind(children[second_stage_indices, ], 0)
    } else if (second_strategy == "jps") {
      second_stage_sample <- jps_sample(children[, 2], ni[i], Hi[i], 0, 1, replace, TRUE)

      second_stage_indices <- second_stage_sample[, 1]
      second_stage_sample[, 1] <- children[second_stage_indices, 1]
    }
    print(second_stage_sample)
  }

  return(first_stage_sample)
}

# TODO: move to example
set.seed(112)
parent_size <- 300
child_size <- 50
# the number of samples to be ranked in each set
H <- 3

sampling_strategies <- c("srs", "jps")
replace <- FALSE
mu <- 10
sigma <- 4
n <- 9

parent_indices <- rep(1:parent_size, child_size)
parent_aux <- abs(qnorm(1:parent_size / (parent_size + 1), mu, sigma) + 5 * rnorm(parent_size, 0, 1))
child_aux <- abs(parent_aux + 10 * rnorm(parent_size * child_size, 0, 1))

population <- cbind(parent_indices, rep(parent_aux, child_size), child_aux)
two_stage_cluster_sample(population, sampling_strategies, n, H, replace, 6, 3, FALSE)
