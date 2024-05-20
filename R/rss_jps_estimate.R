#' Estimate means from RSS or JPS sample.
#'
#' @param data A data frame of `JPS` or `RSS` rankings.
#' @param set_size The set size of the ranks.
#' @param method A method used to sample:
#' - `"JPS"`: Judgment-post stratified sampling
#' - `"RSS"`: Ranked set sampling
#' @param confidence The confidence level to use.
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model_based An inference mode:
#' - `FALSE`: design based inference
#' - `TRUE`: model based inference using super population model
#' @param pop_size The population size. Must be provided if sampling without replacement, or if `model` is `1`.
#'
#' @return A `data.frame` with the point estimates provided by different types of estimators along with standard
#'   error and confidence intervals.
#' @export
#'
#' @examples
#' # JPS estimator
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
#' n <- 30
#'
#' rhos <- rep(0.75, n_rankers)
#' taus <- sigma * sqrt(1 / rhos^2 - 1)
#' population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)
#' data <- RankedSetSampling::jps_sample(population, n, H, taus, n_rankers, with_replacement)
#' data <- data[order(data[, 2]), ]
#'
#' rss_jps_estimate(
#'   data,
#'   set_size = H,
#'   method = "JPS",
#'   confidence = 0.80,
#'   replace = with_replacement,
#'   model_based = FALSE,
#'   pop_size = population_size
#' )
#' #>          Estimator Estimate Standard Error 80% Confidence intervals
#' #> 1       UnWeighted    9.570          0.526               8.88,10.26
#' #> 2      Sd.Weighted    9.595          0.569             8.849,10.341
#' #> 3 Aggregate Weight    9.542          0.500             8.887,10.198
#' #> 4     JPS Estimate    9.502          0.650             8.651,10.354
#' #> 5     SRS estimate    9.793          0.783             8.766,10.821
#' #> 6          Minimum    9.542          0.500             8.887,10.198
#'
#' # RSS estimator
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
#' n <- 30
#'
#' population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)
#' rho <- 0.75
#' tau <- sigma * sqrt(1 / rho^2 - 1)
#' x <- population + tau * rnorm(population_size, 0, 1)
#'
#' population <- cbind(population, x)
#' data <- RankedSetSampling::rss_sample(population, n, H, n_rankers, with_replacement)
#' data <- data[order(data[, 2]), ]
#'
#' rss_estimates <- rss_jps_estimate(
#'   data,
#'   set_size = H,
#'   method = "RSS",
#'   confidence = 0.80,
#'   replace = with_replacement,
#'   model_based = FALSE,
#'   pop_size = population_size
#' )
#'
#' print(rss_estimates)
#' #>             Estimator point.est St.error 80% Confidence Interval
#' #> 1               RSS-1     9.153    0.766            8.148,10.158
#' #> 2  Aggregate Weighted     9.064    0.652             8.209,9.919
#'
rss_jps_estimate <- function(
    data, set_size, method, confidence = 0.95, replace = TRUE, model_based = FALSE, pop_size = NULL) {
  # Break confidence interval into two columns?
  # pop_size: nrow(data)*set_size <= pop_size, > 0, only relevant if replace = FALSE
  # method <- match.arg(toupper(method), c("JPS", "RSS"))
  verify_rss_jps_estimate_params(data, set_size, method, confidence, replace, model_based, pop_size)

  alpha <- 1 - confidence

  if (method == "JPS") {
    results <- jps_estimate(data, set_size, replace, model_based, pop_size, alpha)
  } else if (method == "RSS") {
    first_ranker_ranks <- data[, 2]
    rank_count <- aggregate(first_ranker_ranks, list(first_ranker_ranks), length)$x

    if (length(rank_count) != set_size || min(rank_count) <= 1) {
      stop("For RSS, ranks from the first ranker must have at least two observations in any ranking group.")
    }
    results <- rss_estimate(data, set_size, replace, model_based, pop_size, alpha)
  }

  if (model_based) {
    ci_col <- paste0(confidence * 100, "% Prediction intervals")
    colnames(results) <- c("Predictor", "Prediction", "Pred. Error", ci_col)
  }

  return(results)
}
