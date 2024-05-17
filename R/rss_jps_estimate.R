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
#'
#' # Compute the JPS estimators
#'
#' jps_estimates <- rss_jps_estimate(
#'   data = emergence_ranks, set_size = 4,
#'   method = "JPS", confidence = 0.95,
#'   replace = TRUE, model_based = FALSE,
#'   pop_size = nrow(population)
#' )
#'
rss_jps_estimate <- function(
    data, set_size, method, confidence = 0.95, replace = TRUE, model_based = FALSE, pop_size = NULL) {
  # Choose the type of estimators to output?
  # Rename to estimate? Or something else?
  # Break confidence interval into two columns?
  # pop_size: nrow(data)*set_size <= pop_size, > 0, only relevant if replace = FALSE
  # method <- match.arg(toupper(method), c("JPS", "RSS"))
  verify_rss_jps_estimate_params(data, set_size, method, confidence, replace, model_based, pop_size)

  alpha <- 1 - confidence


  if (method == "JPS") {
    results <- jps_estimator(data, set_size, replace, model_based, pop_size, alpha)
  } else if (method == "RSS") {
    RV <- data[, 2]
    GSV <- aggregate(RV, list(RV), length)$x

    if (length(GSV) != set_size || min(GSV) <= 1) {
      stop("In ranked set sample, first ranking method should have at least two observations in any judgment ranking group")
    }

    results <- RSSEF(data, set_size, replace, model_based, pop_size, alpha)
  }

  if (model_based) {
    ci_col <- paste0(confidence * 100, "% Prediction intervals")
    colnames(results) <- c("Predictor", "Prediction", "Pred. Error", ci_col)
  }

  return(results)
}
