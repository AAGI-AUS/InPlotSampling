#' Calculate the estimate of the rankings
#'
#' @param data A data frame of `JPS` or `RSS` rankings.
#' @param set_size The set size of the ranks.
#' @param method A method used to sample:
#' - `"JPS"`: Judgment-post stratified sampling
#' - `"RSS"`: Ranked set sampling
#' @param confidence The confidence level to use.
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model An inference mode:
#' - `0`: design based inference
#' - `1`: model based inference using super population model
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
#' JPS.Estimates <- OneSample(
#'   data = emergence_ranks, set_size = 4,
#'   method = "JPS", confidence = 0.95,
#'   replace = TRUE, model = 0,
#'   pop_size = nrow(population)
#' )
#'
OneSample <- function(data, set_size, method, confidence = 0.95, replace = TRUE, model = 0, pop_size = NULL) {
  # Choose the type of estimators to output?
  # Rename to estimate? Or something else?
  # Break confidence interval into two columns?
  # If model is 0, it's design based inference, if model = 1, it is model based inference using super population model
  # pop_size: nrow(data)*set_size <= pop_size, > 0, only relevant if replace = FALSE
  # method <- match.arg(toupper(method), c("JPS", "RSS"))
  verify_one_sample_params(data, set_size, method, confidence, replace, model, pop_size)

  # if (set_size < 1 || is.na(set_size) || is.null(set_size) || !is.numeric(set_size)) {
  #   stop("set_size must be a positive numeric value")
  # }
  #
  # if (!isTRUE(replace) && !isFALSE(replace)) {
  #   stop("replace must be TRUE or FALSE")
  # }
  #
  #
  # if (confidence > 1 || confidence < 0 || !is.numeric(confidence)) {
  #   stop("confidence must take a numeric value between 0 and 1, indicating the confidence level")
  # }
  #
  # if (!model %in% c(1, 0)) {
  #   stop("model must be 0 for design based inference or 1 for super-population model")
  # }
  #
  #
  # if (!replace) {
  #   if (missing(pop_size) || is.null(pop_size) || !is.numeric(pop_size)) {
  #     stop("A numeric population size pop_size must be provided when sampling without replacement")
  #   } else if (pop_size <= nrow(data) * set_size || pop_size <= 0) {
  #     stop("pop_size must be positive and less than data x set_size")
  #   }
  # }
  # if (model == 1 && missing(pop_size)) {
  #   stop("The population size pop_size must be provided for super-population model")
  # }
  alpha <- 1 - confidence

  #################################################################
  ### Judgment-post stratified sample #############################
  #################################################################

  if (method == "JPS") {
    results <- JPSEF(data, set_size, replace, model, pop_size, alpha)
  }

  #################################################################
  ### Ranked set sample ###########################################
  #################################################################

  else if (method == "RSS") {
    RV <- data[, 2]
    GSV <- aggregate(RV, list(RV), length)$x

    if (length(GSV) != set_size || min(GSV) <= 1) {
      stop("In ranked set sample, first ranking method should have at least two observations in any judgment ranking group")
    }

    results <- RSSEF(data, set_size, replace, model, pop_size, alpha)
  }

  if (model == 1) {
    colnames(results) <- c("Predictor", "Prediction", "Pred. Error", paste0(confidence * 100, "% Prediction intervals"))
  }

  return(results)
}

#' @rdname OneSample
one_sample <- OneSample
