#' This function provides an estimator for RSS data
#'
#' @param data A matrix with a response variable in the first column and ranks in the following columns.
#' @param set_size Set size for each ranking group.
#' @param replace A boolean which specifies whether to sample with replacement or not.
#' @param model_based An inference mode:
#' - `FALSE`: design based inference
#' - `TRUE`: model based inference using super population model
#' @param pop_size The population size. Must be provided if sampling without replacement, or if `model` is `1`.
#' @param alpha A significance level.
#'
#' @return
#' @keywords internal
#'
rss_estimate <- function(data, set_size, replace, model_based, pop_size, alpha) {
  first_ranker_ranks <- data[, 2]
  y <- data[, 1]
  n <- nrow(data)
  n_rankers <- ncol(data) - 1

  if (!replace) {
    rss_estimated <- rss_estimate_single(first_ranker_ranks, y, set_size, pop_size)
    rss_mean <- rss_estimated[1]
    rss_variance <- rss_estimated[2]
  } else {
    rss_mean <- mean(aggregate(y, list(first_ranker_ranks), FUN = mean)$x)

    rank_count <- aggregate(first_ranker_ranks, list(first_ranker_ranks), FUN = length)$x
    rss_variances <- aggregate(y, list(first_ranker_ranks), FUN = var)$x[rank_count > 1]
    rss_variance <- sum(rss_variances / rank_count[rank_count > 1]) / length(rss_variances)^2
  }

  if (model_based) {
    rss_estimated <- rss_estimate_single(first_ranker_ranks, y, set_size, pop_size)
    rss_mean <- rss_estimated[1]
    rss_variance <- rss_estimated[2]
  }

  ci_col <- paste0((1 - alpha) * 100, "% Confidence Interval")
  std_error <- sqrt(rss_variance)
  interval <- get_interval(rss_mean, n, std_error, alpha, round_digits = 3)
  lower_bound <- interval$lower_bound
  upper_bound <- interval$upper_bound

  if (n_rankers == 1) {
    ci <- paste(lower_bound, upper_bound, sep = ",")
    estimator_name <- c("RSS-1")
    summary.table <- data.frame(
      Estimator = estimator_name,
      point.est = round(rss_mean, digits = 3),
      St.error = round(std_error, digits = 3),
      ci,
      stringsAsFactors = FALSE
    )
    colnames(summary.table)[4] <- ci_col

    return(summary.table)
  }

  if (replace) {
    fc <- 1
  } else {
    fc <- 1 - n / (pop_size - 1)
  }
  jackknife_estimated <- jackknife_agreement_estimate(data[, -1], y, set_size, pop_size, fc)
  agreement_mean <- jackknife_estimated$agreement_mean
  jackknife_var <- jackknife_estimated$jackknife_var

  jackknife_std_error <- sqrt(jackknife_var)
  jackknife_interval <- get_interval(agreement_mean, n, jackknife_std_error, alpha, round_digits = 3)
  jackknife_lower_bound <- jackknife_interval$lower_bound
  jackknife_upper_bound <- jackknife_interval$upper_bound

  lower_bounds <- c(lower_bound, jackknife_lower_bound)
  upper_bounds <- c(upper_bound, jackknife_upper_bound)
  std_errors <- c(std_error, jackknife_std_error)
  means <- c(rss_mean, agreement_mean)
  cis <- paste(lower_bounds, upper_bounds, sep = ",")
  estimator_names <- c("RSS-1", " Aggregate Weighted")

  summary.table <- data.frame(
    Estimator = estimator_names,
    point.est = round(means, digits = 3),
    St.error = round(std_errors, digits = 3),
    cis,
    stringsAsFactors = F
  )
  colnames(summary.table)[4] <- ci_col

  return(summary.table)
}
