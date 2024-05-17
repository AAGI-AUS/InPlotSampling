#' Computes the estimator for JPS data
#'
#' @param data The data to use for estimation.
#' @param set_size Set size for each raking group.
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model_based An inference mode:
#' - `FALSE`: design based inference
#' - `TRUE`: model based inference using super population model
#' @param N The population size.
#' @param alpha The significance level.
#'
#' @importFrom stats aggregate qt rnorm sd var
#'
#' @return A `data.frame` with the point estimates provided by JPS estimators along with standard error and
#'   confidence intervals.
#' @export
#'
jps_estimator <- function(data, set_size, replace = TRUE, model_based, N, alpha) {
  n_rankers <- ncol(data) - 1

  if (!replace && is.null(N)) {
    stop("Population size N must be provided for sampling without replacement")
  }

  n <- nrow(data)
  coefn <- calculate_coefficients(set_size, n)
  if (n_rankers == 1) {
    coef_del1 <- NULL
  } else {
    coef_del1 <- calculate_coefficients(set_size, n - 1)
  }
  if (!replace) {
    coef1d2 <- coefn[1]
    coef2d2 <- (1 / (set_size * (set_size - 1)) + coefn[3] + coefn[2]
      - (coefn[2] + 1 / set_size^2) * set_size / (set_size - 1))
    coef3d2 <- coefn[2] - 1 / (N - 1) * (1 / set_size - (coefn[1] + 1 / set_size^2))
    coefd <- c(coef1d2, coef2d2, coef3d2)
    coef1d2_del <- coef_del1[1]
    coef2d2_del <- (1 / (set_size * (set_size - 1)) + coef_del1[3] + coef_del1[2]
      - (coef_del1[2] + 1 / set_size^2) * set_size / (set_size - 1))
    coef3d2_del <- coef_del1[2] - 1 / (N - 1) * (1 / set_size - (coef_del1[1] + 1 / set_size^2))
    coef_del <- c(coef1d2_del, coef2d2_del, coef3d2_del)

    fc <- 1 - n / N # finite population correction factor
  } else {
    coefd <- coefn
    coef_del <- coef_del1

    fc <- 1 # finite population correction factor
  }

  if (model_based == 1) {
    coefd <- coefn
    coef_del <- coef_del1
  }

  ranks <- data[, -1]
  y <- data[, 1]
  if (n_rankers == 1) {
    jps_estimates <- jps_estimator_single(
      ranks, y, set_size, N, coefd, coef_del, replace, model_based, n_rankers
    )
    estimators <- c("JPS", "SRS")
    means <- round(c(jps_estimates[1], mean(y)), digits = 3)
    # TODO: check std error calculation of jps variance
    std_errors <- round(c(sqrt(jps_estimates[2]), sd(y) / sqrt(n)), digits = 3)

    lower_bound <- round(means - qt(1 - alpha / 2, n - 1) * std_errors, digits = 3)
    upper_bound <- round(means + qt(1 - alpha / 2, n - 1) * std_errors, digits = 3)
    confidence_interval <- paste(lower_bound, upper_bound, sep = ",")
    jps_summary <- data.frame(estimators, means, std_errors, confidence_interval, stringsAsFactors = FALSE)

    ci_col <- paste0((1 - alpha) * 100, "% Confidence intervals")
    colnames(jps_summary) <- c("Estimator", "Estimate", "Standard Error", ci_col)

    return(jps_summary)
  }

  # more than one ranker
  jps_estimates <- apply(
    ranks,
    2,
    jps_estimator_single,
    y = y,
    set_size = set_size,
    N = N,
    coef = coefd,
    coef_del = coef_del,
    replace = replace,
    model_based = model_based,
    n_rankers
  )

  # estimates using all data
  jps_mean_n <- jps_estimates[1, ]
  jps_variance_n <- jps_estimates[2, ]

  # estimates with i-th observation deleted where the i-th row corresponds to the estimates when the i-th
  # observation is deleted
  jps_var_ith_deleted <- jps_estimates[c(3:(n + 2)), ]
  jps_mean_ith_deleted <- jps_estimates[-c(1:(n + 2)), ]
  # if (replace) fc <- 1 else fc <- 1 - n / N # finite population correction factor

  # variance weighted
  jps_var_weighted_mean <- sum((1 / jps_variance_n) * jps_mean_n) / sum(1 / jps_variance_n)
  variance_weights <- t(apply(jps_var_ith_deleted, 1, function(u) {
    (1 / u) / sum(1 / u)
  }))
  jackknife_var_weighted_mean <- diag(variance_weights %*% t(jps_mean_ith_deleted))
  jackknife_var_weighted_var <- fc * (n - 1) * var(jackknife_var_weighted_mean) * ((n - 1) / n)^2

  mean_best_ranker <- jps_mean_n[1]
  variance_best_ranker <- jps_variance_n[1]

  # equally weighted
  jps_mean <- mean(jps_mean_n)
  jackknife_mean <- apply(jps_mean_ith_deleted, 1, mean)
  jackknife_variance <- fc * (n - 1) * var(jackknife_mean) * ((n - 1) / n)^2

  # agreement weighted
  agreement_weights <- t(apply(data.frame(ranks), 1, calculate_agreement_weights, set_size = set_size))
  aw_sum <- apply(agreement_weights, 2, sum)
  cross_product <- y %*% agreement_weights
  agreement_mean <- mean(cross_product[aw_sum > 0] / aw_sum[aw_sum > 0])
  awy <- cbind(y, agreement_weights)
  jackknife_agreement_mean <- apply(matrix(1:n, ncol = 1), 1, FWDel1, AWY = awy)
  jackknife_agreement_var <- fc * (n - 1) * var(jackknife_agreement_mean) * ((n - 1) / n)^2

  estimated_means <- c(jps_mean, jps_var_weighted_mean, agreement_mean, mean_best_ranker, mean(y))
  estimated_variances <- c(
    jackknife_variance,
    jackknife_var_weighted_var,
    jackknife_agreement_var,
    variance_best_ranker,
    var(y) / n
  )

  min_variance_index <- which(estimated_variances == min(estimated_variances))
  estimated_variances <- c(estimated_variances, estimated_variances[min_variance_index])
  std_errors <- sqrt(estimated_variances)

  estimated_means <- c(estimated_means, estimated_means[min_variance_index])
  lower_bounds <- round(estimated_means - qt(1 - alpha / 2, n - 1) * sqrt(estimated_variances), digits = 3)
  upper_bounds <- round(estimated_means + qt(1 - alpha / 2, n - 1) * sqrt(estimated_variances), digits = 3)
  confidence_intervals <- paste(lower_bounds, upper_bounds, sep = ",")

  estimators <- c("UnWeighted", "Sd.Weighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum")
  ci_col <- paste0((1 - alpha) * 100, "% Confidence intervals")
  estimator_summary <- data.frame(estimators,
    round(estimated_means, digits = 3),
    round(std_errors, digits = 3),
    confidence_intervals,
    stringsAsFactors = FALSE
  )
  colnames(estimator_summary) <- c("Estimator", "Estimate", "Standard Error", ci_col)

  return(estimator_summary)
}
