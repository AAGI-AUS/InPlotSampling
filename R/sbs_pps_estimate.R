#' Compute an estimator for SBS PPS sampled data.
#'
#' @param population Population data frame to be sampled with 4 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurement of population unit
#' @param n Sample sizes (SBS sample size, PPS sample size).
#' @param y Sample response values.
#' @param sample_matrix Sample data frame to be sampled with 6 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurement of population unit
#' 5. Weight
#' 6. Inclusion probability
#' @param n_bootstraps Number of bootstrap samples.
#' @param alpha The significance level.
#'
#' @return A summary data frame of the estimator.
#'
sbs_pps_estimate <- function(population, n, y, sample_matrix, n_bootstraps = 100, alpha = 0.05) {
  verify_sbs_pps_estimate_params(population, n, y, sample_matrix, n_bootstraps, alpha)

  n_population <- dim(population)[1]

  if (n[1] != 0) {
    is_duplicated <- duplicated(sample_matrix[, 1])
    sample_matrix <- sample_matrix[!is_duplicated, ]
    y <- y[!is_duplicated]

    estimated_mean <- round(sum(y / sample_matrix[, 6]) / n_population, digit = 3)

    empirical_population <- get_empirical_population(sample_matrix[, 1], population, y)
    empirical_inclusion_prob <- calculate_inclusion_prob(empirical_population[, 3], n)
    empirical_population <- data.frame(empirical_population, empirical_inclusion_prob)

    estimated_variance <- round(bootstrap_sample(empirical_population, n, n_bootstraps), digits = 3)
  } else {
    # pps only
    y_hat <- y / sample_matrix[, 5]
    y_hat_mean <- mean(y_hat)
    n_samples <- dim(sample_matrix)[1]

    estimated_mean <- y_hat_mean / n_population
    estimated_variance <- sum((y_hat - y_hat_mean)^2) / (n_samples - 1) / n_samples / n_population^2
  }

  # confidence interval
  n_samples <- dim(sample_matrix)[1]
  std_error <- sqrt(estimated_variance)
  qt_term <- qt(1 - alpha / 2, n_samples - 1)

  lower_bound <- round(estimated_mean - qt_term * std_error, digits = 3)
  upper_bound <- round(estimated_mean + qt_term * std_error, digits = 3)
  confidence_interval <- paste(lower_bound, upper_bound, sep = ",")

  ci_col <- paste0((1 - alpha) * 100, "% Confidence intervals")
  df_summary <- data.frame(n[1], n[2], estimated_mean, std_error, confidence_interval)
  colnames(df_summary) <- c("n1", "n2", "Estimate", "St.error", ci_col)
  rownames(df_summary) <- NULL

  return(df_summary)
}

verify_sbs_pps_estimate_params <- function(population, n, y, sample_matrix, n_bootstraps, alpha) {
  verify_non_negative_whole(n[1], n[2], var_names = c("SBS sample size", "PPS sample size"))
  verify_positive_whole_number(n_bootstraps)
  verify_between(alpha, lower = 0, upper = 1)

  sample_size <- sum(n)
  verify_matrix_like(population, n_dimensions = 2, n_rows = sample_size, n_cols = 4)
  verify_matrix_like(sample_matrix, n_dimensions = 2, n_rows = sample_size, n_cols = 6)

  if (sample_size != length(y)) {
    stop("The number of `y` must be equal to the number of sample.")
  }

  minimum_size <- min(population[, 4])
  verify_between(minimum_size, lower = 0, lower_exclude = TRUE)
}
