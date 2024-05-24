#' Generate boostrap sample on the provided population.
#'
#' @param pop Population data
#' @param n Sample sizes (SBS sample size, PPS sample size).
#' @param n_bootstraps Number of bootstrap samples.
#'
#' @return A summary data frame of the estimator.
#'
bootstrap_sample <- function(pop, n, n_bootstraps) {
  n_population <- dim(pop)[1]
  bootstrap_estimates <- matrix(0, ncol = 1, nrow = n_bootstraps)
  colnames(bootstrap_estimates) <- c("SBS1-PPS1")
  for (d in (1:n_bootstraps)) {
    sampled <- single_bootstrap(pop, n)

    y <- sampled[, 2]
    inclusion_probabilities <- sampled[, 3]
    estimated_mean <- sum(y / inclusion_probabilities) / n_population
    bootstrap_estimates[d, ] <- estimated_mean
  }

  estimated_variance <- apply(bootstrap_estimates, 2, var)
  return(estimated_variance)
}

#' Generate a single boostrap sample on the provided population.
#'
#' @param pop Population data
#' @param n Sample sizes (SBS sample size, PPS sample size).
#'
#' @return A summary data frame of the estimator.
#'
single_bootstrap <- function(pop, n) {
  # TODO: check why unique is needed
  sbs_pps_indices <- get_sbs_pps_sample_indices(pop[, c(1, 3)], n, with_unique_pps = TRUE)$sbs_pps_indices
  sbs_pps_sample <- data.frame(
    sbs_pps_indices,
    y = pop[sbs_pps_indices, 2],
    inclusion_probability = pop[sbs_pps_indices, 4]
  )

  return(sbs_pps_sample)
}
