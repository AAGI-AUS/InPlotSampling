pacman::p_load(parallel)

#' Calculate first order inclusion probability.
#'
#' @param i Index of the probability being calculated.
#' @param size_measurement Size measurements of population units.
#' @param n Sample sizes (SBS sample size, PPS sample size).
#' @param total_size Sum of size measurement.
#'
#' @return A vector of inclusion probabilities.
#'
calculate_first_order_prob <- function(i, size_measurement, n, total_size) {
  n_population <- length(size_measurement)
  n1 <- n[1]
  n2 <- n[2]

  if (n1 == 0) {
    prob_i <- 1 - (1 - size_measurement[i] / total_size)^n2
    return(prob_i)
  }

  if (n2 == 0) {
    prob_i <- n1 / n_population
    return(prob_i)
  }

  # sbs
  rearranged_index <- (i:(i + n_population - 1)) %% n_population
  sbs_prob <- n1 / n_population

  # pps
  indices_wo_i <- rearranged_index[-1]
  pps_prob <- 0
  for (j in (1:(n_population - n1))) {
    indices <- indices_wo_i[j:(j + n1 - 1)]
    indices_wo_zero <- indices * (indices != 0) + n_population * (indices == 0)
    wi <- size_measurement[i] / (total_size - sum(size_measurement[indices_wo_zero]))

    pps_prob <- pps_prob + (1 - (1 - wi)^n2) * 1 / n_population
  }
  prob_i <- sbs_prob + pps_prob
  return(prob_i)
}

#' Calculate inclusion probabilities.
#'
#' @param size_measurement Size measurements of population units.
#' @param n Sample sizes (SBS sample size, PPS sample size).
#'
#' @return A vector of inclusion probabilities.
#'
calculate_inclusion_prob <- function(size_measurement, n) {
  n_population <- length(size_measurement)
  total_size <- sum(size_measurement)

  # switch between a single core and multiple cores
  if (n_population > 250) {
    n_cores <- detectCores() - 1
    clusters <- makeCluster(n_cores)
    clusterExport(clusters, varlist = c("calculate_first_order_prob"))

    first_order_probability <- parSapply(clusters, (1:n_population), function(i) {
      calculate_first_order_prob(i, size_measurement, n, total_size)
    })

    stopCluster(clusters)
  } else {
    first_order_probability <- sapply((1:n_population), function(i) {
      calculate_first_order_prob(i, size_measurement, n, total_size)
    })
  }

  return(first_order_probability)
}
