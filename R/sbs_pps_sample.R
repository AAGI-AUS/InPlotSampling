# TODO: update docstring
#' Generate probability proportional to size (PPS) and spatially balanced sampling on the population provided.
#'
#' @param pop Population data frame to be sampled with 5 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurements of population units
#' 5. Inclusion probabilities
#' @param n Sample sizes (SBS sample size, PPS sample size).
#'
#' @return A matrix with ranks from each ranker.
#'
sbs_pps_sample <- function(pop, n) {
  sampled <- get_sbs_pps_sample_indices(pop[, c(1, 4)], n)
  sbs_pps_indices <- sampled$sbs_pps_indices
  pps_indices <- sampled$pps_indices
  sizes_wo_sbs <- sampled$sizes_wo_sbs

  measured_sizes <- pop[, 4]
  weights <- c(rep(0, n[1]), measured_sizes[pps_indices] / sum(sizes_wo_sbs))

  inclusion_probabilities <- calculate_inclusion_prob(pop[, 4], n)
  df_sample <- data.frame(
    sbs_pps_indices,
    x1 = pop[sbs_pps_indices, 2],
    x2 = pop[sbs_pps_indices, 3],
    size = pop[sbs_pps_indices, 4],
    weight = weights,
    inclusion_probability = inclusion_probabilities[sbs_pps_indices]
  )

  heat_map <- GGplotF(pop, df_sample)

  return(list(heat_map = heat_map, sbs_pps_sample = df_sample))
}

#' Generate SBS PPS sample indices.
#'
#' @param pop Population data frame to be sampled with 2 columns.
#' 1. Halton numbers
#' 2. Size measurements of population units
#' @param n Sample sizes (SBS sample size, PPS sample size).
#'
#' @return A matrix with ranks from each ranker.
#'
get_sbs_pps_sample_indices <- function(pop, n, with_unique_pps = FALSE) {
  n_population <- dim(pop)[1]
  n1 <- n[1]
  n2 <- n[2]
  measured_sizes <- pop[, 2]
  indices <- pop[, 1]

  # sbs
  if (n1 > 0) {
    sbs_start_index <- sample(indices, 1)
    sbs_indices <- (sbs_start_index:(sbs_start_index + n1 - 1)) %% n_population + 1
    sizes_wo_sbs <- measured_sizes[-sbs_indices]
    indices_wo_sbs <- indices[-sbs_indices]
  } else {
    sbs_indices <- NULL
    sizes_wo_sbs <- measured_sizes
    indices_wo_sbs <- indices
  }

  # pps
  if (n2 > 0) {
    pps_indices <- indices_wo_sbs[lahiri.design(sizes_wo_sbs, n2)] + 1
    if (with_unique_pps) {
      pps_indices <- unique(pps_indices)
    }
  } else {
    pps_indices <- NULL
  }

  sbs_pps_indices <- c(sbs_indices, pps_indices)
  return_values <- list(sbs_pps_indices, pps_indices, sizes_wo_sbs)
  names(return_values) <- c("sbs_pps_indices", "pps_indices", "sizes_wo_sbs")

  return(return_values)
}
