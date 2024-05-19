#' Generate probability proportional to size (PPS) and spatially balanced sampling on the population provided.
#'
#' @param population Population data frame to be sampled with 5 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurements of population units
#' @param n Sample sizes (SBS sample size, PPS sample size).
#' @param n_cores The number of cores to be used for computational tasks (specify 0 for max).
#'
#' @return A named list of:
#' - heatmap: heat map of the sample
#' - sample: SBS PPS sample of the population
#'
sbs_pps_sample <- function(population, n, n_cores = getOption("n_cores", 1)) {
  verify_non_negative_whole(n[1], n[2], n_cores, var_names = c("SBS sample size", "PPS sample size", "n_cores"))
  verify_matrix_like(population, n_dimensions = 2, n_rows = sum(n), n_cols = 4)

  sampled <- get_sbs_pps_sample_indices(population[, c(1, 4)], n)
  sbs_pps_indices <- sampled$sbs_pps_indices
  sbs_indices <- sampled$sbs_indices
  pps_indices <- sampled$pps_indices
  sizes_wo_sbs <- sampled$sizes_wo_sbs

  measured_sizes <- population[, 4]
  weights <- c(rep(0, n[1]), measured_sizes[pps_indices] / sum(sizes_wo_sbs))

  inclusion_probabilities <- calculate_inclusion_prob(population[, 4], n, n_cores)
  df_sample <- data.frame(
    sbs_pps_indices,
    x1 = population[sbs_pps_indices, 2],
    x2 = population[sbs_pps_indices, 3],
    size = population[sbs_pps_indices, 4],
    weight = weights,
    inclusion_probability = inclusion_probabilities[sbs_pps_indices]
  )

  heatmap_ <- sbs_pps_heatmap(population, sbs_indices, pps_indices)
  return(list(heatmap = heatmap_, sample = df_sample))
}

#' Generate SBS PPS sample indices.
#'
#' @param population Population data frame to be sampled with 2 columns.
#' 1. Halton numbers
#' 2. Size measurements of population units
#' @param n Sample sizes (SBS sample size, PPS sample size).
#'
#' @return A named list of:
#' - sbs_pps_indices: sample indices
#' - sbs_indices: sbs sample indices
#' - pps_indices: pps sample indices
#' - sizes_wo_sbs: measured sizes without sbs sample
#'
get_sbs_pps_sample_indices <- function(population, n, with_unique_pps = FALSE) {
  n_population <- dim(population)[1]
  n1 <- n[1]
  n2 <- n[2]
  measured_sizes <- population[, 2]
  indices <- population[, 1]

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
    pps_indices <- indices_wo_sbs[SDaA::lahiri.design(sizes_wo_sbs, n2)] + 1
    if (with_unique_pps) {
      pps_indices <- unique(pps_indices)
    }
  } else {
    pps_indices <- NULL
  }

  sbs_pps_indices <- c(sbs_indices, pps_indices)
  return(list(
    sbs_pps_indices = sbs_pps_indices,
    sbs_indices = sbs_indices,
    pps_indices = pps_indices,
    sizes_wo_sbs = sizes_wo_sbs
  ))
}
