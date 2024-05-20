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
#' @export

#' @examples
#' set.seed(112)
#'
#' # SBS sample size, PPS sample size
#' sample_sizes <- c(5, 5)
#'
#' n_population <- 233
#' k <- 0:(n_population - 1)
#' x1 <- sample(1:13, n_population, replace = TRUE) / 13
#' x2 <- sample(1:8, n_population, replace = TRUE) / 8
#' y <- (x1 + x2) * runif(n = n_population, min = 1, max = 2) + 1
#' measured_sizes <- y * runif(n = n_population, min = 0, max = 4)
#'
#' population <- matrix(cbind(k, x1, x2, measured_sizes), ncol = 4)
#' sample_result <- sbs_pps_sample(population, sample_sizes)
#' print(sample_result$sample)
#' #>    sbs_pps_indices        x1    x2       size      weight inclusion_probability
#' #> 1               87 0.4615385 0.625  0.4665423 0.000000000            0.02319163
#' #> 2               88 0.1538462 0.625  1.7389902 0.000000000            0.02790409
#' #> 3               89 0.8461538 0.625  7.0815547 0.000000000            0.04749104
#' #> 4               90 0.6923077 0.750  9.5428032 0.000000000            0.05640733
#' #> 5               91 0.2307692 0.750  5.1375136 0.000000000            0.04039996
#' #> 6              173 0.1538462 0.500  6.6400168 0.005024130            0.04588620
#' #> 7               26 0.6153846 0.500  4.3146186 0.003264631            0.03738898
#' #> 8              232 0.8461538 0.750 12.0057856 0.009084108            0.06526583
#' #> 9              171 0.6153846 0.750  6.9029083 0.005223046            0.04684225
#' #> 10              29 0.8461538 0.375  4.6324720 0.003505133            0.03855377
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
