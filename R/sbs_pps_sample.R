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
  n1 <- n[1]
  n2 <- n[2]
  sizes <- pop[, 4]
  halton_numbers <- pop[, 1]
  n_population <- length(sizes)

  # sbs
  if (n1 > 0) {
    sbs_start_index <- sample(halton_numbers, 1)
    sbs_index <- (sbs_start_index:(sbs_start_index + n1 - 1)) %% n_population + 1
    sizes_wo_sbs <- sizes[-sbs_index]
    index_wo_sbs <- halton_numbers[-sbs_index]
  } else {
    sbs_index <- NULL
    sizes_wo_sbs <- sizes
    index_wo_sbs <- halton_numbers
  }

  # pps
  if (n2 > 0) {
    pps_index <- index_wo_sbs[lahiri.design(sizes_wo_sbs, n2)] + 1
  } else {
    pps_index <- NULL
  }

  sbs_pps_index <- c(sbs_index, pps_index)
  weights <- c(rep(0, n1), sizes[pps_index] / sum(sizes_wo_sbs))

  inclusion_probabilities <- calculate_inclusion_prob(pop[, 4], n)
  df_sample <- data.frame(
    sbs_pps_index,
    x1 = pop[sbs_pps_index, 2],
    x2 = pop[sbs_pps_index, 3],
    size = pop[sbs_pps_index, 4],
    weight = weights,
    inclusion_probability = inclusion_probabilities[sbs_pps_index]
  )

  heat_map <- GGplotF(pop, df_sample)

  return(list(heat_map = heat_map, pps_sbs_sample = df_sample))
}
