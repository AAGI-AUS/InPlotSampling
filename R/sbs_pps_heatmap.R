#' Generate a heat map of SBS PPS sample of the provided population.
#'
#' @param pop Population data frame to be sampled with 5 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurements of population units
#' 5. Inclusion probabilities
#' @param sbs_indices Indices of SBS sample.
#' @param pps_indices Indices of PPS sample.
#'
#' @return Heat map of the sample.
#'
sbs_pps_heatmap <- function(pop, sbs_indices, pps_indices) {
  n_population <- dim(pop)[1]

  sbs_or_pps <- rep(0, n_population)
  sbs_or_pps[(sbs_indices)] <- "SBS"
  sbs_or_pps[(pps_indices)] <- "PPS"

  x1 <- pop[, 2]
  x2 <- pop[, 3]
  measured_sizes <- pop[, 4]

  heatmap_data <- data.frame(measured_sizes, x1, x2, sbs_or_pps)
  sbs_pps_data <- dplyr::filter(heatmap_data, sbs_or_pps != 0)
  heatmap_ <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x2, x1, fill = measured_sizes)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_distiller(palette = "RdPu") +
    ggplot2::geom_point(
      sbs_pps_data,
      mapping = ggplot2::aes(shape = factor(sbs_or_pps)),
      size = 2
    ) +
    ggplot2::labs(
      x = "X coordinate",
      y = "Y coordinate",
      shape = "Sample type",
      fill = "Size"
    )

  return(heatmap_)
}
