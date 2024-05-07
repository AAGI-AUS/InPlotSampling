#' Compute empirical population by imputing reponses and measured sizes using knn.
#'
#' @param sample_indices Indices of sample.
#' @param pop Population data frame to be sampled with 4 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurement of population unit
#' @param y Sample response values.
#'
#' @return A summary data frame of the estimator.
#' @keywords internal
#'
get_empirical_population <- function(sample_indices, pop, y) {
  sample_rows <- pop[sample_indices, ]
  non_sample_rows <- pop[-sample_indices, -4]

  non_sample_coordinates <- non_sample_rows[, c(1, 2, 3)]
  sample_coordinates <- sample_rows[, -4]
  sample_measured_size <- sample_rows[, 4]

  sample_knn <- FNN::get.knnx(sample_coordinates[, -1], non_sample_coordinates[, -1], 4)
  knn_matrix <- cbind(sample_knn$nn.index, sample_knn$nn.dist)
  imputed_response <- apply(knn_matrix, 1, impute_w_knn, y)
  imputed_measured_size <- apply(knn_matrix, 1, impute_w_knn, sample_measured_size)

  # response
  sample_response <- cbind(sample_rows[, 1], y)
  colnames(sample_response) <- c("k", "imputed.responses")
  non_sample_imputed_res <- cbind(non_sample_rows[, 1], imputed_response)
  colnames(non_sample_imputed_res) <- c("k", "imputed.responses")

  empirical_response <- rbind(sample_response, non_sample_imputed_res)
  order_ <- order(empirical_response[, 1])
  empirical_response <- empirical_response[order_, ]

  # size measurement
  sample_measured_size_matrix <- sample_rows[, c(1, 4)]
  colnames(sample_measured_size_matrix) <- c("k", "imputed.Size")
  non_sample_imputed_size <- cbind(non_sample_rows[, 1], imputed_measured_size)
  colnames(non_sample_imputed_size) <- c("k", "imputed.Size")

  empirical_measured_size <- rbind(sample_measured_size_matrix, non_sample_imputed_size)
  order_ <- order(empirical_measured_size[, 1])
  empirical_measured_size <- empirical_measured_size[order_, ]

  empirical_population <- cbind(empirical_response, empirical_measured_size[, 2])
  return(empirical_population)
}

#' Calculate the mean of the nearest neighbors.
#'
#' @param knn_row A row of knn matrix composed of indices and distances.
#' @param y Sample response values.
#'
#' @return A summary data frame of the estimator.
#' @keywords internal
#'
impute_w_knn <- function(knn_row, y) {
  n_neighbors <- length(knn_row) / 2
  indices <- knn_row[1:n_neighbors]
  distances <- knn_row[(n_neighbors + 1):(2 * n_neighbors)]

  min_distance_indices <- indices[distances == min(distances)]
  mean_y <- mean(y[min_distance_indices])

  return(mean_y)
}
