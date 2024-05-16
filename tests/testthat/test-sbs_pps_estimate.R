test_that("An SBS PPS estimator works.", {
  skip_if(getRversion() < 3.4)
  load("../sbs_pps_input.RData")
  load("../expected_sbs_pps_estimate.RData")

  set.seed(112)
  sample_sizes <- c(20, 20)
  sample_result <- sbs_pps_sample(population, sample_sizes)

  sample_matrix <- sample_result[[2]]
  sample_id <- sample_matrix[, 1]
  y_sample <- y[sample_id]

  estimated <- sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 100, 0.05)
  expect_identical(estimated, expected_sbs_pps_estimate)
})

test_that("An SBS PPS estimator works for PPS only.", {
  skip_if(getRversion() < 3.4)
  load("../sbs_pps_input.RData")
  load("../expected_pps_estimate.RData")

  set.seed(112)
  sample_sizes <- c(0, 20)
  sample_result <- sbs_pps_sample(population, sample_sizes)

  sample_matrix <- sample_result[[2]]
  sample_id <- sample_matrix[, 1]
  y_sample <- y[sample_id]

  estimated <- sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 100, 0.05)
  expect_identical(estimated, expected_pps_estimate)
})

test_that("An SBS PPS estimator works for SBS only.", {
  skip_if(getRversion() < 3.4)
  load("../sbs_pps_input.RData")
  load("../expected_sbs_estimate.RData")

  set.seed(112)
  sample_sizes <- c(20, 0)
  sample_result <- sbs_pps_sample(population, sample_sizes)

  sample_matrix <- sample_result[[2]]
  sample_id <- sample_matrix[, 1]
  y_sample <- y[sample_id]

  estimated <- sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 100, 0.05)
  expect_identical(estimated, expected_sbs_estimate)
})

test_that("population is a 2-dimension matrix with at least `sample size` rows and 4 columns.", {
  y_sample <- 1:6
  sample_matrix <- matrix(1:36, ncol = 6)
  sample_sizes <- c(3, 3)

  population <- matrix(1:20, ncol = 4)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )

  population <- matrix(1:18, ncol = 3)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )

  population <- 1:24
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )
})

test_that("Each sample sizes must be non-negative whole numbers.", {
  population <- matrix(1:48, ncol = 4)
  y_sample <- 1:6
  sample_matrix <- matrix(1:36, ncol = 6)

  sample_sizes <- c(-1, 7)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`SBS sample size` must be a non-negative whole number."
  )

  sample_sizes <- c(7, -1)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`PPS sample size` must be a non-negative whole number."
  )

  sample_sizes <- list("0", 6)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`SBS sample size` must be a non-negative whole number."
  )

  sample_sizes <- list(FALSE, 6)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`SBS sample size` must be a non-negative whole number."
  )
})

test_that("The number of `y` must be equal to the number of sample.", {
  population <- matrix(1:48, ncol = 4)
  sample_matrix <- matrix(1:36, ncol = 6)
  sample_sizes <- c(3, 3)

  y_sample <- 1:5
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "The number of `y` must be equal to the number of sample."
  )

  y_sample <- 1:7
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "The number of `y` must be equal to the number of sample."
  )

  y_sample <- 6
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "The number of `y` must be equal to the number of sample."
  )
})

test_that("sample matrix is a 2-dimension matrix with at least `sample size` rows and 6 columns.", {
  population <- matrix(1:48, ncol = 4)
  sample_sizes <- c(3, 3)
  y_sample <- 1:6

  sample_matrix <- matrix(1:30, ncol = 6)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`sample_matrix` must be a 2-dimension matrix-like object with at least 6 rows and 6 columns."
  )

  sample_matrix <- matrix(1:30, ncol = 5)
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`sample_matrix` must be a 2-dimension matrix-like object with at least 6 rows and 6 columns."
  )

  sample_matrix <- 1:36
  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix),
    "`sample_matrix` must be a 2-dimension matrix-like object with at least 6 rows and 6 columns."
  )
})

test_that("Numerical inputs are validated.", {
  population <- matrix(1:48, ncol = 4)
  sample_sizes <- c(3, 3)
  y_sample <- 1:6
  sample_matrix <- matrix(1:36, ncol = 6)

  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 0, 0.05),
    "`n_bootstraps` must be a positive whole number."
  )

  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, -1, 0.05),
    "`n_bootstraps` must be a positive whole number."
  )

  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 1.1, 0.05),
    "`n_bootstraps` must be a positive whole number."
  )

  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 100, -1),
    "`alpha` must be inclusively between 0 and 1."
  )

  expect_error(
    sbs_pps_estimate(population, sample_sizes, y_sample, sample_matrix, 100, 1.1),
    "`alpha` must be inclusively between 0 and 1."
  )
})
