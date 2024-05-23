test_that("An SBS PPS sample works.", {
  load("../sbs_pps_input.RData")

  n_sbs <- 20
  n_pps <- 20
  sample_sizes <- c(n_sbs, n_pps)
  sample_matrix <- sbs_pps_sample(population, sample_sizes)$sample

  dimension <- dim(sample_matrix)
  expect_equal(dimension, c(40, 6))
  expect_true(all(sample_matrix$weight[1:n_sbs] == 0))

  # larger population to test parallelism
  sample_matrix <- sbs_pps_sample(rbind(population, population), sample_sizes)$sample

  dimension <- dim(sample_matrix)
  expect_equal(dimension, c(40, 6))
  expect_true(all(sample_matrix$weight[1:n_sbs] == 0))
})

test_that("An SBS PPS sample works for PPS only.", {
  load("../sbs_pps_input.RData")

  n_sbs <- 0
  n_pps <- 20
  sample_sizes <- c(n_sbs, n_pps)
  sample_matrix <- sbs_pps_sample(population, sample_sizes)$sample

  dimension <- dim(sample_matrix)
  expect_equal(dimension, c(20, 6))
})

test_that("An SBS PPS sample works for SBS only.", {
  load("../sbs_pps_input.RData")

  n_sbs <- 20
  n_pps <- 0
  sample_sizes <- c(n_sbs, n_pps)
  sample_matrix <- sbs_pps_sample(population, sample_sizes)$sample

  dimension <- dim(sample_matrix)
  expect_equal(dimension, c(20, 6))
  expect_true(all(sample_matrix$weight == 0))
})

test_that("population is a 2-dimension matrix with at least `sample size` rows and 4 columns.", {
  sample_sizes <- c(3, 3)

  population <- matrix(1:20, ncol = 4)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )

  population <- matrix(1:18, ncol = 3)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )

  population <- 1:24
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`population` must be a 2-dimension matrix-like object with at least 6 rows and 4 columns."
  )
})

test_that("Each sample sizes must be non-negative whole numbers.", {
  population <- matrix(1:48, ncol = 4)

  sample_sizes <- c(-1, 7)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`SBS sample size` must be a non-negative whole number."
  )

  sample_sizes <- c(7, -1)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`PPS sample size` must be a non-negative whole number."
  )

  sample_sizes <- list("0", 6)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`SBS sample size` must be a non-negative whole number."
  )

  sample_sizes <- list(FALSE, 6)
  expect_error(
    sbs_pps_sample(population, sample_sizes),
    "`SBS sample size` must be a non-negative whole number."
  )
})

test_that("The number of cores must be non-negative whole number.", {
  population <- matrix(1:48, ncol = 4)
  sample_sizes <- c(3, 3)
  invalid_n_cores <- list(-1, 0.1, NULL, NA, FALSE, "1")

  for (invalid in invalid_n_cores) {
    expect_error(
      sbs_pps_sample(population, sample_sizes, invalid),
      "`n_cores` must be a non-negative whole number."
    )
  }

  # expect_error(
  #   sbs_pps_sample(population, sample_sizes, -1),
  #   "`parallelize` must be a boolean."
  # )
  #
  # expect_error(
  #   sbs_pps_sample(population, sample_sizes, 0.1),
  #   "`parallelize` must be a boolean."
  # )
  #
  # expect_error(
  #   sbs_pps_sample(population, sample_sizes, 0.1),
  #   "`parallelize` must be a boolean."
  # )
  #
  # expect_error(
  #   sbs_pps_sample(population, sample_sizes, NULL),
  #   "`parallelize` must be a boolean."
  # )
  #
  # expect_error(
  #   sbs_pps_sample(population, sample_sizes, NA),
  #   "`parallelize` must be a boolean."
  # )
})
