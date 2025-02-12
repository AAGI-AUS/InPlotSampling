test_that("Two-stage cluster sample has a correct output.", {
  skip_if(getRversion() < "3.4")

  parent_size <- 20
  child_size <- 20

  parent_indices <- rep(1:parent_size, child_size)
  parent_aux <- 1:parent_size
  child_aux <- 1:(parent_size * child_size)
  population <- cbind(parent_indices, rep(parent_aux, child_size), child_aux)

  jps_jps_matrix <- two_stage_cluster_sample(population, c("jps", "jps"), 4, 3, FALSE, 6, 3, FALSE)
  expect_equal(dim(jps_jps_matrix), c(24, 5))
  expect_true(all(jps_jps_matrix[, 2] %in% 1:3))
  expect_true(all(jps_jps_matrix[, 5] %in% 1:3))

  srs_jps_matrix <- two_stage_cluster_sample(population, c("srs", "jps"), 4, 3, FALSE, 6, 3, FALSE)
  expect_equal(dim(srs_jps_matrix), c(24, 5))
  expect_equal(unique(srs_jps_matrix[, 2]), 0)

  jps_srs_matrix <- two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 6, 3, FALSE)
  expect_equal(dim(jps_srs_matrix), c(24, 5))
  expect_equal(unique(jps_srs_matrix[, 5]), 0)

  jps_srs_matrix <- two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, c(4, 3, 3, 3), 3, FALSE)
  expect_equal(dim(jps_srs_matrix), c(13, 5))

  jps_srs_matrix <- two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 6, c(4, 3, 3, 3), FALSE)
  expect_equal(dim(jps_srs_matrix), c(24, 5))
})

test_that("Inputs are valid.", {
  parent_size <- 20
  child_size <- 20

  parent_indices <- rep(1:parent_size, child_size)
  parent_aux <- 1:parent_size
  child_aux <- 1:(parent_size * child_size)
  population <- cbind(parent_indices, rep(parent_aux, child_size), child_aux)

  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 0, 3, FALSE, 6, 3, FALSE),
    "`n` must be a positive whole number."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 0, FALSE, 6, 3, FALSE),
    "`H` must be a positive whole number."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, "FALSE", 6, 3, FALSE),
    "`replace` must be a boolean."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, 0, 6, 3, FALSE),
    "`replace` must be a boolean."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 6, 3, "FALSE"),
    "`replace_i` must be a boolean."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 0, 3, FALSE),
    "`ni` must be a vector of positive whole numbers."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 6, 0, FALSE),
    "`Hi` must be a vector of positive whole numbers."
  )
  expect_error(
    two_stage_cluster_sample(population, "jp", 4, 3, FALSE, 6, 3, FALSE),
    "`sampling_strategies` must be a vector of 2 values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs", "srs"), 4, 3, FALSE, 6, 3, FALSE),
    "`sampling_strategies` must be a vector of 2 values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "sr"), 4, 3, FALSE, 6, 3, FALSE),
    "`sampling_strategies` must be a vector of `'srs'` and/or `'jps'`."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 2, 3, FALSE, 6, 3, FALSE),
    "`n` must be at least `H`."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, rep(4, 3), 3, FALSE),
    "`ni` must be a vector of 1 or `n` values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, rep(4, 5), 3, FALSE),
    "`ni` must be a vector of 1 or `n` values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 4, rep(3, 5), FALSE),
    "`Hi` must be a vector of 1 or `n` values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 4, rep(3, 3), FALSE),
    "`Hi` must be a vector of 1 or `n` values."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 4, 3, FALSE, 2, 3, FALSE),
    "ith value of `ni` must be at least ith value of `Hi`."
  )
  expect_error(
    two_stage_cluster_sample(population, c("jps", "srs"), 8, 3, FALSE, 6, 3, FALSE),
    "The number of population must be at least `nH`."
  )
})
