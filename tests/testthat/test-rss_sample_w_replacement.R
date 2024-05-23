test_that("RSSDF has a correct output.", {
  skip_if(getRversion() < "3.4")
  matrix_ <- matrix(1:200, ncol = 2)

  rss_matrix <- rss_sample_w_replacement(matrix_, 100, 10, 2)
  expect_equal(dim(rss_matrix), c(100, 3))
  expect_equal(sort(unique(rss_matrix[, 2])), 1:10)

  sample_counts_in_sets <- table(rss_matrix[, 2])
  expect_equal(sample_counts_in_sets[[2]], 10)
  expect_equal(table(sample_counts_in_sets)[[1]], 10)

  # # # in case we are supporting `n %% H > 0`
  # rss_matrix_with_dropped_sample <- RSSDF(population, 100, 11, 2)
  # expect_equal(sort(unique(rss_matrix_with_dropped_sample[, 2])), 0:11)
  #
  # sample_counts_in_sets <- table(rss_matrix_with_dropped_sample[, 2])
  # expect_equal(sample_counts_in_sets[[2]], 9)
  # expect_equal(table(sample_counts_in_sets)[[2]], 11)
})

test_that("Inputs are valid.", {
  matrix_ <- matrix(1:1000, ncol = 2)

  expect_error(rss_sample_w_replacement(1:1000, 100, 10, 1), "`pop` must be a 2-dimension matrix-like object.")
  expect_error(rss_sample_w_replacement(matrix(1:1000), 100, 10, 1), "`pop` must have at least 2 columns.")
  expect_error(rss_sample_w_replacement(matrix_, -100, 10, 1), "`n` must be a positive whole number.")
  expect_error(rss_sample_w_replacement(matrix_, 100, -10, 1), "`H` must be a positive whole number.")
  expect_error(rss_sample_w_replacement(matrix_, 100, 10, -1), "`K` must be a positive whole number.")
  expect_error(rss_sample_w_replacement(matrix_, 1000, 10, 1), "`pop` must have at least `n` rows.")
  expect_error(rss_sample_w_replacement(matrix_, 5, 11, 1), "`n` must >= `H`.")
  expect_error(rss_sample_w_replacement(matrix_, 5, 4, 1), "`n` must be a multiple of `H`.")
})
