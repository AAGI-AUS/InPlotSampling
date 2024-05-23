test_that("RSSDF has a correct output.", {
  skip_if(getRversion() < "3.4")
  matrix_ <- matrix(1:200, ncol = 2)

  rss_matrix <- rss_sample_wo_replacement(matrix_, 50, 2, 2)
  expect_equal(dim(rss_matrix), c(50, 3))
  expect_equal(sort(unique(rss_matrix[, 2])), 1:2)

  sample_counts_in_sets <- table(rss_matrix[, 2])
  expect_equal(sample_counts_in_sets[[2]], 25)
  expect_equal(table(sample_counts_in_sets)[[1]], 2)
})

test_that("Inputs are valid.", {
  matrix_ <- matrix(1:50, ncol = 2)

  expect_error(rss_sample_wo_replacement(1:10, 10, 2, 1), "`pop` must be a 2-dimension matrix-like object.")
  expect_error(rss_sample_wo_replacement(matrix(1:10), 2, 10, 1), "`pop` must have at least 2 columns.")
  expect_error(rss_sample_wo_replacement(matrix_, -10, 2, 1), "`n` must be a positive whole number.")
  expect_error(rss_sample_wo_replacement(matrix_, 10, -2, 1), "`H` must be a positive whole number.")
  expect_error(rss_sample_wo_replacement(matrix_, 10, 2, -1), "`K` must be a positive whole number.")
  expect_error(rss_sample_wo_replacement(matrix_, 5, 11, 1), "`n` must >= `H`.")
  expect_error(rss_sample_wo_replacement(matrix_, 5, 4, 1), "`n` must be a multiple of `H`.")
  expect_error(rss_sample_wo_replacement(matrix_, 20, 5, 1), "The number of population must be at least `nH`.")
})
