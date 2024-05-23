test_that("RSS has a correct output.", {
  skip_if(getRversion() < "3.4")
  matrix_ <- matrix(1:200, ncol = 2)

  rss_matrix_w_replacement <- rss_sample(matrix_, 100, 10, 2, TRUE)
  expect_equal(dim(rss_matrix_w_replacement), c(100, 3))
  expect_equal(sort(unique(rss_matrix_w_replacement[, 2])), 1:10)

  sample_counts_in_sets <- table(rss_matrix_w_replacement[, 2])
  expect_equal(sample_counts_in_sets[[2]], 10)
  expect_equal(table(sample_counts_in_sets)[[1]], 10)

  rss_matrix_wo_replacement <- rss_sample(matrix_, 10, 5, 2)
  expect_equal(dim(rss_matrix_wo_replacement), c(10, 3))
  expect_equal(sort(unique(rss_matrix_wo_replacement[, 2])), 1:5)

  sample_counts_in_sets <- table(rss_matrix_wo_replacement[, 2])
  expect_equal(sample_counts_in_sets[[2]], 2)
  expect_equal(table(sample_counts_in_sets)[[1]], 5)
})

test_that("Inputs are valid.", {
  expect_error(rss_sample(1:10, -100, 10, 1, "T"), "`replace` must be a boolean.")
})
