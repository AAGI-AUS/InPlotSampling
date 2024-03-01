test_that("RSSDF has a correct output.", {
  skip_if(getRversion() < 3.4)
  load("../population.rda")

  rss_matrix <- RSSDF(population, 100, 10, 2)
  expect_equal(dim(rss_matrix), c(100, 3))
  expect_equal(sort(unique(rss_matrix[, 2])), 1:10)

  sample_counts_in_sets <- table(rss_matrix[, 2])
  expect_equal(sample_counts_in_sets[[2]], 10)
  expect_equal(table(sample_counts_in_sets)[[1]], 10)

  rss_matrix_with_dropped_sample <- RSSDF(population, 100, 11, 2)
  expect_equal(sort(unique(rss_matrix_with_dropped_sample[, 2])), 0:11)

  sample_counts_in_sets <- table(rss_matrix_with_dropped_sample[, 2])
  expect_equal(sample_counts_in_sets[[2]], 9)
  expect_equal(table(sample_counts_in_sets)[[2]], 11)
})

test_that("Set size must be greater or equal to sample size.", {
  expect_error(RSSDF(1, 10, 11, 1), "`n` must >= `H`.")
})
