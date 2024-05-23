test_that("JPS sample has a correct output.", {
  skip_if(getRversion() < "3.4")
  population <- 1:100
  k <- 3
  tau <- rep(1.2, 3)

  jps_matrix <- jps_sample(population, 30, 2, tau, k)
  expect_equal(dim(jps_matrix), c(30, 4))
  expect_equal(sort(unique(jps_matrix[, 2])), 1:2)
})

test_that("Inputs are valid.", {
  population <- 1:100
  k <- 3
  tau <- rep(1.2, 3)

  expect_error(jps_sample(population, -30, 2, tau, k), "`n` must be a positive whole number.")
  expect_error(jps_sample(population, 30, -2, tau, k), "`H` must be a positive whole number.")
  expect_error(jps_sample(population, 30, 2, tau, -k), "`K` must be a positive whole number.")
  expect_error(jps_sample(population, 30, 2, tau, 4), "The length of `tau` must equal to `K`.")
  expect_error(jps_sample(population, 50, 4, tau, k), "The number of population must be at least `nH`.")
  expect_error(jps_sample(population, 10, 20, tau, k), "`n` must >= `H`.")
  expect_error(jps_sample(population, 30, 2, tau, k, "T"), "`with_replacement` must be a boolean.")
})
