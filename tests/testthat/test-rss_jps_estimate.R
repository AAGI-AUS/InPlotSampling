test_that("RSS JPS estimate works with JPS", {
  skip_if(getRversion() < "3.4")
  load(test_path("data", "jps_data.Rdata"))
  expect_identical(rss_jps_estimate(Data, 3, "JPS", 0.95, FALSE, FALSE, 600), saved_jps_output)
})

test_that("RSS JPS estimate works with RSS", {
  skip_if(getRversion() < "3.4")
  load(test_path("data", "rss_data.Rdata"))
  expect_identical(rss_jps_estimate(Data, 3, "RSS", 0.95, FALSE, FALSE, 600), saved_rss_output)
})

test_that("set_size is positive", {
  expect_error(
    rss_jps_estimate(emergence_ranks, -4, "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, "4", "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, NA, "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
})

test_that("replace is TRUE or FALSE", {
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "JPS", replace = NA, pop_size = 2640),
    "`replace` must be a boolean."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "RSS", replace = "NA", pop_size = 2640),
    "`replace` must be a boolean."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "RSS", replace = 0, pop_size = 2640),
    "`replace` must be a boolean."
  )
})

test_that("pop_size is supplied when needed", {
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, pop_size = NA),
    "A numeric population size `pop_size` must be provided when sampling without replacement"
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, pop_size = "A"),
    "A numeric population size `pop_size` must be provided when sampling without replacement"
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "JPS", replace = T, model_based = TRUE),
    "The population size `pop_size` must be provided for super-population model"
  )
})

test_that("method is correct", {
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "ABC", replace = T),
    '`method` must be `"JPS"` or `"RSS"`.'
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = 1, replace = T),
    '`method` must be `"JPS"` or `"RSS"`.'
  )
})

test_that("confidence is between 0 and 1", {
  expect_error(rss_jps_estimate(emergence_ranks, 4, "JPS", replace = T, confidence = 0), NA)
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "JPS", replace = T, confidence = 5),
    "`confidence` must be inclusively between 0 and 1."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "JPS", replace = T, confidence = "A"),
    "`confidence` must be inclusively between 0 and 1."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, "JPS", replace = T, confidence = NA),
    "`confidence` must be inclusively between 0 and 1."
  )
})

test_that("model is 0 or 1", {
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", model_based = FALSE),
    NA
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", model_based = "A"),
    "`model_based` must be a boolean."
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", model_based = NA),
    "`model_based` must be a boolean."
  )
})

test_that("pop_size is > 0 and >= set_size*nrow(data)", {
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, pop_size = 2640),
    NA
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, model_based = TRUE, pop_size = 2640),
    NA
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, pop_size = -5),
    "`pop_size` must be positive and greater or equal to `data x set_size`"
  )
  expect_error(
    rss_jps_estimate(emergence_ranks, 4, method = "JPS", replace = F, pop_size = 1),
    "`pop_size` must be positive and greater or equal to `data x set_size`"
  )
})
