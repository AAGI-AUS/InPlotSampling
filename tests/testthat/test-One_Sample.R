test_that("onesample works with JPS", {
  skip_if(getRversion() < 3.4)
  load("../jps_data.Rdata")
  expect_identical(OneSample(Data, 3, "JPS", 0.95, FALSE, 0, 600), saved_jps_output)
})

test_that("onesample works with RSS", {
  skip_if(getRversion() < 3.4)
  load("../rss_data.Rdata")
  expect_identical(OneSample(Data, 3, "RSS", 0.95, FALSE, 0, 600), saved_rss_output)
})

test_that("set_size is positive", {
  expect_error(
    OneSample(emergence_ranks, -4, "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
  expect_error(
    OneSample(emergence_ranks, "4", "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
  expect_error(
    OneSample(emergence_ranks, NA, "JPS", replace = T, pop_size = 2640),
    "`set_size` must be a positive whole number."
  )
})

test_that("replace is TRUE or FALSE", {
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = NA, pop_size = 2640),
    "`replace` must be a boolean."
  )
  # expect_error(
  #   OneSample(emergence_ranks, 4, "RSS", replace = NA, pop_size = 2640),
  #   "`replace` must be a boolean."
  # )
  # expect_error(
  #   OneSample(emergence_ranks, 4, "JPS", replace = "NA", pop_size = 2640),
  #   "`replace` must be a boolean."
  # )
  expect_error(
    OneSample(emergence_ranks, 4, "RSS", replace = "NA", pop_size = 2640),
    "`replace` must be a boolean."
  )
  # expect_error(
  #   OneSample(emergence_ranks, 4, "JPS", replace = 0, pop_size = 2640),
  #   "`replace` must be a boolean."
  # )
  expect_error(
    OneSample(emergence_ranks, 4, "RSS", replace = 0, pop_size = 2640),
    "`replace` must be a boolean."
  )
})

test_that("pop_size is supplied when needed", {
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = F, 0),
    "A numeric population size `pop_size` must be provided when sampling without replacement"
  )
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = T, model = 1),
    "The population size `pop_size` must be provided for super-population model"
  )
})

test_that("method is correct", {
  expect_error(
    OneSample(emergence_ranks, 4, method = "ABC", replace = T),
    '`method` must be `"JPS"` or `"RSS"`.'
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = 1, replace = T),
    '`method` must be `"JPS"` or `"RSS"`.'
  )
})

test_that("confidence is between 0 and 1", {
  expect_error(OneSample(emergence_ranks, 4, "JPS", replace = T, confidence = 0), NA)
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = T, confidence = 5),
    "`confidence` must be inclusively between 0 and 1."
  )
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = T, confidence = "A"),
    "`confidence` must be inclusively between 0 and 1."
  )
  expect_error(
    OneSample(emergence_ranks, 4, "JPS", replace = T, confidence = NA),
    "`confidence` must be inclusively between 0 and 1."
  )
})

test_that("model is 0 or 1", {
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", model = 0),
    NA
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", model = "A"),
    "`model` must be `0` or `1`."
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", model = NA),
    "`model` must be `0` or `1`."
  )
})

test_that("pop_size is > 0 and >= set_size*nrow(data)", {
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, pop_size = 2640),
    NA
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, model = 1, pop_size = 2640),
    NA
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, pop_size = -5),
    "`pop_size` must be positive and greater or equal to `data x set_size`"
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, pop_size = 1),
    "`pop_size` must be positive and greater or equal to `data x set_size`"
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, pop_size = NA),
    "A numeric population size `pop_size` must be provided when sampling without replacement"
  )
  expect_error(
    OneSample(emergence_ranks, 4, method = "JPS", replace = F, pop_size = "A"),
    "A numeric population size `pop_size` must be provided when sampling without replacement"
  )
})
