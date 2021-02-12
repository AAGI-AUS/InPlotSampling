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
    expect_error(OneSample(emergence_ranks, -4, "JPS", replace = T, pop_size = 2640),
                 "set_size must be a positive numeric value")
    expect_error(OneSample(emergence_ranks, "4", "JPS", replace = T, pop_size = 2640),
                 "set_size must be a positive numeric value")
    expect_error(OneSample(emergence_ranks, NA, "JPS", replace = T, pop_size = 2640),
                 "set_size must be a positive numeric value")
})

test_that("replace is TRUE or FALSE", {
    expect_error(OneSample(emergence_ranks, 4, "JPS", replace = NA, pop_size = 2640),
                 "replace must be TRUE or FALSE")
    expect_error(OneSample(emergence_ranks, 4, "JPS", replace = "NA", pop_size = 2640),
                 "replace must be TRUE or FALSE")
    expect_error(OneSample(emergence_ranks, 4, "JPS", replace = 0, pop_size = 2640),
                 "replace must be TRUE or FALSE")
})

test_that("pop_size is supplied when needed", {
    expect_error(OneSample(emergence_ranks, 4, "JPS", replace = F, 0),
                 "The population size pop_size must be provided when sampling without replacement")
    expect_error(OneSample(emergence_ranks, 4, "JPS", replace = T, model = 1),
                 "The population size pop_size must be provided for super-population model")
})

test_that("method is JPS or RSS", {
    expect_error(OneSample(emergence_ranks, 4, method = "ABC", replace = T),
                 "'arg' should be one of \"JPS\", \"RSS\"")
    expect_error(OneSample(emergence_ranks, 4, method = 1, replace = T),
                 "'arg' should be one of \"JPS\", \"RSS\"")
})
# Test data, conf = 0.95, model = 0, N = NULL

