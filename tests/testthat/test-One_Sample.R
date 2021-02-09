test_that("onesample works", {
    skip_if(getRversion() < 3.4)
    load("../onesample.Rdata")
    output <- OneSample(Data,Setsize,Method,0.95, Replace,Model,N)
  expect_equal(output$Estimate, c(9.978, 9.947, 9.867, 10.014, 9.584, 9.947))
})


# Test data, set_size, method = c("JPS", "RSS"), conf = 0.95, replace = TRUE, model = 0, N = NULL
