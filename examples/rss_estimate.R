population_size <- 600
# the number of samples to be ranked in each set
set_size <- 3


sigma <- 4
mu <- 10
n_rankers <- 3
# sample size
n <- 30

alpha <- 0.05
rhos <- rep(0.75, n_rankers)
taus <- sigma * sqrt(1 / rhos^2 - 1)

population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)
rho <- 0.75
tau <- sigma * sqrt(1 / rho^2 - 1)
x <- population + tau * rnorm(population_size, 0, 1)

population <- cbind(population, x)
data <- rss_sample(population, n, set_size, n_rankers, with_replacement)
data <- data[order(data[, 2]), ]

rss_estimates <- rss_jps_estimate(
  data,
  set_size,
  method = "RSS",
  confidence = 0.80,
  with_replacement = FALSE,
  model_based = FALSE,
  population_size = population_size
)

print(rss_estimates)
