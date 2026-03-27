# Compute an estimator for SBS PPS sampled data.

Compute an estimator for SBS PPS sampled data.

## Usage

``` r
sbs_pps_estimate(
  population,
  n,
  y,
  sample_matrix,
  n_bootstraps = 100,
  alpha = 0.05,
  n_cores = getOption("n_cores", 1)
)
```

## Arguments

- population:

  Population data frame to be sampled with 4 columns.

  1.  Halton numbers

  2.  X1-coordinate of population unit

  3.  X2-coordinate of population unit

  4.  Size measurements of population units

- n:

  Sample sizes (SBS sample size, PPS sample size).

- y:

  Sample response values.

- sample_matrix:

  Sample data frame to be sampled with 6 columns.

  1.  Halton numbers

  2.  X1-coordinate of population unit

  3.  X2-coordinate of population unit

  4.  Size measurement of population unit

  5.  Weight

  6.  Inclusion probability

- n_bootstraps:

  Number of bootstrap samples.

- alpha:

  The significance level.

- n_cores:

  The number of cores to be used for computational tasks (specify 0 for
  max). This can also be set by calling `options`, e.g.,
  `options(n_cores = 2)`.

## Value

A summary data frame of the estimator.

## Examples

``` r
set.seed(112)

# SBS sample size, PPS sample size
sample_sizes <- c(5, 5)

n_population <- 233
k <- 0:(n_population - 1)
x1 <- sample(1:13, n_population, replace = TRUE) / 13
x2 <- sample(1:8, n_population, replace = TRUE) / 8
y <- (x1 + x2) * runif(n = n_population, min = 1, max = 2) + 1
measured_sizes <- y * runif(n = n_population, min = 0, max = 4)

population <- matrix(cbind(k, x1, x2, measured_sizes), ncol = 4)
sample_result <- sbs_pps_sample(population, sample_sizes)

# estimate the population mean and construct a confidence interval
df_sample <- sample_result$sample
sample_id <- df_sample[, 1]
y_sample <- y[sample_id]

sbs_pps_estimates <- sbs_pps_estimate(
  population, sample_sizes, y_sample, df_sample,
  n_bootstrap = 100, alpha = 0.05
)
print(sbs_pps_estimates)
#>   n1 n2 Estimate  St.error 95% Confidence intervals
#> 1  5  5    2.849 0.1760682              2.451,3.247
#>   n1 n2 Estimate  St.error 95% Confidence intervals
#> 1  5  5    2.849 0.1760682              2.451,3.247
```
