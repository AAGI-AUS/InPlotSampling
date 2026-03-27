# Generate probability proportional to size (PPS) and spatially balanced sampling on the population provided.

Generate probability proportional to size (PPS) and spatially balanced
sampling on the population provided.

## Usage

``` r
sbs_pps_sample(population, n, n_cores = getOption("n_cores", 1))
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

- n_cores:

  The number of cores to be used for computational tasks (specify 0 for
  max). This can also be set by calling `options`, e.g.,
  `options(n_cores = 2)`.

## Value

A named list of:

- heatmap: heat map of the sample

- sample: SBS PPS sample of the population

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
print(sample_result$sample)
#>    sbs_pps_indices        x1    x2       size      weight inclusion_probability
#> 1               87 0.4615385 0.625  0.4665423 0.000000000            0.02319163
#> 2               88 0.1538462 0.625  1.7389902 0.000000000            0.02790409
#> 3               89 0.8461538 0.625  7.0815547 0.000000000            0.04749104
#> 4               90 0.6923077 0.750  9.5428032 0.000000000            0.05640733
#> 5               91 0.2307692 0.750  5.1375136 0.000000000            0.04039996
#> 6              173 0.1538462 0.500  6.6400168 0.005024130            0.04588620
#> 7               26 0.6153846 0.500  4.3146186 0.003264631            0.03738898
#> 8              232 0.8461538 0.750 12.0057856 0.009084108            0.06526583
#> 9              171 0.6153846 0.750  6.9029083 0.005223046            0.04684225
#> 10              29 0.8461538 0.375  4.6324720 0.003505133            0.03855377
#>    sbs_pps_indices        x1    x2       size      weight inclusion_probability
#> 1               87 0.4615385 0.625  0.4665423 0.000000000            0.02319163
#> 2               88 0.1538462 0.625  1.7389902 0.000000000            0.02790409
#> 3               89 0.8461538 0.625  7.0815547 0.000000000            0.04749104
#> 4               90 0.6923077 0.750  9.5428032 0.000000000            0.05640733
#> 5               91 0.2307692 0.750  5.1375136 0.000000000            0.04039996
#> 6              173 0.1538462 0.500  6.6400168 0.005024130            0.04588620
#> 7               26 0.6153846 0.500  4.3146186 0.003264631            0.03738898
#> 8              232 0.8461538 0.750 12.0057856 0.009084108            0.06526583
#> 9              171 0.6153846 0.750  6.9029083 0.005223046            0.04684225
#> 10              29 0.8461538 0.375  4.6324720 0.003505133            0.03855377
```
