# Calculate inclusion probabilities.

Calculate inclusion probabilities.

## Usage

``` r
calculate_inclusion_prob(
  size_measurement,
  n,
  n_cores = getOption("n_cores", 1)
)
```

## Arguments

- size_measurement:

  Size measurements of population units.

- n:

  Sample sizes (SBS sample size, PPS sample size).

- n_cores:

  The number of cores to be used for computational tasks (specify 0 for
  max).

## Value

A vector of inclusion probabilities.
