# Generate ranked set sampling (RSS) with replacement on the population provided.

Generate ranked set sampling (RSS) with replacement on the population
provided.

## Usage

``` r
rss_sample_w_replacement(pop, n, H, K)
```

## Arguments

- pop:

  Population that will be sampled with an auxiliary parameter in the
  second column.

- n:

  Sample size.

- H:

  Set size for each ranking group.

- K:

  Number of rankers.

## Value

A matrix with ranks from each ranker.
