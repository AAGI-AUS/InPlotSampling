# Compute empirical population by imputing reponses and measured sizes using knn.

Compute empirical population by imputing reponses and measured sizes
using knn.

## Usage

``` r
get_empirical_population(sample_indices, pop, y)
```

## Arguments

- sample_indices:

  Indices of sample.

- pop:

  Population data frame to be sampled with 4 columns.

  1.  Halton numbers

  2.  X1-coordinate of population unit

  3.  X2-coordinate of population unit

  4.  Size measurement of population unit

- y:

  Sample response values.

## Value

A summary data frame of the estimator.
