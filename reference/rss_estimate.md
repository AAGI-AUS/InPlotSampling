# This function provides an estimator for RSS data

This function provides an estimator for RSS data

## Usage

``` r
rss_estimate(data, set_size, replace, model_based, pop_size, alpha)
```

## Arguments

- data:

  A matrix with a response variable in the first column and ranks in the
  following columns.

- set_size:

  Set size for each ranking group.

- replace:

  A boolean which specifies whether to sample with replacement or not.

- model_based:

  An inference mode:

  - `FALSE`: design based inference

  - `TRUE`: model based inference using super population model

- pop_size:

  The population size. Must be provided if

  - sampling without replacement, or

  - `model_based` is `TRUE`.

- alpha:

  A significance level.
