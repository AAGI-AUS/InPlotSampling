# Computes the estimator for JPS data

Computes the estimator for JPS data

## Usage

``` r
jps_estimate(data, set_size, replace = TRUE, model_based, N, alpha)
```

## Arguments

- data:

  The data to use for estimation.

- set_size:

  Set size for each ranking group.

- replace:

  Logical (default `TRUE`). Sample with replacement?

- model_based:

  An inference mode:

  - `FALSE`: design based inference

  - `TRUE`: model based inference using super population model

- N:

  The population size.

- alpha:

  The significance level.

## Value

A `data.frame` with the point estimates provided by JPS estimators along
with standard error and confidence intervals.
