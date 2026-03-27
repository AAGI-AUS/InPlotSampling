# Computes the estimator and variance for each individual ranker

Computes the estimator and variance for each individual ranker

## Usage

``` r
jps_estimate_single(
  ranks,
  y,
  set_size,
  N,
  coef,
  coef_del,
  replace,
  model_based,
  K
)
```

## Arguments

- ranks:

  Ranks of Y.

- y:

  Response measurements.

- set_size:

  Set size for each ranking group.

- N:

  Finite population size.

- coef:

  Coefficients used in variance computation when sample size is n.

- coef_del:

  Coefficients used in variance computation when the i-th unit is
  deleted.

- replace:

  Logical. Sample with replacement?

- model_based:

  An inference mode:

  - `FALSE`: design based inference

  - `TRUE`: model based inference using super population model

- K:

  Number of rankers.

## Value

A `data.frame` with the point estimates provided by JPS estimators along
with standard error and confidence intervals.
