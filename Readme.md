
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RankedSetSampling

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/biometryhub/RankedSetSampling/branch/master/graph/badge.svg)](https://codecov.io/gh/biometryhub/RankedSetSampling?branch=master)
[![R build
status](https://github.com/biometryhub/RankedSetSampling/workflows/R-CMD-check/badge.svg)](https://github.com/biometryhub/RankedSetSampling/actions)
![pkgdown](https://github.com/biometryhub/RankedSetSampling/workflows/pkgdown/badge.svg)
<br> [![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.1-orange.svg?style=flat-square)](/commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--03--18-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The RankedSetSampling package provides a way for researchers to easily
implement Ranked Set Sampling in practice.

## Table of Contents

<!-- vim-markdown-toc GFM -->

* [Sampling Methods](#sampling-methods)
  * [JPS Sampling](#jps-sampling)
  * [RSS Sampling](#rss-sampling)
* [Installation](#installation)
* [Examples](#examples)
  * [JPS Sample and Estimator](#jps-sample-and-estimator)
  * [JPS Sample and Estimator](#jps-sample-and-estimator-1)
* [Citing this package](#citing-this-package)
* [Related Reference](#related-reference)

<!-- vim-markdown-toc -->

## Sampling Methods

### JPS Sampling

Sampling is made following the diagram below.

![JPS sampling diagram][jps-diagram]

### RSS Sampling

Sampling is made following the diagram below.

![RSS sampling diagram][rss-diagram]

## Installation

Use the following code to install this package:

``` r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("biometryhub/RankedSetSampling", upgrade = FALSE)
```

## Examples

### JPS Sample and Estimator

<details>
  <summary>snippet</summary>

  ``` r
  set.seed(112)
  population_size <- 600
  # the number of samples to be ranked in each set
  H <- 3

  with_replacement <- FALSE
  sigma <- 4
  mu <- 10
  n_rankers <- 3
  # sample size
  n <- 30

  rhos <- rep(0.75, n_rankers)
  taus <- sigma * sqrt(1 / rhos^2 - 1)
  population <- qnorm((1:population_size) / (population_size + 1), mu, sigma)

  data <- RankedSetSampling::jps_sample(population, n, H, taus, n_rankers, with_replacement)
  data <- data[order(data[, 2]), ]

  RankedSetSampling::rss_jps_estimate(
    data,
    set_size = H,
    method = "JPS",
    confidence = 0.80,
    replace = with_replacement,
    model_based = FALSE,
    pop_size = population_size
  )
  #>          Estimator Estimate Standard Error 80% Confidence intervals
  #> 1       UnWeighted    9.570          0.526               8.88,10.26
  #> 2      Sd.Weighted    9.595          0.569             8.849,10.341
  #> 3 Aggregate Weight    9.542          0.500             8.887,10.198
  #> 4     JPS Estimate    9.502          0.650             8.651,10.354
  #> 5     SRS estimate    9.793          0.783             8.766,10.821
  #> 6          Minimum    9.542          0.500             8.887,10.198
  ```
</details>

### JPS Sample and Estimator

<details>
  <summary>snippet</summary>

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
  ```
</details>

## Citing this package

This package can be cited using `citation("RankedSetSampling")` which
generates


    To cite package 'RankedSetSampling' in publications use:

      Omer Ozturk, Sam Rogers, Olena Kravchuk and Peter Kasprzak (2021).
      RankedSetSampling: Easing the Application of Ranked Set Sampling in
      Practice. R package version 0.0.1.
      https://biometryhub.github.io/RankedSetSampling/

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {RankedSetSampling: Easing the Application of Ranked Set Sampling in Practice},
        author = {Omer Ozturk and Sam Rogers and Olena Kravchuk and Peter Kasprzak},
        year = {2021},
        note = {R package version 0.0.1},
        url = {https://biometryhub.github.io/RankedSetSampling/},
      }

## Related Reference

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Ozturk2021" class="csl-entry">

Ozturk, Omer, and Olena Kravchuk. 2021. “Judgment Post-Stratified
Assessment Combining Ranking Information from Multiple Sources, with a
Field Phenotyping Example.” *Journal of Agricultural, Biological and
Environmental Statistics*. <https://doi.org/10.1007/s13253-021-00439-1>.

</div>

</div>

<!-- links -->

<!-- images -->

<!-- [jps-diagram]: ./assets/img/jps-diagram.drawio.png -->
[jps-diagram]: ./assets/img/jps-diagram.drawio.svg
[rss-diagram]: ./assets/img/rss-diagram.drawio.svg
