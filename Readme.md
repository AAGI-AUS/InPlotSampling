
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RankedSetSampling

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/biometryhub/RankedSetSampling/branch/main/graph/badge.svg)](https://codecov.io/gh/biometryhub/RankedSetSampling?branch=main)
[![R build
status](https://github.com/biometryhub/RankedSetSampling/workflows/R-CMD-check/badge.svg)](https://github.com/biometryhub/RankedSetSampling/actions)
![pkgdown](https://github.com/biometryhub/RankedSetSampling/workflows/pkgdown/badge.svg)
<br> [![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](/commits/main)
[![Last-changedate](https://img.shields.io/badge/last%20change-2024--05--24-yellowgreen.svg)](/commits/main)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The RankedSetSampling package provides a way for researchers to easily
implement Ranked Set Sampling in practice.

## Table of Contents

<!-- vim-markdown-toc GFM -->

- [Sampling Methods](#sampling-methods)
  - [JPS Sampling](#jps-sampling)
  - [RSS Sampling](#rss-sampling)
- [Installation](#installation)
- [Examples](#examples)
  - [JPS Sample and Estimator](#jps-sample-and-estimator)
  - [SBS PPS Sample and Estimator](#sbs-pps-sample-and-estimator)
- [Citing this package](#citing-this-package)
- [Related Reference](#related-reference)

<!-- vim-markdown-toc -->

## Sampling Methods

### JPS Sampling

Sampling is made following the diagram below.

<figure>
<img src="man/figures/jps-diagram.drawio.svg"
alt="JPS sampling diagram" />
<figcaption aria-hidden="true">JPS sampling diagram</figcaption>
</figure>

### RSS Sampling

Sampling is made following the diagram below.

<figure>
<img src="man/figures/rss-diagram.drawio.svg"
alt="RSS sampling diagram" />
<figcaption aria-hidden="true">RSS sampling diagram</figcaption>
</figure>

## Installation

Use the following code to install this package:

``` r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("biometryhub/RankedSetSampling", upgrade = FALSE)
```

# Example of use

This package includes some example data files, which can be seen at
[population](reference/population.html) and
[emergence_ranks](reference/emergence_ranks.html). After installing the
package as above, the package can be used as in the following example:

``` r
# load the package
library(RankedSetSampling)

# Compute the JPS estimators

JPS.Estimates <- OneSample(data = emergence_ranks, set_size = 4,
                           method = "JPS", confidence = 0.95, 
                           replace = TRUE, model = 0, 
                           pop_size = nrow(population))

print(JPS.Estimates)
```

# Citing this package

This package can be cited using `citation("RankedSetSampling")` which
generates

    To cite package 'RankedSetSampling' in publications use:

      Ozturk O, Rogers S, Kravchuk O, Kasprzak P (2021).
      _RankedSetSampling: Easing the Application of Ranked Set Sampling in
      Practice_. R package version 0.1.0,
      <https://biometryhub.github.io/RankedSetSampling/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {RankedSetSampling: Easing the Application of Ranked Set Sampling in Practice},
        author = {Omer Ozturk and Sam Rogers and Olena Kravchuk and Peter Kasprzak},
        year = {2021},
        note = {R package version 0.1.0},
        url = {https://biometryhub.github.io/RankedSetSampling/},
      }

# Related Reference

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Ozturk2021" class="csl-entry">

Ozturk, Omer, and Olena Kravchuk. 2021. “Judgment Post-Stratified
Assessment Combining Ranking Information from Multiple Sources, with a
Field Phenotyping Example.” *Journal of Agricultural, Biological and
Environmental Statistics*. <https://doi.org/10.1007/s13253-021-00439-1>.

</div>

</div>
