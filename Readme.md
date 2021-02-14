
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
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--02--14-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The RankedSetSampling package provides a way for researchers to easily
implement Ranked Set Sampling in practice.

## Installation

Use the following code to install this package:

``` r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("biometryhub/RankedSetSampling", upgrade = FALSE)
```

# Example of use

This package includes some example data files, which can be seen at
[population](reference/population.html) and
[emergence\_ranks](reference/emergence_ranks.html). After installing the
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
#>          Estimator Estimate Standard Error 95% Confidence intervals
#> 1       UnWeighted    1.117          0.238              0.606,1.627
#> 2      Sd.Weighted    1.114          0.253              0.572,1.656
#> 3 Aggregate Weight    1.108          0.210              0.657,1.559
#> 4     JPS Estimate    1.021          0.269              0.443,1.599
#> 5     SRS estimate    1.200          0.262              0.638,1.762
#> 6          Minimum    1.108          0.210              0.657,1.559
```

# Citing this package

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
