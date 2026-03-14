
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpmr

<!-- badges: start -->

[![R-CMD-check](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/psychelzh/cpmr/graph/badge.svg)](https://app.codecov.io/gh/psychelzh/cpmr)
[![CRAN
status](https://www.r-pkg.org/badges/version/cpmr)](https://CRAN.R-project.org/package=cpmr)
<!-- badges: end -->

The cpmr package is specifically designed for the analysis of the
connectome predictive modeling (CPM) method in R. This package relies on
[Rfast](https://CRAN.R-project.org/package=Rfast) to do row oriented
calculation.

## Installation

You can install the released version of cpmr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cpmr")
```

Or you can install the development version of cpmr from
[r-universe](https://psychelzh.r-universe.dev) with:

``` r
install.packages("cpmr", repos = c("https://psychelzh.r-universe.dev", getOption("repos")))
```

## Example

It is very simple to use this package. Just shape your connectivity
matrix as a subjects by edges matrix, i.e., each row contains the
correlation matrix (removed diagonal and duplicated values, e.g., lower
triangular data) for each subject, and your behavior data a vector and
feed them to `fit()`.

``` r
library(cpmr)

withr::local_seed(123)
conmat <- matrix(rnorm(100 * 1000), nrow = 100)
behav <- rnorm(100)
spec <- cpm_spec()
res <- fit(spec, conmat = conmat, behav = behav, return_edges = TRUE)
res
#> CPM results:
#>   Call: fit(object = spec, conmat = conmat, behav = behav, return_edges = TRUE)
#>   Number of observations: 100
#>     Complete cases: 100
#>   Number of edges: 1000
#>   Parameters:
#>     Covariates:       FALSE
#>     Threshold method: alpha
#>     Threshold level:  0.01
#>     Stored splits:    1
#>     Bias correction:  TRUE
summary(res)
#> CPM summary:
#>   Performance (Pearson):
#>     Positive: 0.595
#>     Negative: 0.387
#>     Combined: 0.676
#>   Prop. edges (50% folds):
#>     Positive: 0.70%
#>     Negative: 0.20%
```

## Code of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
