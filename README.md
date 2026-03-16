
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpmr

<!-- badges: start -->

[![R-CMD-check](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/psychelzh/cpmr/graph/badge.svg)](https://app.codecov.io/gh/psychelzh/cpmr)
[![CRAN
status](https://www.r-pkg.org/badges/version/cpmr)](https://CRAN.R-project.org/package=cpmr)
<!-- badges: end -->

The cpmr package is designed for connectome predictive modeling (CPM) in
R. Its primary workflow is a native, matrix-first API that keeps
CPM-specific training, resampling, and leakage-safe preprocessing inside
package-controlled code. This package relies on
[Rfast](https://CRAN.R-project.org/package=Rfast) for row-oriented
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

## Native workflow

Shape your connectivity matrix as a subjects-by-edges matrix, where each
row contains the edge vector for one subject, and pair it with a
behavioral vector. The native workflow uses `cpm_spec()` together with
`fit()`.

``` r
library(cpmr)

withr::local_seed(123)
conmat <- matrix(rnorm(100 * 1000), nrow = 100)
behav <- rnorm(100)
fit_obj <- fit(cpm_spec(), conmat = conmat, behav = behav)

fit_obj
#> CPM results:
#>   Call: fit(object = cpm_spec(), conmat = conmat, behav = behav)
#>   Number of observations: 100
#>     Complete cases: 100
#>   Number of edges: 1000
#>   Parameters:
#>     Covariates:       FALSE
#>     Threshold method: alpha
#>     Threshold level:  0.01
#>     Bias correction:  TRUE
summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Combined: 0.676
#>     Positive: 0.595
#>     Negative: 0.387
#>   Selected edges:
#>     Positive: 0.70%
#>     Negative: 0.20%
```

Cross-validated resampling uses `fit_resamples()` with the same
specification object:

``` r
resample_obj <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav, kfolds = 5)

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: -0.057 (SE 0.062)
#>     Positive: 0.008 (SE 0.089)
#>     Negative: -0.036 (SE 0.077)
head(resample_obj$predictions)
#>   row fold        real        both        pos         neg
#> 1   1    1  0.26499342 0.492748241 -0.2058990  0.76974455
#> 2   2    5  1.83074748 0.323127546  0.2918329  0.05747766
#> 3   3    5 -0.05937826 0.901452079  1.1152311 -0.34018567
#> 4   4    3 -0.05320937 0.659647280  0.5558721  0.26308893
#> 5   5    1  0.43790418 0.002067539  0.2977574 -0.37106308
#> 6   6    4  1.33744904 0.659172053  0.5130919  0.37428531
dim(resample_obj$edges)
#> NULL
```

## Choosing a path

`cpmr` now treats the native workflow as the primary package story:

- use `fit(cpm_spec(), ...)` and `fit_resamples(cpm_spec(), ...)` for
  native CPM analyses;
- keep the `cpm_spec()` object around when you want an explicit,
  reusable parameter object.

Why this matters: CPM often needs leakage-safe fold-local preprocessing
and can benefit from future fold-level caching or threshold-specific
optimization. Those workloads fit native `cpmr` runners better than a
generic orchestration layer.

## Code of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
