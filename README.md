
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
behavioral vector. The most direct entry point is `cpm_fit()`.

``` r
library(cpmr)

withr::local_seed(123)
conmat <- matrix(rnorm(100 * 1000), nrow = 100)
behav <- rnorm(100)
fit_obj <- cpm_fit(
  conmat = conmat,
  behav = behav
)

fit_obj
#> CPM results:
#>   Call: cpm_fit(conmat = conmat, behav = behav)
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

Cross-validated resampling uses the matching native helper
`cpm_fit_resamples()`:

``` r
resample_obj <- cpm_fit_resamples(
  conmat = conmat,
  behav = behav,
  kfolds = 5
)

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: -0.057 (SE 0.062)
#>     Positive: 0.008 (SE 0.089)
#>     Negative: -0.036 (SE 0.077)
#>   Selected edges:
#>     Positive: 0.56%
#>     Negative: 0.32%
head(resample_obj$metrics)
#>   fold n_assess        both         pos         neg
#> 1    1       20 -0.12117842 -0.02914167 -0.10186599
#> 2    2       20  0.13754956  0.13929784  0.03564001
#> 3    3       20 -0.21377733 -0.28600381 -0.02101708
#> 4    4       20 -0.11890438  0.23862535 -0.27888050
#> 5    5       20  0.03090406 -0.02240053  0.18779668
dim(resample_obj$edges)
#> [1] 1000    2
```

## Choosing a path

`cpmr` now treats the native workflow as the primary package story:

- use `cpm_fit()` and `cpm_fit_resamples()` for most real CPM analyses;
- use `fit(cpm_spec(), ...)` and `fit_resamples(cpm_spec(), ...)` when
  you want the lower-level specification object directly.

Why this matters: CPM often needs leakage-safe fold-local preprocessing
and can benefit from future fold-level caching or threshold-specific
optimization. Those workloads fit native `cpmr` runners better than a
generic orchestration layer.

## Code of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
