
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpmr

<!-- badges: start -->

[![R-CMD-check](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/psychelzh/cpmr/graph/badge.svg)](https://app.codecov.io/gh/psychelzh/cpmr)
[![CRAN
status](https://www.r-pkg.org/badges/version/cpmr)](https://CRAN.R-project.org/package=cpmr)
<!-- badges: end -->

The cpmr package implements connectome predictive modeling (CPM) in R.
Its primary workflow is native and matrix-first: you pass a
subject-by-edge matrix together with a behavioral vector, and `cpmr`
handles CPM-specific training, resampling, and leakage-safe
preprocessing inside package code. This package relies on
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
#> CPM fit:
#>   Call: fit(object = cpm_spec(), conmat = conmat, behav = behav)
#>   Number of observations: 100
#>     Complete cases: 100
#>   Candidate edges: 1000
#>   Parameters:
#>     Covariates:       none
#>     Association:      pearson
#>     Threshold method: alpha
#>     Threshold level:  0.01
#>     Feature space:    separate
#>     Edge weighting:   binary
#>     Weighting scale:  0.05
#>     Outcome model:    linear regression
#>     Streams:          joint, positive, negative
#>     Edge standardization: z-score
summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Joint: 0.676
#>     Positive: 0.595
#>     Negative: 0.387
#>   Selected edges:
#>     Positive: 0.70%
#>     Negative: 0.20%
```

`cpm_spec()` can also express richer CPM variants without leaving the
native API surface:

``` r
rich_spec <- cpm_spec(
  screen = cpm_screen(
    association = "spearman",
    threshold = cpm_threshold("effect_size", level = 0.1)
  ),
  feature_space = "net",
  weighting = cpm_weighting("sigmoid", scale = 0.03),
  model = cpm_model_lm()
)

fit(rich_spec, conmat = conmat, behav = behav)$predictions |>
  head()
#>   row        real         net
#> 1   1  0.26499342  0.82985741
#> 2   2  1.83074748  1.97548933
#> 3   3 -0.05937826  0.32139217
#> 4   4 -0.05320937 -0.02564907
#> 5   5  0.43790418  0.50479001
#> 6   6  1.33744904  1.35163465
```

This keeps the main CPM choices visible while grouping naturally paired
options like screening, edge weighting, and the outcome model into small
helper objects.

If you are new to CPM, the key idea behind `feature_space` is:

- `positive` edges are edges whose screening association with the
  outcome is positive and passes the threshold;
- `negative` edges are edges whose screening association with the
  outcome is negative and passes the threshold;
- CPM then collapses those screened edge sets into subject-level network
  strengths before fitting the outcome model.

With `feature_space = "separate"`, `cpmr` keeps the positive and
negative strengths as separate predictors and reports the classic
`joint`, `positive`, and `negative` prediction streams. With
`feature_space = "net"`, `cpmr` constructs a single
`net_strength = positive_strength - negative_strength` feature and
returns one `net` prediction stream.

Cross-validated resampling uses `fit_resamples()` with the same
specification object:

``` r
resample_obj <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav, kfolds = 5)

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Prediction error:
#>     RMSE:
#>       Joint: 1.243
#>       Positive: 1.205
#>       Negative: 1.200
#>     MAE:
#>       Joint: 0.947
#>       Positive: 0.962
#>       Negative: 0.905
#>   Pooled correlations (Pearson):
#>     Joint: -0.104
#>     Positive: -0.072
#>     Negative: -0.074
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.057 (SE 0.062)
#>     Positive: 0.008 (SE 0.089)
#>     Negative: -0.036 (SE 0.077)
head(resample_obj$predictions)
#>   row fold        real       joint   positive    negative
#> 1   1    1  0.26499342 0.492748241 -0.2058990  0.76974455
#> 2   2    5  1.83074748 0.323127546  0.2918329  0.05747766
#> 3   3    5 -0.05937826 0.901452079  1.1152311 -0.34018567
#> 4   4    3 -0.05320937 0.659647280  0.5558721  0.26308893
#> 5   5    1  0.43790418 0.002067539  0.2977574 -0.37106308
#> 6   6    4  1.33744904 0.659172053  0.5130919  0.37428531
dim(resample_obj$edges)
#> NULL
head(resample_metrics(resample_obj))
#>   fold n_assess metric prediction  estimate
#> 1    1       20   rmse      joint 1.1115539
#> 2    1       20   rmse   positive 1.1024986
#> 3    1       20   rmse   negative 1.0753358
#> 4    2       20   rmse      joint 0.9556034
#> 5    2       20   rmse   positive 0.9872521
#> 6    2       20   rmse   negative 1.0424502
```

`summary(resample_obj)` gives the default aggregate report, with pooled
out-of-fold error metrics shown first and correlations reported as
supplementary statistics. Use `resample_metrics(resample_obj)` when you
want pooled or fold-wise metric tables directly.

## Choosing a path

`cpmr` treats this native workflow as the primary package path:

- use `fit(cpm_spec(), ...)` and `fit_resamples(cpm_spec(), ...)` for
  native CPM analyses;
- keep the `cpm_spec()` object around when you want an explicit,
  reusable parameter object.

Why this matters: CPM often needs leakage-safe fold-local preprocessing
and can benefit from future fold-level caching or threshold-aware
optimization. Those workloads fit native `cpmr` runners better than a
generic orchestration layer.

## Code of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
