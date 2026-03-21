
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
#>     Covariates:            none
#>     Selection method:      pearson
#>     Selection criterion:   p_value
#>     Selection level:       0.01
#>     Construction polarity: separate
#>     Edge weighting:        none
#>     Weight scale:          none
#>     Edge standardization:  none
#>     Streams:               joint, positive, negative
#>     Outcome model:         linear regression
summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Joint: 0.677
#>     Positive: 0.595
#>     Negative: 0.388
#>   Selected edges:
#>     Positive: 0.70%
#>     Negative: 0.20%
```

`cpm_spec()` can also express richer CPM variants without leaving the
native API surface:

``` r
rich_spec <- cpm_spec(
  selection = cpm_selection_cor(
    method = "spearman",
    criterion = "absolute",
    level = 0.1
  ),
  construction = cpm_construction_summary(
    polarity = "net",
    weight_scale = 0.03
  ),
  model = cpm_model_lm()
)

fit(rich_spec, conmat = conmat, behav = behav)$predictions |>
  head()
#>   row    observed         net
#> 1   1  0.26499342  0.82238735
#> 2   2  1.83074748  1.95343095
#> 3   3 -0.05937826  0.37334450
#> 4   4 -0.05320937 -0.01398366
#> 5   5  0.43790418  0.52797214
#> 6   6  1.33744904  1.35348830
```

This keeps the main CPM stages visible while grouping naturally paired
options into small helpers:

- `selection = cpm_selection_cor(...)` defines how edges are screened;
- `construction = cpm_construction_summary(...)` defines how screened
  edges become CPM-derived predictors;
- `model = cpm_model_lm()` defines the outcome model fitted on those
  predictors.

By default, `cpm_spec()` keeps edge standardization turned off so the
native workflow stays close to the classic CPM path of screening edges,
constructing summary features, and fitting the outcome model. If you
want fold-local edge z-scoring, opt in with
`cpm_construction_summary(standardize_edges = TRUE)`.

If you are new to CPM, the key idea behind the current summary-based
construction is:

- `positive` edges are edges whose screening association with the
  outcome is positive and passes the threshold;
- `negative` edges are edges whose screening association with the
  outcome is negative and passes the threshold;
- CPM then collapses those screened edge sets into subject-level summary
  features before fitting the outcome model.

With `polarity = "separate"`, `cpmr` keeps the positive and negative
summaries as separate predictors and reports three prediction streams:

- `joint`, which fits one model with both summaries together;
- `positive`, a positive-only diagnostic stream;
- `negative`, a negative-only diagnostic stream.

With `polarity = "net"`, `cpmr` constructs a single
`net_summary = positive_summary - negative_summary` feature and returns
one `net` prediction stream.

Cross-validated resampling uses `fit_resamples()` with the same
specification object:

``` r
resample_obj <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  resamples = 5
)

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Prediction error:
#>     RMSE:
#>       Joint: 1.242
#>       Positive: 1.204
#>       Negative: 1.199
#>     MAE:
#>       Joint: 0.951
#>       Positive: 0.956
#>       Negative: 0.910
#>   Pooled correlations (Pearson):
#>     Joint: -0.108
#>     Positive: -0.074
#>     Negative: -0.080
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.062 (SE 0.064)
#>     Positive: 0.007 (SE 0.086)
#>     Negative: -0.039 (SE 0.078)
head(resample_obj$predictions)
#>   row fold    observed       joint    positive    negative
#> 1   1    1  0.26499342 0.591709195 -0.08846823  0.76686948
#> 2   2    5  1.83074748 0.325440165  0.29380525  0.05747766
#> 3   3    5 -0.05937826 0.845311244  1.05634560 -0.34018567
#> 4   4    3 -0.05320937 0.664779940  0.56680746  0.25720238
#> 5   5    1  0.43790418 0.001660943  0.29376910 -0.36672391
#> 6   6    4  1.33744904 0.591903346  0.45146065  0.34438321
dim(resample_obj$edges)
#> NULL
head(resample_metrics(resample_obj))
#>   fold n_assess metric prediction  estimate
#> 1    1       20   rmse      joint 1.1177508
#> 2    1       20   rmse   positive 1.1031362
#> 3    1       20   rmse   negative 1.0772173
#> 4    2       20   rmse      joint 0.9617461
#> 5    2       20   rmse   positive 0.9885171
#> 6    2       20   rmse   negative 1.0425730
```

`summary(resample_obj)` gives the default aggregate report, with pooled
out-of-fold error metrics shown first and correlations reported as
supplementary statistics. Use `resample_metrics(resample_obj)` when you
want pooled or fold-wise metric tables directly.

For `fit_resamples()`, `resamples = NULL` means leave-one-out
resampling, `resamples = 5` means 5-fold resampling, and a list passed
to `resamples` enforces a manual assessment-fold partition.

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
