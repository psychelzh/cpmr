
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
handles CPM-specific training, fold planning, and leakage-safe
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
behavioral vector. The native workflow uses `spec()` together with
`cpm()`.

``` r
library(cpmr)

withr::local_seed(123)
conmat <- matrix(rnorm(100 * 1000), nrow = 100)
behav <- rnorm(100)
s <- spec()
result <- cpm(conmat = conmat, behav = behav, spec = s, resamples = 5)

summary(result)
#> CPM summary:
#>   Number of folds: 5
#>   Pooled correlations (Pearson):
#>     Joint: -0.108
#>     Positive: -0.074
#>     Negative: -0.080
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.062 (SE 0.064)
#>     Positive: 0.007 (SE 0.086)
#>     Negative: -0.039 (SE 0.078)
#>   Selected edges:
#>     Positive: 0.56%
#>     Negative: 0.32%
head(result$predictions)
#>   row fold    observed       joint    positive    negative
#> 1   1    1  0.26499342 0.591709195 -0.08846823  0.76686948
#> 2   2    5  1.83074748 0.325440165  0.29380525  0.05747766
#> 3   3    5 -0.05937826 0.845311244  1.05634560 -0.34018567
#> 4   4    3 -0.05320937 0.664779940  0.56680746  0.25720238
#> 5   5    1  0.43790418 0.001660943  0.29376910 -0.36672391
#> 6   6    4  1.33744904 0.591903346  0.45146065  0.34438321
```

`spec()` can also express richer CPM variants without leaving the native
API surface:

``` r
rich_spec <- spec(
  selection = cpm_selection_cor(
    method = "spearman",
    criterion = "absolute",
    level = 0.1
  ),
  construction = cpm_construction_strength(
    sign_mode = "net",
    weight_scale = 0.03
  ),
  model = cpm_model_lm()
)

cpm(conmat = conmat, behav = behav, spec = rich_spec, resamples = 5)$predictions |>
  head()
#>   row fold    observed         net
#> 1   1    1  0.26499342  0.12167754
#> 2   2    1  1.83074748  0.47750573
#> 3   3    5 -0.05937826  0.43681670
#> 4   4    5 -0.05320937 -0.01008645
#> 5   5    2  0.43790418 -0.06417020
#> 6   6    3  1.33744904  0.09903232
```

This keeps the main CPM stages visible while grouping naturally paired
options into small helpers:

- `selection = cpm_selection_cor(...)` defines how edges are screened;
- `construction = cpm_construction_strength(...)` defines how screened
  edges become CPM-derived predictors;
- `model = cpm_model_lm()` defines the outcome model fitted on those
  predictors.

By default, `spec()` keeps edge standardization turned off so the native
workflow stays close to the classic CPM path of screening edges,
constructing summary features, and fitting the outcome model. If you
want fold-local edge z-scoring, opt in with
`cpm_construction_strength(standardize_edges = TRUE)`.

If you are new to CPM, the key idea behind the current summary-based
construction is:

- `positive` edges are edges whose screening association with the
  outcome is positive and passes the threshold;
- `negative` edges are edges whose screening association with the
  outcome is negative and passes the threshold;
- CPM then collapses those screened edge sets into subject-level summary
  features before fitting the outcome model.

With `sign_mode = "separate"`, `cpmr` keeps the positive and negative
summaries as separate predictors and reports three prediction streams:

- `joint`, which fits one model with both summaries together;
- `positive`, a positive-only diagnostic stream;
- `negative`, a negative-only diagnostic stream.

With `sign_mode = "net"`, `cpmr` constructs a single
`net_summary = positive_summary - negative_summary` feature and returns
one `net` prediction stream.

`cpm()` is fold-based: it runs CPM under the requested assessment plan
and returns out-of-fold predictions together with the fold structure.

``` r
summary(result)
#> CPM summary:
#>   Number of folds: 5
#>   Pooled correlations (Pearson):
#>     Joint: -0.108
#>     Positive: -0.074
#>     Negative: -0.080
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.062 (SE 0.064)
#>     Positive: 0.007 (SE 0.086)
#>     Negative: -0.039 (SE 0.078)
#>   Selected edges:
#>     Positive: 0.56%
#>     Negative: 0.32%
head(result$predictions)
#>   row fold    observed       joint    positive    negative
#> 1   1    1  0.26499342 0.591709195 -0.08846823  0.76686948
#> 2   2    5  1.83074748 0.325440165  0.29380525  0.05747766
#> 3   3    5 -0.05937826 0.845311244  1.05634560 -0.34018567
#> 4   4    3 -0.05320937 0.664779940  0.56680746  0.25720238
#> 5   5    1  0.43790418 0.001660943  0.29376910 -0.36672391
#> 6   6    4  1.33744904 0.591903346  0.45146065  0.34438321
dim(result$edges)
#> [1] 1000    2
head(tidy(result, component = "metrics"))
#> # A tibble: 6 × 5
#>    fold n_assess prediction estimate method 
#>   <int>    <int> <chr>         <dbl> <chr>  
#> 1     1       20 joint       -0.145  pearson
#> 2     1       20 positive    -0.0444 pearson
#> 3     1       20 negative    -0.110  pearson
#> 4     2       20 joint        0.138  pearson
#> 5     2       20 positive     0.154  pearson
#> 6     2       20 negative     0.0234 pearson
head(tidy(result, component = "metrics", level = "pooled"))
#> # A tibble: 3 × 3
#>   prediction estimate method 
#>   <chr>         <dbl> <chr>  
#> 1 joint       -0.108  pearson
#> 2 positive    -0.0736 pearson
#> 3 negative    -0.0798 pearson
```

`summary(result)` gives the default aggregate correlation report. Use
`tidy(result, component = "metrics")` when you want fold-wise
correlation tables directly, or
`tidy(result, component = "metrics", level = "pooled")` for pooled
correlation tables.

For `cpm()`, `resamples = NULL` means leave-one-out assessment,
`resamples = 5` means 5-fold assessment, and a list passed to
`resamples` enforces a manual assessment-fold partition.

## Choosing a path

`cpmr` treats this native workflow as the primary package path:

- use `cpm(..., spec = spec())` for native CPM analyses;
- keep the `spec()` object around when you want an explicit, reusable
  parameter object.

Why this matters: CPM often needs leakage-safe fold-local preprocessing
and can benefit from future fold-level caching or threshold-aware
optimization. Those workloads fit native `cpmr` runners better than a
generic orchestration layer.

## Code of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
