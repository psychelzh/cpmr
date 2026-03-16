# Native Getting Started with cpmr

## Overview

`cpmr`’s primary workflow is a native, matrix-first API built around
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
together with [`fit()`](https://generics.r-lib.org/reference/fit.html)
and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).

This vignette walks through:

- how to shape CPM inputs;
- how to fit a single native CPM model;
- how to run leakage-safe resampling;
- how to inspect predictions and selected edges;
- how custom resamples and missing-data handling work.

## Prepare Inputs

CPM expects one row per subject and one column per edge.

- `conmat`: an `n x p` connectivity matrix;
- `behav`: a length-`n` numeric outcome vector;
- `covariates`: an optional `n x q` nuisance matrix.

``` r
library(cpmr)

withr::local_seed(123)
n <- 80
p <- 200

conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
covariates <- matrix(rnorm(n), ncol = 1)
behav <- rowMeans(conmat[, 1:12, drop = FALSE]) +
  0.5 * covariates[, 1] +
  rnorm(n, sd = 0.5)
```

## Single Fit with `fit()`

Use [`fit()`](https://generics.r-lib.org/reference/fit.html) on a
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
object when you want a native single fit with explicit CPM parameters.

``` r
fit_obj <- fit(
  cpm_spec(thresh_method = "alpha", thresh_level = 0.05),
  conmat = conmat,
  behav = behav,
  covariates = covariates
)

fit_obj
#> CPM results:
#>   Call: fit(object = cpm_spec(thresh_method = "alpha", thresh_level = 0.05), 
#>     conmat = conmat, behav = behav, covariates = covariates)
#>   Number of observations: 80
#>     Complete cases: 80
#>   Number of edges: 200
#>   Parameters:
#>     Covariates:       TRUE
#>     Threshold method: alpha
#>     Threshold level:  0.05
#>     Bias correction:  TRUE
summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Combined: 0.706
#>     Positive: 0.620
#>     Negative: 0.458
#>   Selected edges:
#>     Positive: 4.00%
#>     Negative: 2.50%
dim(fit_obj$edges)
#> [1] 200   2
```

For a single fit,
[`fit()`](https://generics.r-lib.org/reference/fit.html) stores a
`p x 2` edge-selection mask with `pos` and `neg` columns by default.
This is useful for inspecting the selected network, but the performance
reported by [`summary()`](https://rdrr.io/r/base/summary.html) is still
in-sample.

## Cross-Validated Resampling with `fit_resamples()`

Use
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
on the same
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
object for an out-of-sample estimate of predictive performance.

``` r
resample_obj <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5
)

resample_obj
#> CPM resample results:
#>   Call: fit_resamples(object = cpm_spec(), conmat = conmat, behav = behav, 
#>     covariates = covariates, kfolds = 5)
#>   Number of folds: 5
#>   Number of observations: 80
#>   Edge storage: none
#>   Mean correlations:
#>     Combined: -0.075
#>     Positive: 0.029
#>     Negative: -0.181
summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: -0.075 (SE 0.046)
#>     Positive: 0.029 (SE 0.054)
#>     Negative: -0.181 (SE 0.099)
```

This keeps CPM-specific steps such as covariate handling, edge
selection, and model training inside each resample fold.

## Inspect Predictions and Edges

Native resampling results always keep raw observation-level predictions
on the object, while [`summary()`](https://rdrr.io/r/base/summary.html)
derives fold-level performance when you need an aggregate view.

``` r
predictions <- resample_obj$predictions

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: -0.075 (SE 0.046)
#>     Positive: 0.029 (SE 0.054)
#>     Negative: -0.181 (SE 0.099)
head(predictions)
#>   row fold       real         both          pos           neg
#> 1   1    5 -0.6339456  0.342534590  0.342534590  1.994932e-17
#> 2   2    1  0.4116546 -0.478738601  0.028245399 -5.179443e-01
#> 3   3    1  0.2898583 -0.032769603  0.073529511 -1.070810e-01
#> 4   4    3  0.7870086 -0.001288122 -0.001288122  2.428613e-17
#> 5   5    5  0.1169538 -0.023682986 -0.023682986  1.994932e-17
#> 6   6    3  0.7875511  0.255961736  0.255961736  2.428613e-17
```

By default,
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
uses `return_edges = "none"` and skips edge storage. This keeps the
resampling object light when you only need predictive performance.

If you also want fold-aggregated edge selection rates, request them
explicitly:

``` r
edge_resample_obj <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5,
  return_edges = "sum"
)

dim(edge_resample_obj$edges)
#> [1] 200   2
```

`predictions` returns one row per original observation. If
`na_action = "exclude"` removed subjects before fitting, those rows are
still present and their `fold` value is `NA`.

When `return_edges = "sum"`, `cpmr` stores fold-summed edge counts for
each edge. If memory matters, keep the default `return_edges = "none"`
or use `return_edges = "all"` only when fold-wise edge arrays are truly
needed.

## Custom Resamples

If you already have a partition scheme, pass it through `resamples`.

``` r
custom_resamples <- split(
  seq_len(n),
  cut(seq_len(n), breaks = 4, labels = FALSE)
)

custom_obj <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  resamples = custom_resamples,
  return_edges = "none"
)

summary(custom_obj)
#> CPM resample summary:
#>   Number of folds: 4
#>   Performance:
#>     Combined: 0.158 (SE 0.121)
#>     Positive: 0.247 (SE 0.129)
#>     Negative: 0.026 (SE 0.113)
```

Custom resamples must:

- be supplied as assessment-row indices;
- cover every complete-case subject exactly once;
- not overlap across folds.

When `na_action = "exclude"`, those indices still refer to original row
numbers of the complete-case subjects kept in the analysis.

## Missing-Data Handling

Set `na_action = "exclude"` if you want `cpmr` to fit on complete cases
while preserving original row positions in the outputs.

``` r
behav_with_na <- behav
behav_with_na[c(3, 11)] <- NA_real_

na_obj <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav_with_na,
  kfolds = 5,
  na_action = "exclude"
)

na_predictions <- na_obj$predictions
na_predictions[na_predictions$row %in% c(3, 11), ]
#>    row fold real both pos neg
#> 3    3   NA   NA   NA  NA  NA
#> 11  11   NA   NA   NA  NA  NA
```

This behavior matters when you need to merge predictions back to
subject-level metadata after resampling.

## Next Steps

After you are comfortable with the native path:

- read the workflow-selection article for guidance on how to use the
  native
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  workflow in different settings;
- read the leakage-focused article if you need a more detailed covariate
  handling example;
- keep a
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  object around when you want an explicit reusable parameter object.
