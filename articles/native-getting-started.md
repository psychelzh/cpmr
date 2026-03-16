# Native Getting Started with cpmr

## Overview

`cpmr`’s primary workflow is a native, matrix-first API built around
[`cpm_fit()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md) and
[`cpm_fit_resamples()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md).

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

## Single Fit with `cpm_fit()`

Use [`cpm_fit()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md)
when you want a direct native fit without constructing a
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
object first.

``` r
fit_obj <- cpm_fit(
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  thresh_method = "alpha",
  thresh_level = 0.05
)

fit_obj
#> CPM results:
#>   Call: cpm_fit(conmat = conmat, behav = behav, covariates = covariates, 
#>     thresh_method = "alpha", thresh_level = 0.05)
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
[`cpm_fit()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md)
stores a `p x 2` edge-selection mask with `pos` and `neg` columns by
default. This is useful for inspecting the selected network, but the
performance reported by
[`summary()`](https://rdrr.io/r/base/summary.html) is still in-sample.

## Cross-Validated Resampling with `cpm_fit_resamples()`

Use
[`cpm_fit_resamples()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md)
for an out-of-sample estimate of predictive performance.

``` r
resample_obj <- cpm_fit_resamples(
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5
)

resample_obj
#> CPM resample results:
#>   Call: cpm_fit_resamples(conmat = conmat, behav = behav, covariates = covariates, 
#>     kfolds = 5)
#>   Number of folds: 5
#>   Number of observations: 80
#>   Edge storage: sum
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
#>   Selected edges:
#>     Positive: 1.00%
#>     Negative: 0.50%
```

This keeps CPM-specific steps such as covariate handling, edge
selection, and model training inside each resample fold.

## Inspect Predictions and Edges

Native resampling results keep raw observation-level outputs directly on
the object, while [`summary()`](https://rdrr.io/r/base/summary.html)
derives fold-level performance when you need an aggregate view.

``` r
predictions <- resample_obj$predictions
edges <- resample_obj$edges

summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: -0.075 (SE 0.046)
#>     Positive: 0.029 (SE 0.054)
#>     Negative: -0.181 (SE 0.099)
#>   Selected edges:
#>     Positive: 1.00%
#>     Negative: 0.50%
head(predictions)
#>   row fold       real         both          pos           neg
#> 1   1    5 -0.6339456  0.342534590  0.342534590  8.673617e-18
#> 2   2    1  0.4116546 -0.478738601  0.028245399 -5.179443e-01
#> 3   3    1  0.2898583 -0.032769603  0.073529511 -1.070810e-01
#> 4   4    3  0.7870086 -0.001288122 -0.001288122 -3.469447e-18
#> 5   5    5  0.1169538 -0.023682986 -0.023682986  8.673617e-18
#> 6   6    3  0.7875511  0.255961736  0.255961736 -3.469447e-18
dim(edges)
#> [1] 200   2
```

`predictions` returns one row per original observation. If
`na_action = "exclude"` removed subjects before fitting, those rows are
still present and their `fold` value is `NA`.

For resampling, the default `return_edges = "sum"` stores fold-summed
counts for each edge. If memory matters, use `return_edges = "none"` to
skip edge storage entirely.

## Custom Resamples

If you already have a partition scheme, pass it through `resamples`.

``` r
custom_resamples <- split(
  seq_len(n),
  cut(seq_len(n), breaks = 4, labels = FALSE)
)

custom_obj <- cpm_fit_resamples(
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

na_obj <- cpm_fit_resamples(
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

- read the workflow-selection article for guidance on when to use native
  APIs versus lower-level specification objects;
- read the leakage-focused article if you need a more detailed covariate
  handling example;
- use
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  plus [`fit()`](https://generics.r-lib.org/reference/fit.html) /
  [`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
  when you want an explicit reusable parameter object.
