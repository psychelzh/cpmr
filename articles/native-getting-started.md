# Native Getting Started with cpmr

## Overview

`cpmr`’s primary workflow is a native, matrix-first API built around
[`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md) together
with [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md).

This vignette walks through:

- how to shape CPM inputs;
- how to run leakage-safe fold-based assessment;
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

## Run CPM with `cpm()`

Use [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) on a
[`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md) object
for an out-of-sample estimate of predictive performance.

``` r

result <- cpm(
  conmat = conmat,
  behav = behav,
  spec = spec(),
  covariates = covariates,
  resamples = 5
)

result
#> CPM:
#>   Call: cpm(conmat = conmat, behav = behav, spec = spec(), covariates = covariates, 
#>     resamples = 5)
#>   Number of folds: 5
#>   Number of observations: 80
#>     Complete cases: 80
#>   Parameters:
#>     Covariates:            included
#>     Missing data:          fail
#>     Edge storage:          summed across folds
#>     Selection:             pearson / p_value / 0.01
#>     Construction:          separate / no weighting / raw edges
#>     Model:                 linear regression
#>   Use summary() for aggregate metrics and tidy() for metric tables.
summary(result)
#> CPM summary:
#>   Number of folds: 5
#>   Pooled correlations (Pearson):
#>     Joint: -0.084
#>     Positive: 0.062
#>     Negative: -0.177
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.071 (SE 0.045) (5/5 folds)
#>     Positive: 0.030 (SE 0.055) (5/5 folds)
#>     Negative: -0.174 (SE 0.097) (3/5 folds)
#>   Pooled RMSE:
#>     Joint: 0.705
#>     Positive: 0.644
#>     Negative: 0.701
#>   Pooled MAE:
#>     Joint: 0.573
#>     Positive: 0.515
#>     Negative: 0.558
#>   Selected edges:
#>     Positive: 1.00%
#>     Negative: 0.50%
```

This keeps CPM-specific steps such as covariate handling, edge
selection, and model training inside each assessment fold.

## Inspect Predictions and Edges

CPM results keep observation-level predictions on the object, while
[`summary()`](https://rdrr.io/r/base/summary.html) reports pooled RMSE
and MAE, pooled out-of-fold correlations, and mean fold-wise
correlations. Use
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) to obtain
pooled metrics or the per-fold correlation table.

``` r

predictions <- result$predictions

summary_result <- summary(result)

summary_result
#> CPM summary:
#>   Number of folds: 5
#>   Pooled correlations (Pearson):
#>     Joint: -0.084
#>     Positive: 0.062
#>     Negative: -0.177
#>   Fold-wise correlations (Pearson):
#>     Joint: -0.071 (SE 0.045) (5/5 folds)
#>     Positive: 0.030 (SE 0.055) (5/5 folds)
#>     Negative: -0.174 (SE 0.097) (3/5 folds)
#>   Pooled RMSE:
#>     Joint: 0.705
#>     Positive: 0.644
#>     Negative: 0.701
#>   Pooled MAE:
#>     Joint: 0.573
#>     Positive: 0.515
#>     Negative: 0.558
#>   Selected edges:
#>     Positive: 1.00%
#>     Negative: 0.50%
head(summary_result[["metrics"]])
#>    level metric prediction  estimate std_error method  n n_folds
#> 1 pooled   rmse      joint 0.7050899        NA   <NA> 80      NA
#> 2 pooled    mae      joint 0.5726172        NA   <NA> 80      NA
#> 3 pooled   rmse   positive 0.6441440        NA   <NA> 80      NA
#> 4 pooled    mae   positive 0.5153804        NA   <NA> 80      NA
#> 5 pooled   rmse   negative 0.7009207        NA   <NA> 80      NA
#> 6 pooled    mae   negative 0.5581996        NA   <NA> 80      NA
#>   descriptive_only
#> 1            FALSE
#> 2            FALSE
#> 3            FALSE
#> 4            FALSE
#> 5            FALSE
#> 6            FALSE
head(predictions)
#>   row fold   observed        joint     positive      negative
#> 1   1    5 -0.6339456  0.344338065  0.344338065  8.673617e-18
#> 2   2    1  0.4116546 -0.478738601  0.028245399 -5.179443e-01
#> 3   3    1  0.2898583 -0.032769603  0.073529511 -1.070810e-01
#> 4   4    3  0.7870086  0.001199379  0.001199379  1.387779e-17
#> 5   5    5  0.1169538 -0.028386998 -0.028386998  8.673617e-18
#> 6   6    3  0.7875511  0.264960292  0.264960292  1.387779e-17
head(tidy(result, component = "metrics"))
#> # A tibble: 6 × 6
#>    fold metric      n_assess prediction estimate method 
#>   <int> <chr>          <int> <chr>         <dbl> <chr>  
#> 1     1 correlation       16 joint       -0.0488 pearson
#> 2     1 correlation       16 positive    -0.0913 pearson
#> 3     1 correlation       16 negative     0.0151 pearson
#> 4     2 correlation       16 joint       -0.142  pearson
#> 5     2 correlation       16 positive     0.168  pearson
#> 6     2 correlation       16 negative    -0.233  pearson
head(tidy(result, component = "metrics", level = "pooled"))
#> # A tibble: 6 × 6
#>   metric prediction estimate     n method descriptive_only
#>   <chr>  <chr>         <dbl> <int> <chr>  <lgl>           
#> 1 rmse   joint         0.705    80 NA     FALSE           
#> 2 mae    joint         0.573    80 NA     FALSE           
#> 3 rmse   positive      0.644    80 NA     FALSE           
#> 4 mae    positive      0.515    80 NA     FALSE           
#> 5 rmse   negative      0.701    80 NA     FALSE           
#> 6 mae    negative      0.558    80 NA     FALSE
```

These three outputs have slightly different jobs:

- `summary(result)` prints the default aggregate report;
- `summary(result)[["metrics"]]` exposes the compact summary-level
  metric table stored on the summary object;
- `tidy(result, component = "metrics")` returns the raw fold-wise metric
  table for further filtering, plotting, or export, while
  `level = "pooled"` returns pooled metrics.

By default,
[`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) uses
10-fold assessment and `return_edges = "sum"`. Ten folds require at
least 10 complete cases; around 30 cases are a rough minimum for at
least 3 observations per assessment fold, not a guarantee of stable
correlations. Smaller assessment folds trigger a warning, prioritize
RMSE and MAE, and mark pooled out-of-fold correlations as descriptive.
Explicit leave-one-out assessment is available with `resamples = NULL`.

By default,
[`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) stores
fold-aggregated edge selection counts. This makes edge summaries
immediately available in the common fold-based workflow.

If you want to control edge storage explicitly:

``` r

edge_result <- cpm(
  conmat = conmat,
  behav = behav,
  spec = spec(),
  covariates = covariates,
  resamples = 5,
  return_edges = "sum"
)

dim(edge_result$edges)
#> [1] 200   2
```

`predictions` returns one row per original observation. If
`na_action = "exclude"` removed subjects before fitting, those rows are
still present and their `fold` value is `NA`.

When `return_edges = "sum"`, `cpmr` stores fold-summed edge counts for
each edge. If memory matters, switch to `return_edges = "none"` or use
`return_edges = "all"` only when fold-wise edge arrays are truly needed.

## Custom Resamples

If you already have a partition scheme, pass it through `resamples`.

``` r

custom_resamples <- split(
  seq_len(n),
  cut(seq_len(n), breaks = 4, labels = FALSE)
)

custom_obj <- cpm(
  conmat = conmat,
  behav = behav,
  spec = spec(),
  resamples = custom_resamples,
  return_edges = "none"
)

summary(custom_obj)
#> CPM summary:
#>   Number of folds: 4
#>   Pooled correlations (Pearson):
#>     Joint: 0.143
#>     Positive: 0.169
#>     Negative: -0.003
#>   Fold-wise correlations (Pearson):
#>     Joint: 0.154 (SE 0.119) (4/4 folds)
#>     Positive: 0.240 (SE 0.129) (4/4 folds)
#>     Negative: 0.031 (SE 0.111) (3/4 folds)
#>   Pooled RMSE:
#>     Joint: 0.934
#>     Positive: 0.906
#>     Negative: 0.940
#>   Pooled MAE:
#>     Joint: 0.676
#>     Positive: 0.644
#>     Negative: 0.730
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

na_obj <- cpm(
  conmat = conmat,
  behav = behav_with_na,
  spec = spec(),
  resamples = 5,
  na_action = "exclude"
)

na_predictions <- na_obj$predictions
na_predictions[na_predictions$row %in% c(3, 11), ]
#>    row fold observed joint positive negative
#> 3    3   NA       NA    NA       NA       NA
#> 11  11   NA       NA    NA       NA       NA
```

This behavior matters when you need to merge predictions back to
subject-level metadata after assessment.

## Next Steps

After you are comfortable with the native path:

- read the leakage-focused article if you need a more detailed covariate
  handling example;
- keep a [`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md)
  object around when you want an explicit reusable parameter object.
