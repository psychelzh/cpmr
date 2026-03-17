# Summarize a `cpm_resamples` object

Summarize a `cpm_resamples` object

## Usage

``` r
# S3 method for class 'cpm_resamples'
summary(object, ...)

# S3 method for class 'cpm_resamples_summary'
print(x, ...)
```

## Arguments

- object:

  An object of class `cpm_resamples`.

- ...:

  For future extension. Currently ignored.

- x:

  An object of class `cpm_resamples_summary`.

## Value

A `cpm_resamples_summary` object with the following elements:

- `metrics`:

  A data frame with columns `level`, `metric`, `prediction`, `estimate`,
  `std_error`, and `method`. Resample summaries store pooled errors and
  pooled correlations at `level = "pooled"`, and fold-wise correlation
  summaries at `level = "foldwise"`.

- `edges`:

  Aggregated edge-selection rates, or `NULL` when edges were not stored.

- `params`:

  A list containing summary-relevant resampling settings.

## Details

`summary.cpm_resamples()` is designed to give a compact default report.
It leads with pooled out-of-fold error metrics (`RMSE` and `MAE`), then
reports pooled and fold-wise correlations as supplementary statistics.
This keeps the default summary usable even when fold-wise correlations
are undefined for some resampling schemes, such as leave-one-out
resampling.

## Examples

``` r
withr::local_seed(123)
conmat <- matrix(rnorm(200), nrow = 20)
behav <- rowMeans(conmat[, 1:5, drop = FALSE]) + rnorm(20, sd = 0.2)
res <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav, kfolds = 4)

summary(res)
#> CPM resample summary:
#>   Number of folds: 4
#>   Prediction error:
#>     RMSE:
#>       Combined: 0.432
#>       Positive: 0.432
#>       Negative: 0.452
#>     MAE:
#>       Combined: 0.365
#>       Positive: 0.365
#>       Negative: 0.358
#>   Pooled correlations (Pearson):
#>     Combined: 0.243
#>     Positive: 0.243
#>     Negative: -0.195
#>   Fold-wise correlations (Pearson):
#>     Combined: 0.454
#>     Positive: 0.454
#>     Negative: NA
```
