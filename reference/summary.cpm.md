# Summarize a `cpm` object

Summarize a `cpm` object

## Usage

``` r
# S3 method for class 'cpm'
summary(object, ..., method = c("pearson", "spearman"))

# S3 method for class 'cpm_summary'
print(x, ...)
```

## Arguments

- object:

  An object of class `cpm`.

- ...:

  For future extension. Currently ignored.

- method:

  Correlation method used for correlation summaries.

- x:

  An object of class `cpm_summary`.

## Value

A `cpm_summary` object with the following elements:

- `metrics`:

  A summary-level data frame with `level`, `metric`, `prediction`,
  `estimate`, `std_error`, `method`, `n`, `n_folds`, and
  `descriptive_only` columns.

- `tables`:

  A list of pooled metric and fold-wise correlation tables for
  downstream tidying.

- `edges`:

  Aggregated edge-selection rates, or `NULL` when edges were not stored.

- `settings`:

  A list containing summary-relevant CPM settings.

## Details

`summary.cpm()` reports pooled out-of-fold RMSE and MAE, pooled
correlations, and the mean and standard error of fold-wise correlations.
Correlations require at least three valid pairs; fold-wise correlations
are unavailable for folds with fewer than three assessment observations.
With covariates, reported RMSE and MAE are on the fold-specific
residualized outcome scale, not the original behavior scale. Pooled
correlation rows include a `descriptive_only` flag when assessment folds
are too small for fold-wise correlations.
