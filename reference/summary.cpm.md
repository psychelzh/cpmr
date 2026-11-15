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

  Correlation method used for pooled and fold-wise summaries.

- x:

  An object of class `cpm_summary`.

## Value

A `cpm_summary` object with the following elements:

- `metrics`:

  A compact summary-level data frame with columns `level`, `prediction`,
  `estimate`, `std_error`, and `method`. It stores pooled correlations
  together with fold-wise correlation summaries.

- `tables`:

  A list with `pooled` and `foldwise` correlation tables for downstream
  tidying.

- `edges`:

  Aggregated edge-selection rates, or `NULL` when edges were not stored.

- `settings`:

  A list containing summary-relevant CPM settings.

## Details

`summary.cpm()` is designed to give a compact correlation-based report.
It summarizes pooled out-of-fold correlations for each prediction stream
and then reports the mean and standard error of fold-wise correlations.
