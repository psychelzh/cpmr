# Extract resampling metrics from a `cpm_resamples` object

Extract resampling metrics from a `cpm_resamples` object

## Usage

``` r
resample_metrics(
  x,
  level = c("foldwise", "pooled"),
  metrics = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
)
```

## Arguments

- x:

  A `cpm_resamples` object.

- level:

  Which level of metric output to return. Use `"foldwise"` for one row
  per fold, metric, and prediction stream, or `"pooled"` for one row per
  metric and prediction stream computed across all out-of-fold
  predictions.

- metrics:

  Which metrics to include. Supported values are `"rmse"`, `"mae"`, and
  `"correlation"`.

- correlation_method:

  Correlation method used when `metrics` includes `"correlation"`.

## Value

A data frame. For `level = "foldwise"`, the returned columns are `fold`,
`n_assess`, `metric`, `prediction`, and `estimate`. For
`level = "pooled"`, the returned columns are `metric`, `prediction`, and
`estimate`.

## Details

Use `resample_metrics()` when you want resampling metrics in a tabular
form for downstream inspection or plotting. Compared with
[`summary.cpm_resamples()`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md),
this helper is less opinionated: it can return pooled metrics across all
out-of-fold predictions or the raw fold-wise metrics used to build
aggregate summaries.

## Examples

``` r
withr::local_seed(123)
conmat <- matrix(rnorm(200), nrow = 20)
behav <- rowMeans(conmat[, 1:5, drop = FALSE]) + rnorm(20, sd = 0.2)
res <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav, kfolds = 4)

head(resample_metrics(res))
#>   fold n_assess metric prediction  estimate
#> 1    1        5   rmse       both 0.4106618
#> 2    1        5   rmse        pos 0.4106618
#> 3    1        5   rmse        neg 0.4106618
#> 4    2        5   rmse       both 0.5779391
#> 5    2        5   rmse        pos 0.5779391
#> 6    2        5   rmse        neg 0.6351870
resample_metrics(res, level = "pooled", metrics = "correlation")
#>        metric prediction   estimate
#> 1 correlation       both  0.2428599
#> 2 correlation        pos  0.2428599
#> 3 correlation        neg -0.1949248
```
