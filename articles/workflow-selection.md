# Choosing a Workflow in cpmr

## Overview

`cpmr` is organized around one native workflow: create a
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
object, then use
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
depending on the question you want to answer.

The main choice is not between different API families. It is between:

- [`fit()`](https://generics.r-lib.org/reference/fit.html) for a single
  in-sample CPM fit;
- [`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
  for out-of-sample resampling;
- `fit_resamples(..., resamples = ...)` when you already have a
  partition scheme you want to enforce.

## Start with `cpm_spec()`

For most analyses, start with
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
and reuse it across single-fit and resampling calls.

``` r
library(cpmr)

set.seed(123)
conmat <- matrix(rnorm(80 * 200), nrow = 80)
behav <- rnorm(80)
spec <- cpm_spec()

fit_obj <- fit(spec, conmat = conmat, behav = behav)
resample_obj <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)

summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Combined: 0.508
#>     Positive: 0.300
#>     Negative: 0.430
#>   Selected edges:
#>     Positive: 0.50%
#>     Negative: 1.00%
summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Prediction error:
#>     RMSE:
#>       Combined: 1.120
#>       Positive: 1.149
#>       Negative: 1.137
#>     MAE:
#>       Combined: 0.873
#>       Positive: 0.900
#>       Negative: 0.914
#>   Pooled correlations (Pearson):
#>     Combined: -0.055
#>     Positive: -0.141
#>     Negative: -0.031
#>   Fold-wise correlations (Pearson):
#>     Combined: -0.041 (SE 0.202)
#>     Positive: -0.161 (SE 0.160)
#>     Negative: 0.025 (SE 0.194)
head(summary(resample_obj)[["metrics"]])
#>    level metric prediction  estimate std_error method
#> 1 pooled   rmse       both 1.1203509        NA   <NA>
#> 2 pooled   rmse        pos 1.1492880        NA   <NA>
#> 3 pooled   rmse        neg 1.1373148        NA   <NA>
#> 4 pooled    mae       both 0.8728802        NA   <NA>
#> 5 pooled    mae        pos 0.9000038        NA   <NA>
#> 6 pooled    mae        neg 0.9139934        NA   <NA>
```

This workflow is the best fit when you care about:

- large edge spaces;
- repeated resampling;
- leakage-safe covariate handling;
- future CPM-specific performance improvements such as fold-level reuse.

## Practical Rule of Thumb

Use [`fit()`](https://generics.r-lib.org/reference/fit.html) when you
want to inspect one fitted CPM model on the current data. Use
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
when you want an out-of-sample performance estimate or a custom
resampling design.

As a rule of thumb:

- use `summary(fit_obj)` or `tidy(fit_obj)` for single-fit summaries;
- use `summary(resample_obj)` for the default aggregate resampling
  report;
- use `summary(resample_obj)[["metrics"]]` or
  `resample_metrics(resample_obj)` when you need structured metric
  tables instead of printed output.
