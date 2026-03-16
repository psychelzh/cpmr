# Choosing a Workflow in cpmr

## Overview

`cpmr` supports more than one way to express a CPM analysis, but they do
not all serve the same purpose.

The package is organized around a core-first design:

- the native `cpmr` path is the primary workflow for fitting,
  resampling, and future tuning extensions;
- [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  is the native way to make CPM parameters explicit and reusable.

## Recommended Path: `cpm_spec()`

For most analyses, start with
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
together with [`fit()`](https://generics.r-lib.org/reference/fit.html)
and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).

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
#>   Performance:
#>     Combined: -0.041 (SE 0.202)
#>     Positive: -0.161 (SE 0.160)
#>     Negative: 0.025 (SE 0.194)
```

This path is the best fit when you care about:

- large edge spaces;
- repeated resampling;
- leakage-safe covariate handling;
- future CPM-specific performance improvements such as fold-level reuse.

## Practical Rule of Thumb

Choose the native
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
path when you need speed, explicit CPM behavior, or resampling-heavy
experiments.
