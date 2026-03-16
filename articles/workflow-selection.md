# Choosing a Workflow in cpmr

## Overview

`cpmr` supports more than one way to express a CPM analysis, but they do
not all serve the same purpose.

The package is organized around a core-first design:

- the native `cpmr` path is the primary workflow for fitting,
  resampling, and future tuning extensions;
- specification-driven helpers such as
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  remain useful when you want explicit parameter objects.

## Recommended Path: Native Helpers

For most analyses, start with
[`cpm_fit()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md) and
[`cpm_fit_resamples()`](https://psychelzh.github.io/cpmr/reference/cpm_fit.md).

``` r
library(cpmr)

set.seed(123)
conmat <- matrix(rnorm(80 * 200), nrow = 80)
behav <- rnorm(80)

fit_obj <- cpm_fit(conmat, behav)
resample_obj <- cpm_fit_resamples(conmat, behav, kfolds = 5)

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
#>   Selected edges:
#>     Positive: 0.40%
#>     Negative: 0.80%
```

This path is the best fit when you care about:

- large edge spaces;
- repeated resampling;
- leakage-safe covariate handling;
- future CPM-specific performance improvements such as fold-level reuse.

## Lower-Level Native Path: `cpm_spec()`

If you want an explicit specification object, you can still work with
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
plus [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).

``` r
spec <- cpm_spec(thresh_method = "sparsity", thresh_level = 0.2)

fit_via_spec <- fit(spec, conmat = conmat, behav = behav)
resamples_via_spec <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)
```

This route remains native `cpmr`; it is just a slightly lower-level
interface.

## Practical Rule of Thumb

Choose the native path when you need speed, explicit CPM behavior, or
resampling-heavy experiments. Choose the
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
path when you want an explicit specification object without leaving the
native API surface.
