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
#>   Performance:
#>     Combined: -0.041 (SE 0.202)
#>     Positive: -0.161 (SE 0.160)
#>     Negative: 0.025 (SE 0.194)
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
