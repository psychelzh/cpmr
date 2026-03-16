# Direct native CPM fit helpers

`cpm_fit()` and `cpm_fit_resamples()` provide direct native entry points
for the most common CPM workflows without requiring users to construct a
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
object explicitly first.

## Usage

``` r
cpm_fit(
  conmat,
  behav,
  covariates = NULL,
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE,
  na_action = c("fail", "exclude")
)

cpm_fit_resamples(
  conmat,
  behav,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
)
```

## Arguments

- conmat:

  A matrix of connectome data. Observations in rows and edges in
  columns.

- behav:

  A numeric vector of behavioral data. Length must equal the number of
  rows in `conmat`.

- covariates:

  Optional covariate matrix. Observations in rows and covariates in
  columns.

- thresh_method, thresh_level, bias_correct:

  CPM modeling parameters passed through to
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md).

- na_action:

  How to handle missing values. `"fail"` errors when missing values are
  present; `"exclude"` fits on complete cases and preserves row
  positions in outputs.

- resamples:

  Optional list of assessment indices for custom resampling.

- kfolds:

  Number of folds used when `resamples` is `NULL`.

- return_edges:

  How selected edges should be stored for `cpm_fit_resamples()`. `"sum"`
  stores fold-summed edges, `"all"` stores fold-wise edge arrays, and
  `"none"` skips edge storage. `cpm_fit()` always stores the single-fit
  edge mask by default.

## Value

`cpm_fit()` returns a fitted `cpm` object.

`cpm_fit_resamples()` returns a `cpm_resamples` object.

## Details

These helpers currently wrap the same CPM specification and core fitting
paths used by [`fit()`](https://generics.r-lib.org/reference/fit.html)
and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md),
but make the package's native `conmat`/`behav` workflow more
discoverable.

## Examples

``` r
conmat <- matrix(rnorm(100), ncol = 10)
behav <- rnorm(10)

fit_obj <- cpm_fit(conmat, behav)
res_obj <- cpm_fit_resamples(conmat, behav, kfolds = 5)
```
