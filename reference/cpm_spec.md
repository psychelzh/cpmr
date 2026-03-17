# Define a CPM model specification

Create a lightweight specification object that stores the modeling
parameters required to fit a connectome-based predictive model later
with [`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).

## Usage

``` r
cpm_spec(
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE
)

# S3 method for class 'cpm_spec'
fit(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  na_action = c("fail", "exclude")
)

# S3 method for class 'cpm_spec'
fit_resamples(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
)
```

## Arguments

- thresh_method, thresh_level:

  The threshold method and level used in edge selection. With `"alpha"`,
  edges are selected by thresholding the absolute correlation against a
  critical value implied by `thresh_level`. With `"sparsity"`,
  `thresh_level` is treated as a proportion and edges are selected from
  the lower and upper tails of the correlation distribution.

- bias_correct:

  Logical value indicating if the connectome data should be
  bias-corrected. If `TRUE`, the connectome data will be centered and
  scaled to have unit variance based on the training data before model
  fitting and prediction. See Rapuano et al. (2020) for more details.

- object:

  A `cpm_spec` object.

- conmat:

  A matrix of connectome data. Observations in row, edges in column.

- behav:

  A numeric outcome vector with one value per observation in `conmat`.
  Row or column matrices are accepted and converted with
  [`drop()`](https://rdrr.io/r/base/drop.html).

- ...:

  For future extension. Currently ignored.

- covariates:

  A matrix of covariates. Observations in row, variables in column. If
  `NULL`, no covariates are used. Vectors are converted to single-column
  matrices.

- na_action:

  A character string indicating the action when missing values are found
  in the inputs. `"fail"` stops immediately when any required value is
  missing. `"exclude"` fits on complete cases and keeps the original row
  layout in the returned predictions.

- resamples:

  Optional list of assessment indices defining resamples. Each element
  must be an integer vector indexing rows in `conmat`. If `NULL`, folds
  are generated from `kfolds`.

- kfolds:

  Number of folds used when `resamples` is `NULL`. If `NULL`, it is set
  to the number of complete-case observations (LOOCV).

- return_edges:

  A character string indicating the return value of the selected edges.
  `"none"` skips edge storage, `"sum"` stores fold counts for each
  selected edge, and `"all"` stores the fold-wise edge masks.

## Value

`cpm_spec()` returns a `cpm_spec` object that can be reused across calls
to [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).

[`fit()`](https://generics.r-lib.org/reference/fit.html) returns a `cpm`
object from a single in-sample fit. Single-fit CPM objects always store
the selected edge mask.

[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
returns a `cpm_resamples` object containing observation-level
predictions, resampling folds, and optional stored edges. Call
[`summary.cpm_resamples()`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md)
for the default aggregate report, or
[`resample_metrics()`](https://psychelzh.github.io/cpmr/reference/resample_metrics.md)
when you want pooled or fold-wise metrics in tabular form.

## Examples

``` r
spec <- cpm_spec(thresh_level = 0.01)
spec
#> CPM specification:
#>   Threshold method: alpha
#>   Threshold level:  0.01
#>   Bias correction:  yes

conmat <- matrix(rnorm(100 * 100), nrow = 100)
behav <- rnorm(100)
fit_obj <- fit(spec, conmat = conmat, behav = behav)
summary(fit_obj)
#> CPM summary:
#>   Performance (Pearson):
#>     Combined: 0.258
#>     Positive: 0.258
#>     Negative: NA
#>   Selected edges:
#>     Positive: 1.00%
#>     Negative: 0.00%

resample_obj <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)
summary(resample_obj)
#> CPM resample summary:
#>   Number of folds: 5
#>   Prediction error:
#>     RMSE:
#>       Combined: 1.078
#>       Positive: 1.078
#>       Negative: 1.126
#>     MAE:
#>       Combined: 0.818
#>       Positive: 0.818
#>       Negative: 0.836
#>   Pooled correlations (Pearson):
#>     Combined: 0.036
#>     Positive: 0.036
#>     Negative: -0.205
#>   Fold-wise correlations (Pearson):
#>     Combined: 0.095
#>     Positive: 0.095
#>     Negative: -0.456
```
