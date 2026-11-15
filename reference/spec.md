# Define a CPM specification

Create a lightweight specification object that stores the staged
decisions required to run connectome-based predictive modeling later
with `cpm()`.

`cpm()` is the primary CPM workflow. It applies a CPM specification
under the requested assessment plan and returns a `cpm` object with
out-of-fold predictions.

## Usage

``` r
spec(
  selection = cpm_selection_cor(),
  construction = cpm_construction_strength(),
  model = cpm_model_lm()
)

cpm(
  conmat,
  behav,
  spec = NULL,
  ...,
  covariates = NULL,
  resamples = 10L,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
)
```

## Arguments

- selection:

  Selection helper created by
  [`cpm_selection_cor()`](https://psychelzh.github.io/cpmr/reference/cpm_selection_cor.md).

- construction:

  Construction helper created by
  [`cpm_construction_strength()`](https://psychelzh.github.io/cpmr/reference/cpm_construction_strength.md).

- model:

  Outcome-model helper created by
  [`cpm_model_lm()`](https://psychelzh.github.io/cpmr/reference/cpm_model_lm.md).
  This stage maps CPM-derived subject-level features to the behavioral
  outcome.

- conmat:

  A matrix of connectome data. Observations in row, edges in column.

- behav:

  A numeric outcome vector with one value per observation in `conmat`.
  Row or column matrices are accepted and converted with
  [`drop()`](https://rdrr.io/r/base/drop.html).

- spec:

  A CPM specification created by `spec()`. If `NULL`, `spec()` is used.

- ...:

  Reserved for future extensions; must be empty.

- covariates:

  A matrix of covariates. Observations in row, variables in column. If
  `NULL`, no covariates are used. Vectors are converted to single-column
  matrices.

- resamples:

  Assessment-plan specification. Defaults to 10-fold assessment. Use
  `NULL` for leave-one-out assessment, a single integer greater than or
  equal to 2 to request k-fold assessment, or a list of assessment
  indices for manual folds. Manual `resamples` must contain integer
  vectors indexing rows in `conmat`. Ten-fold assessment needs at least
  10 complete cases; around 30 cases are a rough minimum for having at
  least 3 observations in each assessment fold, not a guarantee of
  stable correlations.

- return_edges:

  A character string indicating the return value of the selected edges.
  `"sum"` stores fold counts for each selected edge, `"none"` skips edge
  storage, and `"all"` stores the fold-wise edge masks.

- na_action:

  A character string indicating the action when missing values are found
  in the inputs. `"fail"` stops immediately when any required value is
  missing. `"exclude"` fits on complete cases and keeps the original row
  layout in the returned predictions.

## Value

`spec()` returns a CPM specification object that can be reused across
CPM runs.

`cpm()` returns a `cpm` object containing observation-level predictions,
assessment folds, and optional stored edges. Call
[`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md)
for prediction errors and correlation summaries, or
[`tidy.cpm()`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md)
for their pooled or fold-wise tables.

## Details

`spec()` keeps the main CPM decisions visible at the top level while
grouping stage-specific settings into helpers.

`spec()` groups CPM settings into three stages:

- [`cpm_selection_cor()`](https://psychelzh.github.io/cpmr/reference/cpm_selection_cor.md)
  configures the current correlation-based edge selection path.

- [`cpm_construction_strength()`](https://psychelzh.github.io/cpmr/reference/cpm_construction_strength.md)
  configures the current strength-construction path.

- [`cpm_model_lm()`](https://psychelzh.github.io/cpmr/reference/cpm_model_lm.md)
  defines the outcome model fitted on CPM-derived features.

## Examples

``` r
s <- spec(
  selection = cpm_selection_cor(
    method = "spearman",
    criterion = "absolute",
    level = 0.1
  ),
  construction = cpm_construction_strength(
    sign_mode = "net",
    weight_scale = 0.03
  ),
  model = cpm_model_lm()
)
s
#> CPM specification:
#>   Selection:
#>     Method:                spearman
#>     Criterion:             absolute
#>     Level:                 0.1
#>   Construction:
#>     Sign mode:             net
#>     Edge weighting:        sigmoid
#>     Weight scale:          0.03
#>     Edge standardization:  none
#>   Model:
#>     Outcome model:         linear regression

conmat <- matrix(rnorm(100 * 100), nrow = 100)
behav <- rnorm(100)
result <- cpm(
  conmat = conmat,
  behav = behav,
  spec = s,
  resamples = 5
)
summary(result)
#> CPM summary:
#>   Number of folds: 5
#>   Pooled correlations (Pearson):
#>     Net: 0.007
#>   Fold-wise correlations (Pearson):
#>     Net: 0.020 (SE 0.094) (5/5 folds)
#>   Pooled RMSE:
#>     Net: 1.190
#>   Pooled MAE:
#>     Net: 0.883
#>   Selected edges:
#>     Positive: 22.40%
#>     Negative: 15.60%
```
