# Define a CPM model specification

Create a lightweight specification object that stores the modeling
parameters required to fit a connectome-based predictive model later
with [`fit()`](https://generics.r-lib.org/reference/fit.html).

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
  return_edges = c("sum", "none", "all"),
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

  The threshold method and level used in edge selection. If method is
  set to be `"alpha"`, the edge selection is based on the critical value
  of correlation coefficient. If method is set to be `"sparsity"`, the
  edge selection is based on the quantile of correlation coefficient,
  thus network sparsity is controlled.

- bias_correct:

  Logical value indicating if the connectome data should be
  bias-corrected. If `TRUE`, the connectome data will be centered and
  scaled to have unit variance based on the training data before model
  fitting and prediction. See Rapuano et al. (2020) for more details.

- object:

  A `cpm_spec` object.

- conmat:

  A matrix of connectome data. Observations in row, edges in column
  (assumed that duplicated edges are removed).

- behav:

  A numeric vector contains behavior data. Length must equal to number
  of observations in `conmat`. Note `behav` could also be a row/column
  matrix, which will be converted to a vector using
  [`drop()`](https://rdrr.io/r/base/drop.html).

- ...:

  For future extension. Currently ignored.

- covariates:

  A matrix of covariates. Observations in row, variables in column. If
  `NULL`, no covariates are used. Note if a vector is provided, it will
  be converted to a column matrix.

- return_edges:

  A character string indicating the return value of the selected edges.
  If `"none"`, no edges are returned/stored. If `"sum"`, edge masks are
  returned for single-fit and summed across folds for resampling. If
  `"all"`, single-fit stores a 3D array with a singleton third dimension
  while resampling stores fold-wise edge arrays.

- na_action:

  A character string indicating the action when missing values are found
  in `behav`. If `"fail"`, an error will be thrown. If `"exclude"`,
  missing values will be excluded from the analysis but kept in the
  output.

- resamples:

  Optional list of assessment indices defining resamples. Each element
  must be an integer vector indexing rows in `conmat`. If `NULL`, folds
  are generated from `kfolds`.

- kfolds:

  Number of folds used when `resamples` is `NULL`. If `NULL`, it is set
  to the number of complete-case observations (LOOCV).

## Value

A `cpm_spec` object storing parameters for later fitting.

A fitted `cpm` object from a single in-sample fit.

A `cpm_resamples` object containing fold-level metrics and
observation-level predictions.

## Examples

``` r
spec <- cpm_spec(thresh_level = 0.01)
spec
#> CPM model specification:
#>   Threshold method: alpha
#>   Threshold level:  0.01
#>   Bias correction:  TRUE

conmat <- matrix(rnorm(100 * 100), nrow = 100)
behav <- rnorm(100)
fit(spec, conmat = conmat, behav = behav)
#> CPM results:
#>   Call: fit(object = spec, conmat = conmat, behav = behav)
#>   Number of observations: 100
#>     Complete cases: 100
#>   Number of edges: 100
#>   Parameters:
#>     Covariates:       FALSE
#>     Threshold method: alpha
#>     Threshold level:  0.01
#>     Stored splits:    1
#>     Bias correction:  TRUE
```
