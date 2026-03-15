#' Direct native CPM fit helpers
#'
#' `cpm_fit()` and `cpm_fit_resamples()` provide direct native entry points for
#' the most common CPM workflows without requiring users to construct a
#' [cpm_spec()] object explicitly first.
#'
#' These helpers currently wrap the same CPM specification and core fitting
#' paths used by [fit()] and [fit_resamples()], but make the package's native
#' `conmat`/`behav` workflow more discoverable.
#'
#' @param conmat A matrix of connectome data. Observations in rows and edges in
#'   columns.
#' @param behav A numeric vector of behavioral data. Length must equal the
#'   number of rows in `conmat`.
#' @param covariates Optional covariate matrix. Observations in rows and
#'   covariates in columns.
#' @param thresh_method,thresh_level,bias_correct CPM modeling parameters passed
#'   through to [cpm_spec()].
#' @param return_edges How selected edges should be stored. For `cpm_fit()`,
#'   `"sum"` stores the selected edge mask, `"all"` stores a singleton 3D array,
#'   and `"none"` skips edge storage. For `cpm_fit_resamples()`, `"sum"` stores
#'   fold-summed edges and `"all"` stores fold-wise edge arrays.
#' @param na_action How to handle missing values. `"fail"` errors when missing
#'   values are present; `"exclude"` fits on complete cases and preserves row
#'   positions in outputs.
#' @param resamples Optional list of assessment indices for custom resampling.
#' @param kfolds Number of folds used when `resamples` is `NULL`.
#'
#' @return
#' `cpm_fit()` returns a fitted `cpm` object.
#'
#' `cpm_fit_resamples()` returns a `cpm_resamples` object.
#'
#' @examples
#' conmat <- matrix(rnorm(100), ncol = 10)
#' behav <- rnorm(10)
#'
#' fit_obj <- cpm_fit(conmat, behav)
#' res_obj <- cpm_fit_resamples(conmat, behav, kfolds = 5)
#' @export
cpm_fit <- function(
  conmat,
  behav,
  covariates = NULL,
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  spec <- cpm_spec(
    thresh_method = thresh_method,
    thresh_level = thresh_level,
    bias_correct = bias_correct
  )

  core_fit_single(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    return_edges = return_edges,
    na_action = na_action,
    call = call
  )
}

#' @rdname cpm_fit
#' @export
cpm_fit_resamples <- function(
  conmat,
  behav,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  spec <- cpm_spec(
    thresh_method = thresh_method,
    thresh_level = thresh_level,
    bias_correct = bias_correct
  )

  fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    kfolds = kfolds,
    return_edges = return_edges,
    na_action = na_action
  )
}
