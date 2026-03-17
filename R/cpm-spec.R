#' Define a CPM model specification
#'
#' Create a lightweight specification object that stores the modeling
#' parameters required to fit a connectome-based predictive model later with
#' [fit()] or [fit_resamples()].
#'
#' @param thresh_method,thresh_level The threshold method and level used in edge
#'   selection. With `"alpha"`, edges are selected by thresholding the absolute
#'   correlation against a critical value implied by `thresh_level`. With
#'   `"sparsity"`, `thresh_level` is treated as a proportion and edges are
#'   selected from the lower and upper tails of the correlation distribution.
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data before model fitting and
#'   prediction. See Rapuano et al. (2020) for more details.
#'
#' @examples
#' spec <- cpm_spec(thresh_level = 0.01)
#' spec
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' fit_obj <- fit(spec, conmat = conmat, behav = behav)
#' summary(fit_obj)
#'
#' resample_obj <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)
#' summary(resample_obj)
#' @export
cpm_spec <- function(
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE
) {
  validate_cpm_spec_params(
    thresh_level = thresh_level,
    bias_correct = bias_correct
  )

  thresh_method <- match.arg(thresh_method)

  new_cpm_spec(
    params = list(
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      bias_correct = bias_correct
    )
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM model specification:\n")
  cat(sprintf("  Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("  Threshold level:  %.2f\n", x$params$thresh_level))
  cat(sprintf("  Bias correction:  %s\n", x$params$bias_correct))
  invisible(x)
}

#' Fit a CPM model specification
#'
#' @rdname cpm_spec
#' @param object A `cpm_spec` object.
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column.
#' @param behav A numeric outcome vector with one value per observation in
#'   `conmat`. Row or column matrices are accepted and converted with [drop()].
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Vectors are converted to
#'   single-column matrices.
#' @param na_action A character string indicating the action when missing values
#'   are found in the inputs. `"fail"` stops immediately when any required value
#'   is missing. `"exclude"` fits on complete cases and keeps the original row
#'   layout in the returned predictions.
#' @export
fit.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  call[[1]] <- quote(fit)

  run_single_fit(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    call = call
  )
}

#' Fit a CPM model specification on resamples
#'
#' @rdname cpm_spec
#' @param resamples Optional list of assessment indices defining resamples.
#'   Each element must be an integer vector indexing rows in `conmat`. If
#'   `NULL`, folds are generated from `kfolds`.
#' @param kfolds Number of folds used when `resamples` is `NULL`. If `NULL`,
#'   it is set to the number of complete-case observations (LOOCV).
#' @param return_edges A character string indicating the return value of the
#'   selected edges. `"none"` skips edge storage, `"sum"` stores fold counts for
#'   each selected edge, and `"all"` stores the fold-wise edge masks.
#' @param na_action A character string indicating the action when missing values
#'   are found in the inputs. `"fail"` stops immediately when any required value
#'   is missing. `"exclude"` fits on complete cases and keeps the original row
#'   layout in the returned predictions.
#'
#' @return
#' `cpm_spec()` returns a `cpm_spec` object that can be reused across calls to
#' [fit()] and [fit_resamples()].
#'
#' `fit()` returns a `cpm` object from a single in-sample fit. Single-fit CPM
#' objects always store the selected edge mask.
#'
#' `fit_resamples()` returns a `cpm_resamples` object containing
#' observation-level predictions, resampling folds, and optional stored edges.
#' Call [summary.cpm_resamples()] for the default aggregate report, or
#' [resample_metrics()] when you want pooled or fold-wise metrics in tabular
#' form.
#' @export
fit_resamples.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  call[[1]] <- quote(fit_resamples)
  return_edges <- match.arg(return_edges)
  fit_context <- resolve_fit_context(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "resampling",
    min_cases = 2L
  )

  resolved <- resolve_resample_folds(
    resamples = resamples,
    kfolds = kfolds,
    include_cases = fit_context$include_cases
  )

  run_resample_fit(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    folds = resolved$folds,
    return_edges = return_edges,
    na_action = fit_context$na_action,
    fit_context = fit_context,
    call = call
  )
}

new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

validate_cpm_spec_params <- function(thresh_level, bias_correct) {
  if (
    !is.numeric(thresh_level) ||
      length(thresh_level) != 1L ||
      is.na(thresh_level) ||
      !is.finite(thresh_level) ||
      thresh_level < 0 ||
      thresh_level > 1
  ) {
    stop("`thresh_level` must be a single number between 0 and 1.")
  }

  if (
    !is.logical(bias_correct) ||
      length(bias_correct) != 1L ||
      is.na(bias_correct)
  ) {
    stop("`bias_correct` must be either TRUE or FALSE.")
  }

  invisible()
}
