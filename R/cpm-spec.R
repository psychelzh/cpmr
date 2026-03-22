#' Define a CPM model specification
#'
#' Create a lightweight specification object that stores the modeling
#' parameters required to fit a connectome-based predictive model later with
#' [fit()] or [fit_resamples()].
#'
#' `cpm_spec()` keeps the main CPM decisions visible at the top level while
#' grouping stage-specific settings into helpers:
#'
#' - [cpm_selection_cor()] configures the current correlation-based edge
#'   selection path.
#' - [cpm_construction_summary()] configures the current summary-construction
#'   path.
#' - [cpm_model_lm()] defines the outcome model fitted on CPM-derived features.
#'
#' @param selection Selection helper created by [cpm_selection_cor()].
#' @param construction Construction helper created by
#'   [cpm_construction_summary()].
#' @param model Outcome-model helper created by [cpm_model_lm()]. This stage
#'   maps CPM-derived subject-level features to the behavioral outcome.
#'
#' @examples
#' spec <- cpm_spec(
#'   selection = cpm_selection_cor(
#'     method = "spearman",
#'     criterion = "absolute",
#'     level = 0.1
#'   ),
#'   construction = cpm_construction_summary(
#'     polarity = "net",
#'     weight_scale = 0.03
#'   ),
#'   model = cpm_model_lm()
#' )
#' spec
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' fit_obj <- fit(spec, conmat = conmat, behav = behav)
#' summary(fit_obj)
#'
#' resample_obj <- fit_resamples(spec, conmat = conmat, behav = behav, resamples = 5)
#' summary(resample_obj)
#' @export
cpm_spec <- function(
  selection = cpm_selection_cor(),
  construction = cpm_construction_summary(),
  model = cpm_model_lm()
) {
  selection <- validate_selection_spec(selection)
  construction <- validate_construction_spec(construction)
  model <- validate_model_spec(model)

  new_cpm_spec(
    params = list(
      selection = selection,
      construction = construction,
      model = model
    )
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM specification:\n")
  print_staged_settings(
    selection = x$params$selection,
    construction = x$params$construction,
    model = x$params$model,
    prediction_streams = construction_prediction_streams(x$params$construction),
    headers = list(
      selection = "  Selection:\n",
      construction = "  Construction:\n",
      model = "  Model:\n"
    )
  )
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
#' @param resamples Optional resampling specification. Use `NULL` for leave-one-
#'   out resampling, a single integer greater than or equal to 2 to request
#'   k-fold resampling, or a list of assessment indices for manual folds. Manual
#'   resamples must contain integer vectors indexing rows in `conmat`.
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
    list(
      params = params
    ),
    class = "cpm_spec"
  )
}
