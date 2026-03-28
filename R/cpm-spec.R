#' Define a CPM specification
#'
#' Create a lightweight specification object that stores the staged decisions
#' required to run connectome-based predictive modeling later with [cpm()].
#'
#' `spec()` keeps the main CPM decisions visible at the top level while
#' grouping stage-specific settings into helpers.
#'
#' `spec()` groups CPM settings into three stages:
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
#' s <- spec(
#'   selection = cpm_selection_cor(
#'     method = "spearman",
#'     criterion = "absolute",
#'     level = 0.1
#'   ),
#'   construction = cpm_construction_summary(
#'     sign_mode = "net",
#'     weight_scale = 0.03
#'   ),
#'   model = cpm_model_lm()
#' )
#' s
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' resample_obj <- cpm(
#'   conmat = conmat,
#'   behav = behav,
#'   spec = s,
#'   resamples = 5
#' )
#' summary(resample_obj)
#' @export
spec <- function(
  selection = cpm_selection_cor(),
  construction = cpm_construction_summary(),
  model = cpm_model_lm()
) {
  assert_spec_class(selection, "selection", "cpm_selection_spec")
  assert_spec_class(construction, "construction", "cpm_construction_spec")
  assert_spec_class(model, "model", "cpm_model_spec")

  structure(
    list(
      selection = selection,
      construction = construction,
      model = model
    ),
    class = "cpm_spec"
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM specification:\n")
  print_staged_settings(
    selection = x$selection,
    construction = x$construction,
    model = x$model,
    headers = list(
      selection = "  Selection:\n",
      construction = "  Construction:\n",
      model = "  Model:\n"
    )
  )
  invisible(x)
}

#' Run CPM
#'
#' `cpm()` is the primary CPM workflow. It applies a CPM specification under a
#' resampling plan and returns a `cpm` object with out-of-fold predictions.
#'
#' @rdname spec
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column.
#' @param behav A numeric outcome vector with one value per observation in
#'   `conmat`. Row or column matrices are accepted and converted with [drop()].
#' @param spec A CPM specification created by [spec()]. If `NULL`, [spec()] is
#'   used.
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Vectors are converted to
#'   single-column matrices.
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
#' `spec()` returns a CPM specification object that can be reused across CPM
#' runs.
#'
#' `cpm()` returns a `cpm` object containing observation-level predictions,
#' resampling folds, and optional stored edges. Call [summary.cpm()] for the
#' default aggregate report, or [resample_metrics()] when you want pooled or
#' fold-wise metrics in tabular form.
#' @export
cpm <- function(
  conmat,
  behav,
  spec = NULL,
  ...,
  covariates = NULL,
  resamples = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()

  if (is.null(spec)) {
    spec <- spec()
  }
  if (!inherits(spec, "cpm_spec")) {
    stop(
      "`spec` must be a CPM specification created by `spec()`.",
      call. = FALSE
    )
  }

  run_resample_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = return_edges,
    na_action = na_action,
    call = call
  )
}

assert_spec_class <- function(x, arg, class) {
  if (!inherits(x, class)) {
    stop(
      sprintf("`%s` must be a `%s` object.", arg, class),
      call. = FALSE
    )
  }
}
