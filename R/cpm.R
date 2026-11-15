#' cpm object
#'
#' A `cpm` object is the primary result returned by [cpm()].
#'
#' @section Structure:
#' A `cpm` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for the CPM run.}
#'   \item{`spec`}{The originating CPM specification object.}
#'   \item{`settings`}{Runtime settings used for the CPM run, including
#'     missing-data handling and edge-storage preferences.}
#'   \item{`predictions`}{Data frame of observation-level predictions with
#'     fold IDs and one column per configured prediction stream.}
#'   \item{`edges`}{Stored edge output based on `return_edges`
#'     (`NULL`/matrix/array).}
#'   \item{`folds`}{List of assessment-row indices for each fold.}
#' }
#'
#' @seealso [cpm()], [summary.cpm()], [tidy.cpm()]
#' @name cpm_object
NULL

#' Run CPM
#'
#' `cpm()` is the primary CPM workflow. It applies a CPM specification under
#' the requested assessment plan and returns a `cpm` object with out-of-fold
#' predictions.
#'
#' @rdname spec
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column.
#' @param behav A numeric outcome vector with one value per observation in
#'   `conmat`. Row or column matrices are accepted and converted with [drop()].
#' @param spec A CPM specification created by [spec()]. If `NULL`, [spec()] is
#'   used.
#' @param ... Reserved for future extensions; must be empty.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Vectors are converted to
#'   single-column matrices.
#' @param resamples Assessment-plan specification. Defaults to 10-fold
#'   assessment. Use `NULL` for leave-one-out assessment, a single integer
#'   greater than or equal to 2 to request k-fold assessment, or a list of
#'   assessment indices for manual folds. Manual `resamples` must contain
#'   integer vectors indexing rows in `conmat`. Ten-fold assessment needs at
#'   least 10 complete cases; around 30 cases are a rough minimum for having
#'   at least 3 observations in each assessment fold, not a guarantee of
#'   stable correlations.
#' @param return_edges A character string indicating the return value of the
#'   selected edges. `"sum"` stores fold counts for each selected edge,
#'   `"none"` skips edge storage, and `"all"` stores the fold-wise edge masks.
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
#' assessment folds, and optional stored edges. Call [summary.cpm()] for
#' prediction errors and correlation summaries, or [tidy.cpm()] for their
#' pooled or fold-wise tables.
#' @export
cpm <- function(
  conmat,
  behav,
  spec = NULL,
  ...,
  covariates = NULL,
  resamples = 10L,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()

  if (...length() > 0L) {
    stop(
      paste0(
        "Unused arguments in `...` are not supported. Use `covariates` ",
        "instead of `confounds`, `resamples` instead of `kfolds`, and ",
        "`spec()` for selection and construction settings."
      ),
      call. = FALSE
    )
  }

  if (is.null(spec)) {
    spec <- spec()
  }
  if (!inherits(spec, "cpm_spec")) {
    stop(
      "`spec` must be a CPM specification created by `spec()`.",
      call. = FALSE
    )
  }

  run_cpm_workflow(
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

#' @export
print.cpm <- function(x, ...) {
  edge_storage <- switch(
    x$settings$return_edges,
    none = "not stored",
    sum = "summed across folds",
    all = "stored for each fold"
  )
  selection_summary <- paste(
    x$spec$selection$method,
    x$spec$selection$criterion,
    format_threshold_level(x$spec$selection$level),
    sep = " / "
  )
  construction_summary <- paste(
    x$spec$construction$sign_mode,
    if (x$spec$construction$weight_scale == 0) {
      "no weighting"
    } else {
      paste0(
        "sigmoid weighting (",
        format_threshold_level(
          x$spec$construction$weight_scale
        ),
        ")"
      )
    },
    if (isTRUE(x$spec$construction$standardize_edges)) {
      "z-score edges"
    } else {
      "raw edges"
    },
    sep = " / "
  )
  model_summary <- switch(
    x$spec$model$type,
    lm = "linear regression",
    x$spec$model$type
  )

  cat("CPM:\n")
  if (!is.null(x$call)) {
    cat("  Call: ")
    print(x$call)
  }
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf(
    "    Complete cases: %d\n",
    sum(stats::complete.cases(x$predictions[,
      prediction_columns(x$predictions),
      drop = FALSE
    ]))
  ))
  cat("  Parameters:\n")
  print_setting_line(
    "Covariates",
    if (isTRUE(x$settings$covariates)) "included" else "none"
  )
  print_setting_line(
    "Missing data",
    x$settings$na_action
  )
  print_setting_line(
    "Edge storage",
    edge_storage
  )
  print_setting_line(
    "Selection",
    selection_summary
  )
  print_setting_line(
    "Construction",
    construction_summary
  )
  print_setting_line(
    "Model",
    model_summary
  )
  cat(
    "  Use summary() for aggregate metrics and tidy() for metric tables.\n"
  )
  invisible(x)
}
