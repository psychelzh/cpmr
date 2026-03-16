#' cpm_resamples Resampling Object
#'
#' A `cpm_resamples` object is returned by [fit_resamples()] and stores
#' fold-level outputs from resampling.
#'
#' @section Structure:
#' A `cpm_resamples` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for resampling.}
#'   \item{`spec`}{The originating `cpm_spec` object.}
#'   \item{`params`}{Parameter list used for the resampling run.}
#'   \item{`predictions`}{Data frame of observation-level predictions with
#'     fold IDs.}
#'   \item{`edges`}{Stored edge output based on `return_edges`
#'     (`NULL`/matrix/array).}
#'   \item{`folds`}{List of assessment-row indices for each fold.}
#' }
#'
#' @seealso [fit_resamples()], [summary.cpm_resamples()]
#' @name cpm_resamples
NULL

new_cpm_resamples <- function(
  call,
  spec,
  params,
  predictions,
  edges,
  folds
) {
  structure(
    list(
      call = call,
      spec = spec,
      params = params,
      predictions = predictions,
      edges = edges,
      folds = folds
    ),
    class = "cpm_resamples"
  )
}

#' @export
print.cpm_resamples <- function(x, ...) {
  summary_x <- summary(x)
  cat("CPM resample results:\n")
  if (!is.null(x$call)) {
    cat("  Call: ")
    print(x$call)
  }
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf("  Edge storage: %s\n", x$params$return_edges))
  print_error_block(summary_x$errors)
  invisible(x)
}

#' Summary of a `cpm_resamples` object
#'
#' @rdname summary.cpm_resamples
#' @param object An object of class `cpm_resamples`.
#' @param ... For future extension. Currently ignored.
#'
#' @return A `cpm_resamples_summary` object with the following elements:
#' \describe{
#'   \item{`errors`}{A matrix of pooled out-of-fold error metrics.}
#'   \item{`pooled_correlation`}{A named vector of pooled out-of-fold
#'     correlations for `both`, `pos`, and `neg` predictions.}
#'   \item{`foldwise_correlation`}{A matrix of fold-wise correlation means and
#'     standard errors.}
#'   \item{`edges`}{Aggregated edge-selection rates, or `NULL` when edges were
#'     not stored.}
#'   \item{`params`}{A list containing summary-relevant resampling settings.}
#' }
#' @export
summary.cpm_resamples <- function(object, ...) {
  structure(
    list(
      errors = compute_pooled_errors(object$predictions),
      pooled_correlation = compute_pooled_correlations(object$predictions),
      foldwise_correlation = summarize_fold_correlations(
        object$predictions,
        object$folds
      ),
      edges = summarize_resample_edges(
        object$edges,
        return_edges = object$params$return_edges,
        kfolds = object$params$kfolds
      ),
      params = list(
        kfolds = object$params$kfolds,
        return_edges = object$params$return_edges
      )
    ),
    class = "cpm_resamples_summary"
  )
}

#' @rdname summary.cpm_resamples
#' @param x An object of class `cpm_resamples_summary`.
#' @export
print.cpm_resamples_summary <- function(x, ...) {
  cat("CPM resample summary:\n")
  cat(sprintf("  Number of folds: %d\n", x$params$kfolds))
  print_error_block(x$errors)
  print_performance_block(
    values = x$pooled_correlation[prediction_types],
    header = "  Pooled correlations:\n"
  )
  if (
    any(stats::complete.cases(
      x$foldwise_correlation[, prediction_types, drop = FALSE]
    ))
  ) {
    print_performance_block(
      values = x$foldwise_correlation["mean", prediction_types],
      std_error = x$foldwise_correlation["std_error", prediction_types],
      header = "  Fold-wise correlations:\n"
    )
  } else {
    cat(
      paste0(
        "  Fold-wise correlations: not defined for assessment folds ",
        "smaller than 2 observations.\n"
      )
    )
  }
  print_edge_rate_block(x$edges)
  invisible(x)
}
