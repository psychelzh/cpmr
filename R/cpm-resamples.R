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
  metrics <- compute_fold_metrics(x$predictions, x$folds)
  cat("CPM resample results:\n")
  if (!is.null(x$call)) {
    cat("  Call: ")
    print(x$call)
  }
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf("  Edge storage: %s\n", x$params$return_edges))
  print_performance_block(
    values = vapply(
      prediction_types,
      function(prediction_type) safe_mean(metrics[[prediction_type]]),
      numeric(1)
    ),
    header = "  Mean correlations:\n"
  )
  invisible(x)
}

#' Summary of a `cpm_resamples` object
#'
#' @rdname summary.cpm_resamples
#' @param object An object of class `cpm_resamples`.
#' @param ... For future extension. Currently ignored.
#'
#' @return A `cpm_resamples_summary` object with aggregated fold-level
#'   performance and edge-selection rates.
#' @export
summary.cpm_resamples <- function(object, ...) {
  metrics <- compute_fold_metrics(object$predictions, object$folds)
  performance <- rbind(
    mean = vapply(
      prediction_types,
      function(edge_type) safe_mean(metrics[[edge_type]]),
      numeric(1)
    ),
    std_error = vapply(
      prediction_types,
      function(edge_type) safe_std_error(metrics[[edge_type]]),
      numeric(1)
    )
  )
  colnames(performance) <- prediction_types

  structure(
    list(
      performance = performance,
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
  print_performance_block(
    values = x$performance["mean", prediction_types],
    std_error = x$performance["std_error", prediction_types],
    header = "  Performance:\n"
  )
  print_edge_rate_block(x$edges)
  invisible(x)
}

compute_fold_metrics <- function(predictions, folds) {
  fold_metrics <- lapply(seq_along(folds), function(i) {
    rows <- folds[[i]]
    data.frame(
      fold = i,
      n_assess = length(rows),
      both = safe_cor(predictions$real[rows], predictions$both[rows]),
      pos = safe_cor(predictions$real[rows], predictions$pos[rows]),
      neg = safe_cor(predictions$real[rows], predictions$neg[rows])
    )
  })
  do.call(rbind, fold_metrics)
}

safe_std_error <- function(x) {
  valid <- x[stats::complete.cases(x)]
  if (length(valid) < 2L) {
    return(NA_real_)
  }

  stats::sd(valid) / sqrt(length(valid))
}

summarize_resample_edges <- function(edges, return_edges, kfolds) {
  if (is.null(edges) || return_edges == "none") {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(edges / kfolds)
  }

  apply(edges, c(1, 2), mean)
}

prediction_row_names <- function(real) {
  if (!is.null(names(real)) && !anyDuplicated(names(real))) {
    names(real)
  } else {
    NULL
  }
}

new_predictions <- function(real, pred, fold = NULL) {
  predictions <- data.frame(
    row = seq_along(real),
    real = real,
    both = pred[, "both"],
    pos = pred[, "pos"],
    neg = pred[, "neg"],
    row.names = prediction_row_names(real)
  )

  if (is.null(fold)) {
    return(predictions)
  }

  predictions$fold <- fold
  predictions[, c("row", "fold", "real", prediction_types), drop = FALSE]
}

compute_single_predictions <- function(real, pred) {
  new_predictions(real, pred)
}

compute_fold_predictions <- function(real, pred, folds) {
  fold_id <- rep(NA_integer_, length(real))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  new_predictions(real, pred, fold = fold_id)
}
