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
#'   \item{`metrics`}{Data frame of fold-level performance metrics.}
#' }
#'
#' @seealso [fit_resamples()], [collect_metrics()], [collect_predictions()],
#'   [collect_edges()]
#' @name cpm_resamples
NULL

new_cpm_resamples <- function(
  call,
  spec,
  params,
  predictions,
  edges,
  folds,
  metrics
) {
  structure(
    list(
      call = call,
      spec = spec,
      params = params,
      predictions = predictions,
      edges = edges,
      folds = folds,
      metrics = metrics
    ),
    class = "cpm_resamples"
  )
}

#' @export
print.cpm_resamples <- function(x, ...) {
  cat("CPM resample results:\n")
  if (!is.null(x$call)) {
    cat("  Call: ")
    print(x$call)
  }
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf("  Edge storage: %s\n", x$params$return_edges))
  cat("  Mean correlations:\n")
  cat(sprintf("    Both: %.3f\n", safe_mean(x$metrics$both)))
  cat(sprintf("    Pos:  %.3f\n", safe_mean(x$metrics$pos)))
  cat(sprintf("    Neg:  %.3f\n", safe_mean(x$metrics$neg)))
  invisible(x)
}

#' Collect fold-level metrics from CPM resamples
#'
#' @param x A `cpm_resamples` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A [tibble][tibble::tibble-package] with one row per fold.
#' @export
collect_metrics.cpm_resamples <- function(x, ...) {
  tibble::as_tibble(x$metrics)
}

#' Collect observation-level predictions from CPM resamples
#'
#' @param x A `cpm_resamples` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A [tibble][tibble::tibble-package] with one row per observation.
#' @export
collect_predictions.cpm_resamples <- function(x, ...) {
  tibble::as_tibble(x$predictions)
}

#' Collect selected edges from CPM resamples
#'
#' @param x A `cpm_resamples` object.
#' @param format Output format for edges. Use `"raw"` to return the stored
#'   matrix/array directly. Use `"index"` to return sparse edge indices.
#' @param ... For future extension. Currently ignored.
#'
#' @return A matrix for `return_edges = "sum"`, a 3D array for
#'   `return_edges = "all"`, or `NULL` for `return_edges = "none"` when
#'   `format = "raw"`. For `format = "index"`, returns sparse edge indices.
#' @export
collect_edges.cpm_resamples <- function(x, format = c("raw", "index"), ...) {
  format <- match.arg(format)

  if (format == "raw") {
    return(x$edges)
  }

  edges_to_index(x$edges, x$params$return_edges)
}

edges_to_index <- function(edges, return_edges) {
  if (return_edges == "none" || is.null(edges)) {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(list(
      pos = which(edges[, "pos"] > 0),
      neg = which(edges[, "neg"] > 0)
    ))
  }

  lapply(seq_len(dim(edges)[3]), function(fold) {
    list(
      fold = fold,
      pos = which(edges[, "pos", fold]),
      neg = which(edges[, "neg", fold])
    )
  })
}

compute_fold_metrics <- function(real, pred, folds) {
  fold_metrics <- lapply(seq_along(folds), function(i) {
    rows <- folds[[i]]
    data.frame(
      fold = i,
      n_assess = length(rows),
      both = safe_cor(real[rows], pred[rows, "both"]),
      pos = safe_cor(real[rows], pred[rows, "pos"]),
      neg = safe_cor(real[rows], pred[rows, "neg"])
    )
  })
  do.call(rbind, fold_metrics)
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
