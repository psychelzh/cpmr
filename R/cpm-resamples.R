#' @export
print.cpm_resamples <- function(x, ...) {
  safe_mean <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }

  cat("CPM resample results:\n")
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

compute_fold_predictions <- function(real, pred, folds) {
  fold_id <- rep(NA_integer_, length(real))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  data.frame(
    row = seq_along(real),
    fold = fold_id,
    real = real,
    both = pred[, "both"],
    pos = pred[, "pos"],
    neg = pred[, "neg"]
  )
}

safe_cor <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }

  x <- x[valid]
  y <- y[valid]
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y)
}

new_cpm_resamples <- function(
  spec,
  folds,
  edges,
  metrics,
  predictions,
  params
) {
  structure(
    list(
      spec = spec,
      folds = folds,
      edges = edges,
      metrics = metrics,
      predictions = predictions,
      params = params
    ),
    class = "cpm_resamples"
  )
}
