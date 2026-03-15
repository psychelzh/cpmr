#' Summary of a cpm object.
#'
#' This function provides a summary of a \code{cpm} object, including the
#' prediction performance and the selected edges.
#'
#' @rdname summary.cpm
#' @param object An object of class \code{cpm}.
#' @param ... Other parameters passed to the function.
#' @param method A character vector indicating the method used to calculate the
#'   correlation between the real and predicted values.
#' @return A list of class \code{cpm_summary} containing two elements:
#'   \item{performance}{A matrix of prediction performance, including the
#'     correlation between the real and predicted values for both edges,
#'     positive edges only, and negative edges only.}
#'
#'   \item{edges}{A logical matrix indicating which edges are selected by the
#'     CPM model (is `NULL` if `return_edges = FALSE`).}
#'
#'   \item{params}{A list of parameters used in the summary.}
#' @export
summary.cpm <- function(
  object,
  ...,
  method = c("pearson", "spearman")
) {
  method <- match.arg(method)
  # summary prediction performance
  performance <- matrix(
    vapply(
      colnames(object$pred),
      function(edge_type) {
        safe_cor(object$real, object$pred[, edge_type], method = method)
      },
      numeric(1)
    ),
    nrow = 1,
    dimnames = list(NULL, colnames(object$pred))
  )
  # summary edge selection
  edges <- object$edges
  structure(
    list(
      performance = performance,
      edges = edges,
      params = list(
        method = method
      )
    ),
    class = "cpm_summary"
  )
}

#' @rdname summary.cpm
#' @param x An object of class \code{cpm_summary}.
#' @export
print.cpm_summary <- function(x, ...) {
  cat("CPM summary:\n")
  cat(
    sprintf(
      "  Performance (%s):\n",
      # capitalize the first letter
      capitalize_first(x$params$method)
    )
  )
  cat(sprintf("    Positive: %s\n", format_cor(x$performance[, "pos"])))
  cat(sprintf("    Negative: %s\n", format_cor(x$performance[, "neg"])))
  cat(sprintf("    Combined: %s\n", format_cor(x$performance[, "both"])))
  if (!is.null(x$edges)) {
    cat("  Selected edges:\n")
    cat(sprintf("    Positive: %s\n", format_rate(safe_mean(x$edges[, "pos"]))))
    cat(sprintf("    Negative: %s\n", format_rate(safe_mean(x$edges[, "neg"]))))
  }
  invisible(x)
}

#' Summary of a cpm_resamples object.
#'
#' This function provides an aggregate summary of a \code{cpm_resamples} object,
#' including pooled and mean-fold prediction performance plus edge-selection
#' rates across folds when edges are stored.
#'
#' @rdname summary.cpm_resamples
#' @param object An object of class \code{cpm_resamples}.
#' @param ... Other parameters passed to the function.
#' @param method A character vector indicating the method used to calculate the
#'   correlation between the real and predicted values.
#' @param edge_level A number between 0 and 1 used to binarize edge-selection
#'   rates across folds. An edge is considered selected when its fold-wise
#'   selection rate is greater than or equal to `edge_level`.
#' @return A list of class \code{cpm_resamples_summary} containing three
#'   elements:
#'   \item{performance}{A matrix with pooled and mean-fold correlations for
#'     combined, positive, and negative predictions.}
#'   \item{edges}{A logical matrix of selected edges after applying
#'     `edge_level` to fold-wise selection rates (or `NULL` if edges were not
#'     stored).}
#'   \item{params}{A list of parameters used in the summary.}
#' @export
summary.cpm_resamples <- function(
  object,
  ...,
  method = c("pearson", "spearman"),
  edge_level = 0.5
) {
  method <- match.arg(method)
  performance <- compute_resample_performance(object$predictions, method)
  edge_rates <- compute_resample_edge_rates(
    object$edges,
    object$params$return_edges,
    length(object$folds)
  )
  edges <- binarize_resample_edges(edge_rates, edge_level)

  structure(
    list(
      performance = performance,
      edges = edges,
      params = list(
        method = method,
        return_edges = object$params$return_edges,
        n_folds = length(object$folds),
        edge_level = edge_level
      )
    ),
    class = "cpm_resamples_summary"
  )
}

#' @rdname summary.cpm_resamples
#' @param x An object of class \code{cpm_resamples_summary}.
#' @export
print.cpm_resamples_summary <- function(x, ...) {
  cat("CPM resamples summary:\n")
  cat(
    sprintf(
      "  Performance (%s):\n",
      capitalize_first(x$params$method)
    )
  )
  cat("    Pooled:\n")
  cat(sprintf(
    "      Positive: %s\n",
    format_cor(x$performance["pooled", "pos"])
  ))
  cat(sprintf(
    "      Negative: %s\n",
    format_cor(x$performance["pooled", "neg"])
  ))
  cat(sprintf(
    "      Combined: %s\n",
    format_cor(x$performance["pooled", "both"])
  ))
  cat("    Mean fold:\n")
  cat(
    sprintf(
      "      Positive: %s\n",
      format_cor(x$performance["fold_mean", "pos"])
    )
  )
  cat(
    sprintf(
      "      Negative: %s\n",
      format_cor(x$performance["fold_mean", "neg"])
    )
  )
  cat(
    sprintf(
      "      Combined: %s\n",
      format_cor(x$performance["fold_mean", "both"])
    )
  )

  if (!is.null(x$edges)) {
    cat(
      sprintf(
        "  Selected edges (edge_level = %s):\n",
        format_cor(x$params$edge_level)
      )
    )
    cat(sprintf("    Positive: %s\n", format_rate(safe_mean(x$edges[, "pos"]))))
    cat(sprintf("    Negative: %s\n", format_rate(safe_mean(x$edges[, "neg"]))))
  }
  invisible(x)
}

compute_resample_performance <- function(predictions, method) {
  assessed <- !is.na(predictions$fold)
  fold_ids <- sort(unique(predictions$fold[assessed]))

  pooled <- vapply(
    inc_edges,
    function(edge_type) {
      safe_cor(
        predictions$real[assessed],
        predictions[[edge_type]][assessed],
        method = method
      )
    },
    numeric(1)
  )

  fold_mean <- vapply(
    inc_edges,
    function(edge_type) {
      cor_by_fold <- vapply(
        fold_ids,
        function(fold_id) {
          rows <- predictions$fold == fold_id
          safe_cor(
            predictions$real[rows],
            predictions[[edge_type]][rows],
            method = method
          )
        },
        numeric(1)
      )
      safe_mean(cor_by_fold)
    },
    numeric(1)
  )

  matrix(
    c(pooled, fold_mean),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("pooled", "fold_mean"), inc_edges)
  )
}

compute_resample_edge_rates <- function(edges, return_edges, kfolds) {
  if (return_edges == "none" || is.null(edges)) {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(edges / kfolds)
  }

  apply(edges, c(1, 2), mean)
}

binarize_resample_edges <- function(edge_rates, edge_level) {
  if (is.null(edge_rates)) {
    return(NULL)
  }

  edge_rates >= edge_level
}

safe_mean <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

capitalize_first <- function(x) {
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

format_cor <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
}
