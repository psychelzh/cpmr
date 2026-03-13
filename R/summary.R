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
#' @param edge_level A numeric value between 0 and 1 indicating the proportional
#'   threshold for edge selection.
#' @return A list of class \code{cpm_summary} containing two elements:
#'   \item{performance}{A matrix of prediction performance, including the
#'     correlation between the real and predicted values for both edges,
#'     positive edges only, and negative edges only.}
#'
#'   \item{edges}{A logical vector indicating whether each edge is selected
#'     based on the edge_level.}
#'
#'   \item{params}{A list of parameters used in the summary.}
#' @export
summary.cpm <- function(
  object,
  ...,
  method = c("pearson", "spearman"),
  edge_level = 0.5
) {
  method <- match.arg(method)
  # summary prediction performance
  performance <- matrix(
    vapply(
      colnames(object$pred),
      function(edge_type) {
        x <- object$real
        y <- object$pred[, edge_type]
        valid <- stats::complete.cases(x, y)
        if (sum(valid) < 2) {
          return(NA_real_)
        }
        x <- x[valid]
        y <- y[valid]
        if (stats::sd(x) == 0 || stats::sd(y) == 0) {
          return(NA_real_)
        }
        stats::cor(x, y, method = method)
      },
      numeric(1)
    ),
    nrow = 1,
    dimnames = list(NULL, colnames(object$pred))
  )
  # summary edge selection
  edges <- if (!is.null(object$edges)) {
    folds_count <- length(object$folds)
    kfolds <- if (!is.null(object$params$kfolds)) {
      min(object$params$kfolds, folds_count)
    } else {
      folds_count
    }
    edges_count <- object$edges
    if (length(dim(object$edges)) == 3) {
      edges_count <- apply(object$edges, 1:2, sum)
    }
    edges_count > edge_level * kfolds
  }
  structure(
    list(
      performance = performance,
      edges = edges,
      params = list(
        method = method,
        edge_level = edge_level
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
      sub("^(.)", "\\U\\1", x$params$method, perl = TRUE)
    )
  )
  cat(sprintf("    Positive: %.3f\n", x$performance[, "pos"]))
  cat(sprintf("    Negative: %.3f\n", x$performance[, "neg"]))
  cat(sprintf("    Combined: %.3f\n", x$performance[, "both"]))
  if (!is.null(x$edges)) {
    cat(sprintf("  Prop. edges (%.0f%% folds):\n", x$params$edge_level * 100))
    cat(sprintf("    Positive: %.2f%%\n", mean(x$edges[, "pos"]) * 100))
    cat(sprintf("    Negative: %.2f%%\n", mean(x$edges[, "neg"]) * 100))
  }
  invisible(x)
}
