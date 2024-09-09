#' Summary of a cpm object.
#'
#' This function provides a summary of a \code{cpm} object, including the
#' prediction performance and the selected edges.
#'
#' @rdname summary.cpm
#' @param object An object of class \code{cpm}.
#' @param edge_level A numeric value between 0 and 1 indicating the proportional
#'   threshold for edge selection.
#' @param ... Other parameters passed to the function.
#' @return A list of class \code{cpm_summary} containing two elements:
#'   \item{performance}{A matrix of prediction performance, including the
#'     correlation between the real and predicted values for both edges,
#'     positive edges only, and negative edges only.}
#'
#'   \item{edges}{A logical vector indicating whether each edge is selected
#'     based on the edge_level.}
#' @export
summary.cpm <- function(object, edge_level = 0.5, ...) {
  # summary prediction performance
  performance <- stats::cor(object$real, object$pred)
  # summary edge selection
  edges <- if (!is.null(object$edges)) {
    if (length(dim(object$edges)) == 3) {
      object$edges <- apply(object$edges, 2:3, sum)
    }
    object$edges > edge_level * length(unique(object$folds))
  }
  structure(
    list(
      performance = performance,
      edges = edges,
      edge_level = edge_level
    ),
    class = "cpm_summary"
  )
}

#' @rdname summary.cpm
#' @param x An object of class \code{cpm_summary}.
#' @export
print.cpm_summary <- function(x, ...) {
  cat("CPM summary:\n")
  cat("  Performance: \n")
  cat(sprintf("    Positive: %.3f\n", x$performance[, "pos"]))
  cat(sprintf("    Negative: %.3f\n", x$performance[, "neg"]))
  cat(sprintf("    Combined: %.3f\n", x$performance[, "both"]))
  if (!is.null(x$edges)) {
    cat(sprintf("  Prop. edges (%.0f%% folds):\n", x$edge_level * 100))
    cat(sprintf("    Positive: %.2f%%\n", mean(x$edges[, "pos"]) * 100))
    cat(sprintf("    Negative: %.2f%%\n", mean(x$edges[, "neg"]) * 100))
  }
  invisible(x)
}
