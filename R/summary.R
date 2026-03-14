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
  dots <- list(...)
  if ("edge_level" %in% names(dots)) {
    stop(
      paste0(
        "`edge_level` is no longer supported for `summary.cpm()`. ",
        "Use `collect_edges()` for resampling outputs."
      )
    )
  }

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
      sub("^(.)", "\\U\\1", x$params$method, perl = TRUE)
    )
  )
  cat(sprintf("    Positive: %.3f\n", x$performance[, "pos"]))
  cat(sprintf("    Negative: %.3f\n", x$performance[, "neg"]))
  cat(sprintf("    Combined: %.3f\n", x$performance[, "both"]))
  if (!is.null(x$edges)) {
    cat("  Selected edges:\n")
    cat(sprintf("    Positive: %.2f%%\n", mean(x$edges[, "pos"]) * 100))
    cat(sprintf("    Negative: %.2f%%\n", mean(x$edges[, "neg"]) * 100))
  }
  invisible(x)
}
