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
#'     fitted CPM model.}
#'
#'   \item{params}{A list of parameters used in the summary.}
#' @export
summary.cpm <- function(
  object,
  ...,
  method = c("pearson", "spearman")
) {
  method <- match.arg(method)
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

  structure(
    list(
      performance = performance,
      edges = object$edges,
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

safe_mean <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

format_cor <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
}
