#' @export
print.cpm <- function(x, ...) {
  cat("CPM results:\n")
  cat("  Call: ")
  print(x$call)
  cat(sprintf("  Number of observations: %d\n", length(x$real)))
  cat(sprintf("    Complete cases: %d\n", sum(stats::complete.cases(x$pred))))
  if (!is.null(x$edges)) {
    cat(sprintf("  Number of edges: %d\n", dim(x$edges)[1]))
  } else {
    cat("  Number of edges: not stored\n")
  }
  covariates_param <- if (!is.null(x$params$covariates)) {
    x$params$covariates
  } else {
    NA
  }
  cat("  Parameters:\n")
  cat(sprintf("    Covariates:       %s\n", covariates_param))
  cat(sprintf("    Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("    Threshold level:  %.2f\n", x$params$thresh_level))
  cat(sprintf("    Bias correction:  %s\n", x$params$bias_correct))
  invisible(x)
}

#' Collect selected edges from a CPM fit
#'
#' @param x A `cpm` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A logical matrix storing the selected positive and negative edges.
#' @export
collect_edges.cpm <- function(x, ...) {
  x$edges
}

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

#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy.
#' @return A [tibble][tibble::tibble-package] with columns storing parameters of the
#'   `cpm` object and further columns depending on the `component` argument:
#'
#'   For `component = "performance"`:
#'
#'   \item{method}{The method used to calculate the correlation between the real
#'   and predicted values.}
#'
#'   \item{pos}{The correlation between the real and predicted values for
#'   positive edges.}
#'
#'   \item{neg}{The correlation between the real and predicted values for
#'   negative edges.}
#'
#'   For `component = "edges"`:
#'
#'   \item{pos}{A logical vector indicating whether each edge is selected by the
#'   fitted CPM model (positive).}
#'
#'   \item{neg}{A logical vector indicating whether each edge is selected by the
#'   fitted CPM model (negative).}
#' @export
tidy.cpm <- function(x, ..., component = c("performance", "edges")) {
  component <- match.arg(component)
  params <- tibble::as_tibble(x$params)
  sum_x <- summary(x, ...)
  switch(
    component,
    performance = tibble::tibble(
      params,
      method = sum_x$params$method,
      tibble::as_tibble(sum_x$performance)
    ),
    edges = {
      if (is.null(sum_x$edges)) {
        warning(
          "No edges stored in the object."
        )
        return(tibble::tibble())
      }
      tibble::tibble(
        params,
        tibble::as_tibble(apply(sum_x$edges, 2, list))
      )
    }
  )
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
