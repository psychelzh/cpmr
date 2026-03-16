#' @export
print.cpm <- function(x, ...) {
  edge_storage <- describe_single_fit_edge_storage(x)

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
  cat(sprintf("    Edge storage:     %s\n", edge_storage))
  cat(sprintf("    Bias correction:  %s\n", x$params$bias_correct))
  invisible(x)
}

describe_single_fit_edge_storage <- function(x) {
  if (!is.null(x$params$return_edges)) {
    return(x$params$return_edges)
  }

  if (is.null(x$edges)) {
    return("none")
  }

  if (length(dim(x$edges)) == 3L) {
    return("all")
  }

  "stored"
}

#' Collect selected edges from a CPM fit
#'
#' @param x A `cpm` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A matrix when single-fit edges are stored, a 3D array for
#'   `return_edges = "all"`, or `NULL` for `return_edges = "none"`.
#' @export
collect_edges.cpm <- function(x, ...) {
  x$edges
}
