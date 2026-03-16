#' cpm Fitted Object
#'
#' A `cpm` object is returned by [fit()] when fitting a [cpm_spec()] on a
#' single dataset.
#'
#' @section Structure:
#' A `cpm` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for fitting.}
#'   \item{`spec`}{The originating `cpm_spec` object.}
#'   \item{`params`}{Parameter list used at fit time.}
#'   \item{`predictions`}{Data frame of observation-level outputs with columns
#'     `row`, `real`, `both`, `pos`, and `neg`.}
#'   \item{`edges`}{Stored single-fit edge mask as a `p x 2` logical matrix
#'     with `pos` and `neg` columns.}
#'   \item{`model`}{Trained CPM model components used by prediction.}
#' }
#'
#' @seealso [fit()], [summary.cpm()], [tidy.cpm()]
#' @name cpm
NULL

new_cpm <- function(call, spec, params, predictions, edges, model) {
  structure(
    list(
      call = call,
      spec = spec,
      params = params,
      predictions = predictions,
      edges = edges,
      model = model
    ),
    class = "cpm"
  )
}

#' @export
print.cpm <- function(x, ...) {
  cat("CPM results:\n")
  cat("  Call: ")
  print(x$call)
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf(
    "    Complete cases: %d\n",
    sum(stats::complete.cases(x$predictions[, prediction_types, drop = FALSE]))
  ))
  cat(sprintf("  Number of edges: %d\n", dim(x$edges)[1]))
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
      prediction_types,
      function(edge_type) {
        safe_cor(
          object$predictions$real,
          object$predictions[[edge_type]],
          method = method
        )
      },
      numeric(1)
    ),
    nrow = 1,
    dimnames = list(NULL, prediction_types)
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
  print_performance_block(
    values = x$performance[1, prediction_types],
    header = sprintf(
      "  Performance (%s):\n",
      sub("^(.)", "\\U\\1", x$params$method, perl = TRUE)
    )
  )
  print_edge_rate_block(x$edges)
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
    edges = tibble::tibble(
      params,
      tibble::as_tibble(apply(sum_x$edges, 2, list))
    )
  )
}
