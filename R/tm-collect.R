#' Collect stored CPM edge masks
#'
#' @param x An object with stored CPM edges.
#' @param ... Unused.
#'
#' @return Stored CPM edge masks.
#' @export
collect_edges <- function(x, ...) {
  UseMethod("collect_edges")
}

#' @export
collect_edges.cpm_fit <- function(x, ...) {
  x$edges
}

#' @export
collect_edges.model_fit <- function(x, ...) {
  if (!inherits(x$fit, "cpm_fit")) {
    stop(
      "`collect_edges()` only supports `model_fit` objects created by the cpmr engine."
    )
  }

  collect_edges(x$fit, ...)
}

#' @export
collect_edges.workflow <- function(x, ...) {
  rlang::check_installed("workflows")
  collect_edges(workflows::extract_fit_parsnip(x), ...)
}
