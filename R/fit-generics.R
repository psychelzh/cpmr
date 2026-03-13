#' @importFrom generics fit
#' @export
generics::fit

#' Fit a model specification on resamples
#'
#' Generic for fitting model specifications across resamples.
#'
#' @param object A model specification object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A resampling result object.
#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' Collect fold-level metrics from resampling results
#'
#' Generic for extracting fold-level metrics from resampling result objects.
#'
#' @param x A resampling result object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A data frame or tibble with one row per fold.
#' @export
collect_metrics <- function(x, ...) {
  UseMethod("collect_metrics")
}

#' Collect observation-level predictions from resampling results
#'
#' Generic for extracting observation-level predictions from resampling result
#' objects.
#'
#' @param x A resampling result object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A data frame or tibble with one row per observation.
#' @export
collect_predictions <- function(x, ...) {
  UseMethod("collect_predictions")
}

#' Collect selected edges from fitted objects
#'
#' Generic for extracting stored selected-edge outputs.
#'
#' @param x A fitted object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A matrix, array, or `NULL` depending on how edges were stored.
#' @export
collect_edges <- function(x, ...) {
  UseMethod("collect_edges")
}