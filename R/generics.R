#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom generics tidy
#' @export
generics::tidy

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
