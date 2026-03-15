new_cpm_fit <- function(model, edges, network, predictors, outcome, params) {
  structure(
    list(
      model = model,
      edges = edges,
      network = network,
      predictors = predictors,
      outcome = outcome,
      params = params
    ),
    class = "cpm_fit"
  )
}

#' CPM engine fit object
#'
#' Internal engine fit object returned by the `cpmr` parsnip engine.
#'
#' @param x A `cpm_fit` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @name cpm_fit
NULL

#' @export
print.cpm_fit <- function(x, ...) {
  cat("CPM engine fit\n")
  cat(sprintf("  Predictors: %d\n", length(x$predictors)))
  cat(sprintf("  Network: %s\n", x$network))
  cat(sprintf("  Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("  Threshold level: %.3f\n", x$params$thresh_level))
  cat(sprintf("  Bias correction: %s\n", x$params$bias_correct))
  invisible(x)
}

#' @export
predict.cpm_fit <- function(
  object,
  new_data,
  type = c("numeric", "raw"),
  network = object$network,
  ...
) {
  type <- match.arg(type)
  network <- core_validate_network(network)

  pred <- core_predict_networks(object, new_data)
  switch(
    type,
    numeric = unname(pred[, network]),
    raw = tibble::as_tibble(pred)
  )
}
