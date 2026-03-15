#' Extract CPM edge masks from a fitted tidymodels object
#'
#' Use this helper with `tune::control_resamples(extract = ...)` or
#' `tune::control_grid(extract = ...)` to retain lightweight fold-wise CPM edge
#' masks during resampling.
#'
#' @param x A fitted `cpm_fit`, `parsnip::model_fit`, or fitted
#'   `workflows::workflow`.
#' @param ... Unused.
#'
#' @return A lightweight `cpm_edge_extract` object containing predictor names and
#'   CPM edge masks.
#' @export
extract_cpm_edges <- function(x, ...) {
  if (inherits(x, "workflow")) {
    rlang::check_installed("workflows")
    x <- workflows::extract_fit_parsnip(x)
  }

  if (inherits(x, "model_fit")) {
    if (!inherits(x$fit, "cpm_fit")) {
      stop(
        paste(
          "`extract_cpm_edges()` only supports fitted `model_fit` objects",
          "created by the cpmr engine."
        )
      )
    }
    x <- x$fit
  }

  if (!inherits(x, "cpm_fit")) {
    stop(
      paste(
        "`extract_cpm_edges()` requires a fitted `cpm_fit`, `model_fit`,",
        "or fitted `workflow` object created by cpmr."
      )
    )
  }

  new_cpm_edge_extract(
    predictors = x$predictors,
    edges = x$edges
  )
}

new_cpm_edge_extract <- function(predictors, edges) {
  if (!is.character(predictors) || length(predictors) == 0L) {
    stop("`predictors` must be a non-empty character vector.")
  }
  if (!is.matrix(edges) || !is.logical(edges)) {
    stop("`edges` must be a logical matrix.")
  }
  if (!identical(colnames(edges), edge_signs)) {
    stop("`edges` must have columns named `pos` and `neg`.")
  }
  if (nrow(edges) != length(predictors)) {
    stop("`predictors` and `edges` must describe the same number of rows.")
  }

  structure(
    list(
      predictors = predictors,
      edges = edges
    ),
    class = "cpm_edge_extract"
  )
}
