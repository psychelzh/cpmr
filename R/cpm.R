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
#'   \item{`settings`}{Runtime settings used at fit time, such as whether
#'     covariates were supplied and the missing-data policy.}
#'   \item{`predictions`}{Data frame of observation-level outputs with columns
#'     `row`, `observed`, and one column per configured prediction stream.}
#'   \item{`edges`}{Stored single-fit edge mask as a `p x 2` logical matrix
#'     with `positive` and `negative` columns.}
#'   \item{`model`}{Trained CPM model components used by prediction.}
#' }
#'
#' @seealso [fit()], [summary.cpm()], [tidy.cpm()]
#' @name cpm
NULL

new_cpm <- function(call, spec, settings, predictions, edges, model) {
  structure(
    list(
      call = call,
      spec = spec,
      settings = settings,
      predictions = predictions,
      edges = edges,
      model = model
    ),
    class = "cpm"
  )
}

#' @export
print.cpm <- function(x, ...) {
  cat("CPM fit:\n")
  cat("  Call: ")
  print(x$call)
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf(
    "    Complete cases: %d\n",
    sum(stats::complete.cases(x$predictions[,
      prediction_columns(x$predictions),
      drop = FALSE
    ]))
  ))
  cat(sprintf("  Candidate edges: %d\n", dim(x$edges)[1]))
  covariates_param <- if (!is.null(x$settings$covariates)) {
    x$settings$covariates
  } else {
    NA
  }
  cat("  Parameters:\n")
  print_setting_line(
    "Covariates",
    format_covariates(covariates_param)
  )
  print_staged_settings(
    selection = x$spec$selection,
    construction = x$spec$construction,
    model = x$spec$model,
    prediction_streams = prediction_columns(x$predictions),
    selection_labels = list(
      method = "Selection method",
      criterion = "Selection criterion",
      level = "Selection level"
    ),
    construction_labels = list(
      polarity = "Construction polarity"
    )
  )
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
#'   correlation between the observed and predicted values.
#' @return A list of class \code{cpm_summary} containing:
#'   \item{metrics}{A data frame with columns `level`, `metric`, `prediction`,
#'     `estimate`, `std_error`, and `method`. Single-fit CPM summaries store
#'     correlation metrics at `level = "single"`.}
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
  metrics <- as_summary_metrics(
    compute_pooled_metric_table(
      object$predictions,
      metrics = "correlation",
      correlation_method = method
    ),
    level = "single",
    method = method
  )

  structure(
    list(
      metrics = metrics,
      edges = object$edges,
      params = list(
        method = method,
        prediction_streams = prediction_columns(object$predictions)
      )
    ),
    class = "cpm_summary"
  )
}

#' @rdname summary.cpm
#' @param x An object of class \code{cpm_summary}.
#' @export
print.cpm_summary <- function(x, ...) {
  method <- summary_metric_method(
    x$metrics,
    level = "single",
    metric = "correlation"
  )
  cat("CPM summary:\n")
  print_performance_block(
    values = summary_metric_values(
      x$metrics,
      level = "single",
      metric = "correlation",
      prediction_streams = x$params$prediction_streams
    ),
    header = sprintf("  Performance (%s):\n", format_method_name(method))
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
#'   \item{method}{The method used to calculate the correlation between the
#'   observed and predicted values.}
#'
#'   \item{prediction columns}{One numeric column per configured prediction
#'   stream. For example, `joint`, `positive`, and `negative` when
#'   `construction_polarity = "separate"`, or `net` when
#'   `construction_polarity = "net"`.}
#'
#'   For `component = "edges"`:
#'
#'   \item{positive}{A logical vector indicating whether each edge is selected
#'   by the fitted CPM model (positive).}
#'
#'   \item{negative}{A logical vector indicating whether each edge is selected
#'   by the fitted CPM model (negative).}
#' @export
tidy.cpm <- function(x, ..., component = c("performance", "edges")) {
  component <- match.arg(component)
  params <- tidy_cpm_fields(x$spec, x$settings)
  sum_x <- summary(x, ...)
  switch(
    component,
    performance = tibble::tibble(
      params,
      method = summary_metric_method(
        sum_x$metrics,
        level = "single",
        metric = "correlation"
      ),
      tibble::as_tibble_row(as.list(summary_metric_values(
        sum_x$metrics,
        level = "single",
        metric = "correlation",
        prediction_streams = sum_x$params$prediction_streams
      )))
    ),
    edges = tibble::tibble(
      params,
      tibble::as_tibble(apply(sum_x$edges, 2, list))
    )
  )
}

tidy_cpm_fields <- function(spec, settings) {
  tibble::as_tibble(c(
    settings,
    tidy_selection_params(spec$selection),
    tidy_construction_params(spec$construction),
    tidy_model_params(spec$model)
  ))
}

tidy_selection_params <- function(selection) {
  list(
    selection_type = selection$type,
    selection_method = selection$method,
    selection_criterion = selection$criterion,
    selection_level = selection$level
  )
}

tidy_construction_params <- function(construction) {
  list(
    construction_type = construction$type,
    construction_polarity = construction$polarity,
    weight_scale = construction$weight_scale,
    standardize_edges = construction$standardize_edges
  )
}

tidy_model_params <- function(model) {
  list(
    model_type = model$type
  )
}
