#' cpm object
#'
#' A `cpm` object is the primary result returned by [cpm()].
#'
#' @section Structure:
#' A `cpm` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for fitting or resampling.}
#'   \item{`spec`}{The originating CPM specification object.}
#'   \item{`settings`}{Runtime settings used for the CPM run, including
#'     missing-data handling and edge-storage preferences.}
#'   \item{`predictions`}{Data frame of observation-level predictions with
#'     fold IDs and one column per configured prediction stream.}
#'   \item{`edges`}{Stored edge output based on `return_edges`
#'     (`NULL`/matrix/array).}
#'   \item{`folds`}{List of assessment-row indices for each fold.}
#'   \item{`model`}{Stored fitted CPM components when available.}
#' }
#'
#' @seealso [cpm()], [summary.cpm()], [tidy.cpm()]
#' @name cpm_object
NULL

#' @export
print.cpm <- function(x, ...) {
  cat("CPM:\n")
  if (!is.null(x$call)) {
    cat("  Call: ")
    print(x$call)
  }
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat(sprintf(
    "    Complete cases: %d\n",
    sum(stats::complete.cases(x$predictions[,
      prediction_columns(x$predictions),
      drop = FALSE
    ]))
  ))
  cat("  Parameters:\n")
  print_setting_line(
    "Covariates",
    format_covariates(x$settings$covariates)
  )
  print_setting_line(
    "Missing data",
    x$settings$na_action
  )
  print_setting_line(
    "Edge storage",
    cpm_edge_storage_label(x)
  )
  print_staged_settings(
    selection = x$spec$selection,
    construction = x$spec$construction,
    model = x$spec$model,
    selection_labels = list(
      method = "Selection method",
      criterion = "Selection criterion",
      level = "Selection level"
    ),
    construction_labels = list(
      sign_mode = "Construction sign mode"
    )
  )
  cat("  Use summary() for aggregate metrics and tidy() for metric tables.\n")
  invisible(x)
}

#' Summarize a `cpm` object
#'
#' @rdname summary.cpm
#' @param object An object of class `cpm`.
#' @param ... For future extension. Currently ignored.
#' @param method Correlation method used for pooled and fold-wise correlation
#'   summaries.
#'
#' @details
#' `summary.cpm()` is designed to give a compact default report. It leads with
#' pooled prediction-error metrics (`RMSE` and `MAE`), then reports pooled and
#' fold-wise correlations as supplementary statistics.
#'
#' @return A `cpm_summary` object with the following elements:
#' \describe{
#'   \item{`metrics`}{A compact summary-level data frame with columns `level`,
#'     `metric`, `prediction`, `estimate`, `std_error`, and `method`. CPM
#'     summaries lead with pooled errors and pooled correlations, then report
#'     fold-wise correlation summaries as supplementary statistics.}
#'   \item{`tables`}{A list with `pooled` and `foldwise` raw metric tables for
#'     downstream tidying.}
#'   \item{`edges`}{Aggregated edge-selection rates, or `NULL` when edges were
#'     not stored.}
#'   \item{`params`}{A one-row tibble of CPM settings used when tidying the
#'     summary object back into tabular form.}
#'   \item{`settings`}{A list containing summary-relevant CPM settings.}
#' }
#' @export
summary.cpm <- function(
  object,
  ...,
  method = c("pearson", "spearman")
) {
  correlation_method <- match.arg(method)
  overview_metrics <- compute_resample_summary_metrics(
    object$predictions,
    object$folds,
    correlation_method = correlation_method
  )
  pooled_metrics <- compute_pooled_metric_table(
    object$predictions,
    metrics = c("rmse", "mae", "correlation"),
    correlation_method = correlation_method
  )
  foldwise_metrics <- compute_fold_metric_table(
    object$predictions,
    object$folds,
    metrics = c("rmse", "mae", "correlation"),
    correlation_method = correlation_method
  )

  structure(
    list(
      metrics = overview_metrics,
      tables = list(
        pooled = pooled_metrics,
        foldwise = foldwise_metrics
      ),
      edges = summarize_cpm_edges(object),
      params = tidy_cpm_fields(object$spec, object$settings),
      settings = list(
        n_folds = length(object$folds),
        edge_storage = cpm_edge_storage_label(object),
        correlation_method = correlation_method,
        prediction_streams = prediction_columns(object$predictions)
      )
    ),
    class = "cpm_summary"
  )
}

#' @rdname summary.cpm
#' @param x An object of class `cpm_summary`.
#' @export
print.cpm_summary <- function(x, ...) {
  correlation_method <- summary_metric_method(
    x$metrics,
    level = c("pooled", "foldwise"),
    metric = "correlation"
  )

  cat("CPM summary:\n")
  cat(sprintf("  Number of folds: %d\n", x$settings$n_folds))
  print_error_block(summary_metric_matrix(
    x$metrics,
    level = "pooled",
    metric = c("rmse", "mae"),
    prediction_streams = x$settings$prediction_streams
  ))
  print_performance_block(
    values = summary_metric_values(
      x$metrics,
      level = "pooled",
      metric = "correlation",
      prediction_streams = x$settings$prediction_streams
    ),
    header = sprintf(
      "  Pooled correlations (%s):\n",
      format_method_name(correlation_method)
    )
  )
  foldwise_values <- summary_metric_values(
    x$metrics,
    level = "foldwise",
    metric = "correlation",
    prediction_streams = x$settings$prediction_streams
  )
  if (any(!is.na(foldwise_values))) {
    print_performance_block(
      values = foldwise_values,
      std_error = summary_metric_values(
        x$metrics,
        level = "foldwise",
        metric = "correlation",
        prediction_streams = x$settings$prediction_streams,
        field = "std_error"
      ),
      header = sprintf(
        "  Fold-wise correlations (%s):\n",
        format_method_name(correlation_method)
      )
    )
  } else {
    cat(
      paste0(
        "  Fold-wise correlations: unavailable because they were ",
        "undefined for all prediction streams.\n"
      )
    )
  }
  print_edge_rate_block(x$edges)
  invisible(x)
}

#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy. Use
#'   `"performance"` for the compact pooled-correlation overview, `"metrics"`
#'   for tidy metric tables derived from [summary.cpm()], or `"edges"` for
#'   stored edge output.
#' @param level Metric table to return when `component = "metrics"`. `"pooled"`
#'   returns pooled out-of-fold metrics, and `"foldwise"` returns one row per
#'   fold, metric, and prediction stream.
#' @param metrics Metrics to keep when `component = "metrics"`.
#' @return A [tibble][tibble::tibble-package] with component-specific columns.
#'   Use `summary(x)$params` when you need the static CPM settings separately
#'   from the tidy result table.
#' @export
tidy.cpm <- function(
  x,
  ...,
  component = c("performance", "metrics", "edges"),
  level = c("foldwise", "pooled"),
  metrics = c("rmse", "mae", "correlation")
) {
  component <- match.arg(component)
  level <- match.arg(level)
  metrics <- match.arg(metrics, several.ok = TRUE)
  sum_x <- summary(x, ...)

  switch(
    component,
    performance = tibble::tibble(
      method = summary_metric_method(
        sum_x$metrics,
        level = "pooled",
        metric = "correlation"
      ),
      tibble::as_tibble_row(as.list(summary_metric_values(
        sum_x$metrics,
        level = "pooled",
        metric = "correlation",
        prediction_streams = sum_x$settings$prediction_streams
      )))
    ),
    metrics = tidy_metric_component(
      tables = sum_x$tables,
      level = level,
      requested_metrics = metrics
    ),
    edges = {
      if (is.null(sum_x$edges)) {
        stop(
          paste0(
            "Edge output is unavailable because this `cpm` object was fit ",
            "with `return_edges = \"none\"`. Refit with `return_edges = ",
            "\"sum\"` or `\"all\"` to tidy edges."
          ),
          call. = FALSE
        )
      }
      tibble::as_tibble(apply(sum_x$edges, 2, list))
    }
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
    construction_sign_mode = construction$sign_mode,
    weight_scale = construction$weight_scale,
    standardize_edges = construction$standardize_edges
  )
}

tidy_model_params <- function(model) {
  list(
    model_type = model$type
  )
}

tidy_metric_component <- function(tables, level, requested_metrics) {
  metric_table <- tables[[level]]
  metric_table <- metric_table[
    metric_table$metric %in% requested_metrics,
    ,
    drop = FALSE
  ]
  tibble::as_tibble(metric_table)
}

summarize_cpm_edges <- function(object) {
  if (is.null(object$edges)) {
    return(NULL)
  }

  if (length(dim(object$edges)) == 2L) {
    if (
      !is.null(object$settings$return_edges) &&
        identical(object$settings$return_edges, "sum") &&
        length(object$folds) > 0L
    ) {
      return(object$edges / length(object$folds))
    }

    return(object$edges)
  }

  summarize_resample_edges(
    object$edges,
    return_edges = object$settings$return_edges,
    n_folds = length(object$folds)
  )
}

cpm_edge_storage_label <- function(object) {
  if (is.null(object$edges)) {
    return(edge_storage_label("none"))
  }

  if (is.null(object$settings$return_edges)) {
    return("stored")
  }

  edge_storage_label(object$settings$return_edges)
}
