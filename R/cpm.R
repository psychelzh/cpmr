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
#' @seealso [cpm()], [summary.cpm()], [resample_metrics()]
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
  cat("  Use summary() for aggregate metrics.\n")
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
#'   \item{`metrics`}{A data frame with columns `level`, `metric`,
#'     `prediction`, `estimate`, `std_error`, and `method`. CPM summaries store
#'     pooled errors and pooled correlations at `level = "pooled"`, and
#'     fold-wise correlation summaries at `level = "foldwise"`.}
#'   \item{`edges`}{Aggregated edge-selection rates, or `NULL` when edges were
#'     not stored.}
#'   \item{`settings`}{A list containing summary-relevant CPM settings.}
#' }
#' @export
summary.cpm <- function(
  object,
  ...,
  method = c("pearson", "spearman")
) {
  correlation_method <- match.arg(method)

  structure(
    list(
      metrics = compute_resample_summary_metrics(
        object$predictions,
        object$folds,
        correlation_method = correlation_method
      ),
      edges = summarize_cpm_edges(object),
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
#' @param component A character vector indicating the component to tidy.
#' @return A [tibble][tibble::tibble-package] with columns storing parameters of
#'   the `cpm` object and further columns depending on the `component`
#'   argument.
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
    edges = tibble::tibble(
      params,
      tibble::as_tibble(apply(sum_x$edges, 2, list))
    )
  )
}

#' Extract metrics from a `cpm` object
#'
#' @param x A `cpm` object.
#' @param level Which level of metric output to return. Use `"foldwise"` for
#'   one row per fold, metric, and prediction stream, or `"pooled"` for one row
#'   per metric and prediction stream computed across all out-of-fold
#'   predictions.
#' @param metrics Which metrics to include. Supported values are `"rmse"`,
#'   `"mae"`, and `"correlation"`.
#' @param correlation_method Correlation method used when `metrics` includes
#'   `"correlation"`.
#'
#' @details
#' Use `resample_metrics()` when you want CPM metrics in a tabular form for
#' downstream inspection or plotting. Compared with [summary.cpm()], this
#' helper is less opinionated: it can return pooled metrics across all
#' predictions or the raw fold-wise metrics used to build aggregate summaries.
#'
#' @return A data frame. For `level = "foldwise"`, the returned columns are
#'   `fold`, `n_assess`, `metric`, `prediction`, and `estimate`. For
#'   `level = "pooled"`, the returned columns are `metric`, `prediction`, and
#'   `estimate`.
#'
#' @examples
#' withr::local_seed(123)
#' conmat <- matrix(rnorm(200), nrow = 20)
#' behav <- rowMeans(conmat[, 1:5, drop = FALSE]) + rnorm(20, sd = 0.2)
#' res <- cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 4)
#'
#' head(resample_metrics(res))
#' resample_metrics(res, level = "pooled", metrics = "correlation")
#' @export
resample_metrics <- function(
  x,
  level = c("foldwise", "pooled"),
  metrics = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
) {
  if (!inherits(x, "cpm")) {
    stop("`resample_metrics()` requires a `cpm` object.")
  }

  level <- match.arg(level)
  metrics <- match.arg(metrics, several.ok = TRUE)

  switch(
    level,
    pooled = compute_pooled_metric_table(
      predictions = x$predictions,
      metrics = metrics,
      correlation_method = correlation_method
    ),
    foldwise = compute_fold_metric_table(
      predictions = x$predictions,
      folds = x$folds,
      metrics = metrics,
      correlation_method = correlation_method
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
