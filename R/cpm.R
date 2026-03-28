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

#' Run CPM
#'
#' `cpm()` is the primary CPM workflow. It applies a CPM specification under a
#' resampling plan and returns a `cpm` object with out-of-fold predictions.
#'
#' @rdname spec
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column.
#' @param behav A numeric outcome vector with one value per observation in
#'   `conmat`. Row or column matrices are accepted and converted with [drop()].
#' @param spec A CPM specification created by [spec()]. If `NULL`, [spec()] is
#'   used.
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Vectors are converted to
#'   single-column matrices.
#' @param resamples Optional resampling specification. Use `NULL` for leave-one-
#'   out resampling, a single integer greater than or equal to 2 to request
#'   k-fold resampling, or a list of assessment indices for manual folds. Manual
#'   resamples must contain integer vectors indexing rows in `conmat`.
#' @param return_edges A character string indicating the return value of the
#'   selected edges. `"sum"` stores fold counts for each selected edge,
#'   `"none"` skips edge storage, and `"all"` stores the fold-wise edge masks.
#' @param na_action A character string indicating the action when missing values
#'   are found in the inputs. `"fail"` stops immediately when any required value
#'   is missing. `"exclude"` fits on complete cases and keeps the original row
#'   layout in the returned predictions.
#'
#' @return
#' `spec()` returns a CPM specification object that can be reused across CPM
#' runs.
#'
#' `cpm()` returns a `cpm` object containing observation-level predictions,
#' resampling folds, and optional stored edges. Call [summary.cpm()] for the
#' default aggregate report, or [tidy.cpm()] when you want pooled or fold-wise
#' metrics in tabular form.
#' @export
cpm <- function(
  conmat,
  behav,
  spec = NULL,
  ...,
  covariates = NULL,
  resamples = NULL,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()

  if (is.null(spec)) {
    spec <- spec()
  }
  if (!inherits(spec, "cpm_spec")) {
    stop(
      "`spec` must be a CPM specification created by `spec()`.",
      call. = FALSE
    )
  }

  run_resample_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = return_edges,
    na_action = na_action,
    call = call
  )
}

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

compute_resample_metric <- function(
  observed,
  predicted,
  metric = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
) {
  metric <- match.arg(metric)

  switch(
    metric,
    rmse = safe_rmse(observed, predicted),
    mae = safe_mae(observed, predicted),
    correlation = safe_cor(
      observed,
      predicted,
      method = match.arg(correlation_method)
    )
  )
}

compute_pooled_metric_table <- function(
  predictions,
  metrics = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
) {
  prediction_streams <- prediction_columns(predictions)
  if ("correlation" %in% metrics) {
    correlation_method <- match.arg(correlation_method)
  }
  metric_tables <- lapply(metrics, function(metric) {
    estimates <- vapply(
      prediction_streams,
      function(prediction_stream) {
        compute_resample_metric(
          predictions$observed,
          predictions[[prediction_stream]],
          metric = metric,
          correlation_method = correlation_method
        )
      },
      numeric(1)
    )

    data.frame(
      metric = metric,
      prediction = prediction_streams,
      estimate = unname(estimates),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, metric_tables)
}

compute_fold_metric_table <- function(
  predictions,
  folds,
  metrics = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
) {
  prediction_streams <- prediction_columns(predictions)
  if ("correlation" %in% metrics) {
    correlation_method <- match.arg(correlation_method)
  }
  metric_tables <- lapply(metrics, function(metric) {
    fold_tables <- lapply(seq_along(folds), function(i) {
      rows <- folds[[i]]
      estimates <- vapply(
        prediction_streams,
        function(prediction_stream) {
          compute_resample_metric(
            predictions$observed[rows],
            predictions[[prediction_stream]][rows],
            metric = metric,
            correlation_method = correlation_method
          )
        },
        numeric(1)
      )

      data.frame(
        fold = i,
        n_assess = length(rows),
        metric = metric,
        prediction = prediction_streams,
        estimate = unname(estimates),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, fold_tables)
  })

  do.call(rbind, metric_tables)
}

compute_resample_summary_metrics <- function(
  predictions,
  folds,
  correlation_method = c("pearson", "spearman")
) {
  correlation_method <- match.arg(correlation_method)

  pooled_metrics <- as_summary_metrics(
    compute_pooled_metric_table(
      predictions,
      metrics = c("rmse", "mae", "correlation"),
      correlation_method = correlation_method
    ),
    level = "pooled",
    method = correlation_method
  )

  foldwise_correlations <- summarize_metric_estimates(
    compute_fold_metric_table(
      predictions,
      folds,
      metrics = "correlation",
      correlation_method = correlation_method
    ),
    level = "foldwise",
    method = correlation_method
  )

  rbind(pooled_metrics, foldwise_correlations)
}

summarize_resample_edges <- function(edges, return_edges, n_folds) {
  if (is.null(edges) || return_edges == "none") {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(edges / n_folds)
  }

  apply(edges, c(1, 2), mean)
}

safe_cor <- function(x, y, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  valid <- stats::complete.cases(x, y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }

  x <- x[valid]
  y <- y[valid]
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y, method = method)
}

safe_rmse <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (!any(valid)) {
    return(NA_real_)
  }

  sqrt(mean((x[valid] - y[valid])^2))
}

safe_mae <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (!any(valid)) {
    return(NA_real_)
  }

  mean(abs(x[valid] - y[valid]))
}

safe_mean <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

safe_std_error <- function(x) {
  valid <- x[stats::complete.cases(x)]
  if (length(valid) < 2L) {
    return(NA_real_)
  }

  stats::sd(valid) / sqrt(length(valid))
}

as_summary_metrics <- function(metric_table, level, method = NA_character_) {
  data.frame(
    level = rep(level, nrow(metric_table)),
    metric = metric_table$metric,
    prediction = metric_table$prediction,
    estimate = metric_table$estimate,
    std_error = rep(NA_real_, nrow(metric_table)),
    method = ifelse(
      metric_table$metric == "correlation",
      method,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

summarize_metric_estimates <- function(
  metric_table,
  level,
  method = NA_character_
) {
  keys <- unique(metric_table[c("metric", "prediction")])

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    idx <- metric_table$metric == keys$metric[[i]] &
      metric_table$prediction == keys$prediction[[i]]

    data.frame(
      level = level,
      metric = keys$metric[[i]],
      prediction = keys$prediction[[i]],
      estimate = safe_mean(metric_table$estimate[idx]),
      std_error = safe_std_error(metric_table$estimate[idx]),
      method = if (identical(keys$metric[[i]], "correlation")) {
        method
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

summary_metric_rows <- function(metrics, level = NULL, metric = NULL) {
  rows <- metrics

  if (!is.null(level)) {
    rows <- rows[rows$level %in% level, , drop = FALSE]
  }

  if (!is.null(metric)) {
    rows <- rows[rows$metric %in% metric, , drop = FALSE]
  }

  rows
}

summary_metric_values <- function(
  metrics,
  level,
  metric,
  prediction_streams = unique(metrics$prediction),
  field = c("estimate", "std_error")
) {
  field <- match.arg(field)
  rows <- summary_metric_rows(metrics, level = level, metric = metric)
  values <- stats::setNames(
    rep(NA_real_, length(prediction_streams)),
    prediction_streams
  )

  values[rows$prediction] <- rows[[field]]
  values
}

summary_metric_matrix <- function(
  metrics,
  level,
  metric,
  prediction_streams = unique(metrics$prediction),
  field = c("estimate", "std_error")
) {
  field <- match.arg(field)
  metric <- as.character(metric)

  values <- do.call(
    rbind,
    lapply(metric, function(metric_name) {
      unname(summary_metric_values(
        metrics,
        level = level,
        metric = metric_name,
        prediction_streams = prediction_streams,
        field = field
      ))
    })
  )

  rownames(values) <- metric
  colnames(values) <- prediction_streams
  values
}

summary_metric_method <- function(metrics, level, metric) {
  rows <- summary_metric_rows(metrics, level = level, metric = metric)
  methods <- unique(rows$method[!is.na(rows$method)])

  if (!length(methods)) {
    return(NA_character_)
  }

  methods[[1]]
}

print_performance_block <- function(values, header, std_error = NULL) {
  cat(header)
  for (prediction_stream in names(values)) {
    line <- sprintf(
      "    %s: %s",
      prediction_label(prediction_stream),
      format_cor(values[[prediction_stream]])
    )
    if (!is.null(std_error) && !is.na(std_error[[prediction_stream]])) {
      line <- sprintf(
        "%s (SE %s)",
        line,
        format_cor(std_error[[prediction_stream]])
      )
    }
    cat(line, "\n", sep = "")
  }
}

print_error_block <- function(errors, header = "  Prediction error:\n") {
  cat(header)
  for (error_type in rownames(errors)) {
    cat(sprintf("    %s:\n", toupper(error_type)))
    for (prediction_stream in colnames(errors)) {
      cat(sprintf(
        "      %s: %s\n",
        prediction_label(prediction_stream),
        format_value(errors[error_type, prediction_stream])
      ))
    }
  }

  invisible(NULL)
}

print_edge_rate_block <- function(edges, header = "  Selected edges:\n") {
  if (is.null(edges)) {
    return(invisible(NULL))
  }

  cat(header)
  for (edge_type in edge_signs) {
    cat(sprintf(
      "    %s: %s\n",
      prediction_label(edge_type),
      format_rate(safe_mean(edges[, edge_type]))
    ))
  }

  invisible(NULL)
}

format_value <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_cor <- function(x) {
  format_value(x)
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
}

format_threshold_level <- function(x) {
  trimws(formatC(x, format = "fg", digits = 3))
}

format_edge_standardization <- function(x) {
  if (isTRUE(x)) "z-score" else "none"
}

format_covariates <- function(x) {
  if (isTRUE(x)) "included" else "none"
}

format_method_name <- function(x) {
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

format_model_type <- function(model_type) {
  switch(
    model_type,
    lm = "linear regression",
    model_type
  )
}

format_weighting_label <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", "sigmoid")
}

format_weight_scale <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", format_threshold_level(weight_scale))
}

edge_storage_labels <- c(
  none = "not stored",
  sum = "summed across folds",
  all = "stored for each fold"
)

edge_storage_label <- function(x) {
  unname(edge_storage_labels[[x]])
}

prediction_labels <- c(
  joint = "Joint",
  net = "Net",
  positive = "Positive",
  negative = "Negative"
)

prediction_label <- function(prediction_stream) {
  unname(prediction_labels[[prediction_stream]])
}

print_setting_line <- function(label, value, indent = "    ", width = 22L) {
  cat(sprintf(
    "%s%-*s %s\n",
    indent,
    width,
    paste0(label, ":"),
    value
  ))
}

print_selection_settings <- function(
  selection,
  indent = "    ",
  method_label = "Method",
  criterion_label = "Criterion",
  level_label = "Level"
) {
  print_setting_line(method_label, selection$method, indent = indent)
  print_setting_line(criterion_label, selection$criterion, indent = indent)
  print_setting_line(
    level_label,
    format_threshold_level(selection$level),
    indent = indent
  )

  invisible(NULL)
}

print_construction_settings <- function(
  construction,
  indent = "    ",
  sign_mode_label = "Sign mode"
) {
  print_setting_line(
    sign_mode_label,
    construction$sign_mode,
    indent = indent
  )
  print_setting_line(
    "Edge weighting",
    format_weighting_label(construction$weight_scale),
    indent = indent
  )
  print_setting_line(
    "Weight scale",
    format_weight_scale(construction$weight_scale),
    indent = indent
  )
  print_setting_line(
    "Edge standardization",
    format_edge_standardization(construction$standardize_edges),
    indent = indent
  )

  invisible(NULL)
}

print_model_settings <- function(
  model,
  indent = "    ",
  model_label = "Outcome model"
) {
  print_setting_line(
    model_label,
    format_model_type(model$type),
    indent = indent
  )

  invisible(NULL)
}

print_staged_settings <- function(
  selection,
  construction,
  model,
  indent = "    ",
  headers = NULL,
  selection_labels = list(
    method = "Method",
    criterion = "Criterion",
    level = "Level"
  ),
  construction_labels = list(
    sign_mode = "Sign mode"
  ),
  model_label = "Outcome model"
) {
  if (!is.null(headers$selection)) {
    cat(headers$selection)
  }
  print_selection_settings(
    selection,
    indent = indent,
    method_label = selection_labels$method,
    criterion_label = selection_labels$criterion,
    level_label = selection_labels$level
  )
  if (!is.null(headers$construction)) {
    cat(headers$construction)
  }
  print_construction_settings(
    construction,
    indent = indent,
    sign_mode_label = construction_labels$sign_mode
  )
  if (!is.null(headers$model)) {
    cat(headers$model)
  }
  print_model_settings(
    model,
    indent = indent,
    model_label = model_label
  )

  invisible(NULL)
}
