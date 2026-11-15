#' Summarize a `cpm` object
#'
#' @rdname summary.cpm
#' @param object An object of class `cpm`.
#' @param ... For future extension. Currently ignored.
#' @param method Correlation method used for correlation summaries.
#'
#' @details
#' `summary.cpm()` reports pooled out-of-fold RMSE and MAE, pooled correlations,
#' and the mean and standard error of fold-wise correlations. Correlations
#' require at least three valid pairs; fold-wise correlations are unavailable
#' for folds with fewer than three assessment observations.
#'
#' @return A `cpm_summary` object with the following elements:
#' \describe{
#'   \item{`metrics`}{A summary-level data frame with `level`, `metric`,
#'     `prediction`, `estimate`, `std_error`, `method`, `n`, and `n_folds`
#'     columns.}
#'   \item{`tables`}{A list of pooled metric and fold-wise correlation tables
#'     for downstream tidying.}
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
  pooled_metrics <- compute_pooled_metric_table(
    object$predictions,
    correlation_method = correlation_method
  )
  foldwise_metrics <- compute_fold_correlation_table(
    object$predictions,
    object$folds,
    correlation_method = correlation_method
  )
  overview_metrics <- compute_summary_metrics(
    pooled_metrics = pooled_metrics,
    foldwise_metrics = foldwise_metrics,
    correlation_method = correlation_method
  )
  edge_rates <- switch(
    object$settings$return_edges,
    none = NULL,
    sum = object$edges / length(object$folds),
    all = apply(object$edges, c(1, 2), mean)
  )
  edge_storage <- switch(
    object$settings$return_edges,
    none = "not stored",
    sum = "summed across folds",
    all = "stored for each fold"
  )

  structure(
    list(
      metrics = overview_metrics,
      tables = list(
        pooled = pooled_metrics,
        foldwise = foldwise_metrics
      ),
      edges = edge_rates,
      settings = list(
        n_folds = length(object$folds),
        small_assessment_folds = any(lengths(object$folds) < 3L),
        edge_storage = edge_storage,
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
  method_name <- sub(
    "^(.)",
    "\\U\\1",
    x$settings$correlation_method,
    perl = TRUE
  )

  cat("CPM summary:\n")
  cat(sprintf("  Number of folds: %d\n", x$settings$n_folds))
  small_assessment_folds <- isTRUE(x$settings$small_assessment_folds)
  print_prediction_error_blocks <- function() {
    for (metric in c("rmse", "mae")) {
      print_metric_block(
        values = summary_metric_values(
          x$metrics,
          level = "pooled",
          metric = metric,
          prediction_streams = x$settings$prediction_streams
        ),
        header = sprintf("  Pooled %s:\n", toupper(metric))
      )
    }
  }
  if (small_assessment_folds) {
    print_prediction_error_blocks()
  }
  print_correlation_block(
    values = summary_correlation_values(
      x$metrics,
      level = "pooled",
      prediction_streams = x$settings$prediction_streams
    ),
    header = sprintf(
      "  Pooled correlations (%s%s):\n",
      method_name,
      if (small_assessment_folds) "; descriptive only" else ""
    )
  )
  foldwise_values <- summary_correlation_values(
    x$metrics,
    level = "foldwise",
    prediction_streams = x$settings$prediction_streams
  )
  if (any(!is.na(foldwise_values))) {
    print_correlation_block(
      values = foldwise_values,
      std_error = summary_correlation_values(
        x$metrics,
        level = "foldwise",
        prediction_streams = x$settings$prediction_streams,
        field = "std_error"
      ),
      n_folds = summary_metric_values(
        x$metrics,
        level = "foldwise",
        metric = "correlation",
        prediction_streams = x$settings$prediction_streams,
        field = "n_folds"
      ),
      total_folds = x$settings$n_folds,
      header = sprintf("  Fold-wise correlations (%s):\n", method_name)
    )
  } else {
    cat(
      paste0(
        "  Fold-wise correlations: unavailable because they were ",
        "undefined for all prediction streams.\n"
      )
    )
  }
  if (!small_assessment_folds) {
    print_prediction_error_blocks()
  }
  print_edge_rate_block(x$edges)
  invisible(x)
}

compute_pooled_metric_table <- function(
  predictions,
  correlation_method = c("pearson", "spearman")
) {
  prediction_streams <- prediction_columns(predictions)
  error_metrics <- do.call(
    rbind,
    lapply(prediction_streams, function(prediction_stream) {
      valid <- stats::complete.cases(
        predictions$observed,
        predictions[[prediction_stream]]
      )
      errors <- predictions$observed[valid] -
        predictions[[prediction_stream]][valid]

      data.frame(
        metric = c("rmse", "mae"),
        prediction = prediction_stream,
        estimate = c(
          if (length(errors) == 0L) NA_real_ else sqrt(mean(errors^2)),
          if (length(errors) == 0L) NA_real_ else mean(abs(errors))
        ),
        n = length(errors),
        method = NA_character_,
        stringsAsFactors = FALSE
      )
    })
  )
  correlations <- compute_pooled_correlation_table(
    predictions,
    correlation_method = correlation_method
  )
  correlations$metric <- "correlation"
  correlations$n <- vapply(
    prediction_streams,
    function(prediction_stream) {
      sum(stats::complete.cases(
        predictions$observed,
        predictions[[prediction_stream]]
      ))
    },
    integer(1)
  )
  correlations <- correlations[
    , c("metric", "prediction", "estimate", "n", "method"),
    drop = FALSE
  ]

  rbind(error_metrics, correlations)
}

compute_pooled_correlation_table <- function(
  predictions,
  correlation_method = c("pearson", "spearman")
) {
  prediction_streams <- prediction_columns(predictions)
  correlation_method <- match.arg(correlation_method)

  data.frame(
    prediction = prediction_streams,
    estimate = unname(vapply(
      prediction_streams,
      function(prediction_stream) {
        safe_cor(
          predictions$observed,
          predictions[[prediction_stream]],
          method = correlation_method
        )
      },
      numeric(1)
    )),
    method = correlation_method,
    stringsAsFactors = FALSE
  )
}

compute_fold_correlation_table <- function(
  predictions,
  folds,
  correlation_method = c("pearson", "spearman")
) {
  prediction_streams <- prediction_columns(predictions)
  correlation_method <- match.arg(correlation_method)

  do.call(
    rbind,
    lapply(seq_along(folds), function(i) {
      rows <- folds[[i]]

      data.frame(
        fold = i,
        metric = "correlation",
        n_assess = length(rows),
        prediction = prediction_streams,
        estimate = unname(vapply(
          prediction_streams,
          function(prediction_stream) {
            safe_cor(
              predictions$observed[rows],
              predictions[[prediction_stream]][rows],
              method = correlation_method
            )
          },
          numeric(1)
        )),
        method = correlation_method,
        stringsAsFactors = FALSE
      )
    })
  )
}

compute_summary_metrics <- function(
  pooled_metrics,
  foldwise_metrics,
  correlation_method = c("pearson", "spearman")
) {
  correlation_method <- match.arg(correlation_method)
  pooled_correlations <- pooled_metrics[
    pooled_metrics$metric == "correlation",
    ,
    drop = FALSE
  ]
  pooled_errors <- pooled_metrics[
    pooled_metrics$metric %in% c("rmse", "mae"),
    ,
    drop = FALSE
  ]
  foldwise_predictions <- unique(foldwise_metrics$prediction)

  rbind(
    data.frame(
      level = rep("pooled", nrow(pooled_errors)),
      metric = pooled_errors$metric,
      prediction = pooled_errors$prediction,
      estimate = pooled_errors$estimate,
      std_error = rep(NA_real_, nrow(pooled_errors)),
      method = pooled_errors$method,
      n = pooled_errors$n,
      n_folds = rep(NA_integer_, nrow(pooled_errors)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = rep("pooled", nrow(pooled_correlations)),
      metric = pooled_correlations$metric,
      prediction = pooled_correlations$prediction,
      estimate = pooled_correlations$estimate,
      std_error = rep(NA_real_, nrow(pooled_correlations)),
      method = correlation_method,
      n = pooled_correlations$n,
      n_folds = rep(NA_integer_, nrow(pooled_correlations)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "foldwise",
      metric = "correlation",
      prediction = foldwise_predictions,
      estimate = vapply(
        foldwise_predictions,
        function(prediction_stream) {
          safe_mean(foldwise_metrics$estimate[
            foldwise_metrics$prediction == prediction_stream
          ])
        },
        numeric(1)
      ),
      std_error = vapply(
        foldwise_predictions,
        function(prediction_stream) {
          safe_std_error(foldwise_metrics$estimate[
            foldwise_metrics$prediction == prediction_stream
          ])
        },
        numeric(1)
      ),
      method = correlation_method,
      n = NA_integer_,
      n_folds = vapply(
        foldwise_predictions,
        function(prediction_stream) {
          sum(!is.na(foldwise_metrics$estimate[
            foldwise_metrics$prediction == prediction_stream
          ]))
        },
        integer(1)
      ),
      stringsAsFactors = FALSE
    )
  )
}

safe_cor <- function(x, y, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  valid <- stats::complete.cases(x, y)
  if (sum(valid) < 3) {
    return(NA_real_)
  }

  x <- x[valid]
  y <- y[valid]
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y, method = method)
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
summary_correlation_values <- function(
  metrics,
  level,
  prediction_streams = unique(metrics$prediction),
  field = c("estimate", "std_error")
) {
  field <- match.arg(field)
  summary_metric_values(
    metrics,
    level = level,
    metric = "correlation",
    prediction_streams = prediction_streams,
    field = field
  )
}

summary_metric_values <- function(
  metrics,
  level,
  metric,
  prediction_streams = unique(metrics$prediction),
  field = c("estimate", "std_error", "n_folds")
) {
  field <- match.arg(field)
  rows <- metrics[
    metrics$level %in% level & metrics$metric %in% metric,
    ,
    drop = FALSE
  ]
  values <- stats::setNames(
    rep(NA_real_, length(prediction_streams)),
    prediction_streams
  )

  values[rows$prediction] <- rows[[field]]
  values
}

print_metric_block <- function(values, header) {
  cat(header)
  for (prediction_stream in names(values)) {
    cat(sprintf(
      "    %s: %s\n",
      prediction_label(prediction_stream),
      format_cor(values[[prediction_stream]])
    ))
  }
}

print_correlation_block <- function(
  values,
  header,
  std_error = NULL,
  n_folds = NULL,
  total_folds = NULL
) {
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
    if (!is.null(n_folds) && !is.na(n_folds[[prediction_stream]])) {
      line <- sprintf(
        "%s (%d/%d folds)",
        line,
        as.integer(n_folds[[prediction_stream]]),
        total_folds
      )
    }
    cat(line, "\n", sep = "")
  }
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

format_cor <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
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
