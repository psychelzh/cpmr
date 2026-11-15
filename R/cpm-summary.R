#' Summarize a `cpm` object
#'
#' @rdname summary.cpm
#' @param object An object of class `cpm`.
#' @param ... For future extension. Currently ignored.
#' @param method Correlation method used for pooled and fold-wise summaries.
#'
#' @details
#' `summary.cpm()` is designed to give a compact correlation-based report. It
#' summarizes pooled out-of-fold correlations for each prediction stream and
#' then reports the mean and standard error of fold-wise correlations.
#'
#' @return A `cpm_summary` object with the following elements:
#' \describe{
#'   \item{`metrics`}{A compact summary-level data frame with columns `level`,
#'     `prediction`, `estimate`, `std_error`, and `method`. It stores pooled
#'     correlations together with fold-wise correlation summaries.}
#'   \item{`tables`}{A list with `pooled` and `foldwise` correlation tables for
#'     downstream tidying.}
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
  pooled_metrics <- compute_pooled_correlation_table(
    object$predictions,
    correlation_method = correlation_method
  )
  foldwise_metrics <- compute_fold_correlation_table(
    object$predictions,
    object$folds,
    correlation_method = correlation_method
  )
  overview_metrics <- compute_summary_correlations(
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
  print_correlation_block(
    values = summary_correlation_values(
      x$metrics,
      level = "pooled",
      prediction_streams = x$settings$prediction_streams
    ),
    header = sprintf("  Pooled correlations (%s):\n", method_name)
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
  print_edge_rate_block(x$edges)
  invisible(x)
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

compute_summary_correlations <- function(
  pooled_metrics,
  foldwise_metrics,
  correlation_method = c("pearson", "spearman")
) {
  correlation_method <- match.arg(correlation_method)

  rbind(
    data.frame(
      level = "pooled",
      prediction = pooled_metrics$prediction,
      estimate = pooled_metrics$estimate,
      std_error = NA_real_,
      method = correlation_method,
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "foldwise",
      prediction = unique(foldwise_metrics$prediction),
      estimate = vapply(
        unique(foldwise_metrics$prediction),
        function(prediction_stream) {
          safe_mean(foldwise_metrics$estimate[
            foldwise_metrics$prediction == prediction_stream
          ])
        },
        numeric(1)
      ),
      std_error = vapply(
        unique(foldwise_metrics$prediction),
        function(prediction_stream) {
          safe_std_error(foldwise_metrics$estimate[
            foldwise_metrics$prediction == prediction_stream
          ])
        },
        numeric(1)
      ),
      method = correlation_method,
      stringsAsFactors = FALSE
    )
  )
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
  rows <- metrics[metrics$level %in% level, , drop = FALSE]
  values <- stats::setNames(
    rep(NA_real_, length(prediction_streams)),
    prediction_streams
  )

  values[rows$prediction] <- rows[[field]]
  values
}

print_correlation_block <- function(values, header, std_error = NULL) {
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
