#' cpm object
#'
#' A `cpm` object is the primary result returned by [cpm()].
#'
#' @section Structure:
#' A `cpm` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for the CPM run.}
#'   \item{`spec`}{The originating CPM specification object.}
#'   \item{`settings`}{Runtime settings used for the CPM run, including
#'     missing-data handling and edge-storage preferences.}
#'   \item{`predictions`}{Data frame of observation-level predictions with
#'     fold IDs and one column per configured prediction stream.}
#'   \item{`edges`}{Stored edge output based on `return_edges`
#'     (`NULL`/matrix/array).}
#'   \item{`folds`}{List of assessment-row indices for each fold.}
#' }
#'
#' @seealso [cpm()], [summary.cpm()], [tidy.cpm()]
#' @name cpm_object
NULL

#' Run CPM
#'
#' `cpm()` is the primary CPM workflow. It applies a CPM specification under
#' the requested assessment plan and returns a `cpm` object with out-of-fold
#' predictions.
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
#' @param resamples Optional assessment-plan specification. Use `NULL` for
#'   leave-one-out assessment, a single integer greater than or equal to 2 to
#'   request k-fold assessment, or a list of assessment indices for manual
#'   folds. Manual `resamples` must contain integer vectors indexing rows in
#'   `conmat`.
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
#' assessment folds, and optional stored edges. Call [summary.cpm()] for the
#' default correlation summary, or [tidy.cpm()] when you want pooled or
#' fold-wise correlation tables.
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

  run_cpm_workflow(
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
  edge_storage <- switch(
    x$settings$return_edges,
    none = "not stored",
    sum = "summed across folds",
    all = "stored for each fold"
  )

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
    if (isTRUE(x$settings$covariates)) "included" else "none"
  )
  print_setting_line(
    "Missing data",
    x$settings$na_action
  )
  print_setting_line(
    "Edge storage",
    edge_storage
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
  cat(
    "  Use summary() for aggregate correlations and tidy() for correlation tables.\n"
  )
  invisible(x)
}

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

#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy. Use
#'   `"metrics"` for correlation tables derived from [summary.cpm()], or
#'   `"edges"` for stored edge output.
#' @param level Correlation table to return when `component = "metrics"`.
#'   `"pooled"` returns pooled out-of-fold correlations, and `"foldwise"`
#'   returns one row per fold and prediction stream.
#' @return A [tibble][tibble::tibble-package] with component-specific columns.
#' @export
tidy.cpm <- function(
  x,
  ...,
  component = c("metrics", "edges"),
  level = c("foldwise", "pooled")
) {
  component <- match.arg(component)
  level <- match.arg(level)
  sum_x <- summary(x, ...)

  switch(
    component,
    metrics = tibble::as_tibble(sum_x$tables[[level]]),
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

format_threshold_level <- function(x) {
  trimws(formatC(x, format = "fg", digits = 3))
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
    if (construction$weight_scale == 0) "none" else "sigmoid",
    indent = indent
  )
  print_setting_line(
    "Weight scale",
    if (construction$weight_scale == 0) {
      "none"
    } else {
      format_threshold_level(construction$weight_scale)
    },
    indent = indent
  )
  print_setting_line(
    "Edge standardization",
    if (isTRUE(construction$standardize_edges)) "z-score" else "none",
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
    switch(
      model$type,
      lm = "linear regression",
      model$type
    ),
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
