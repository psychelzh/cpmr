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
  prediction_types = unique(metrics$prediction),
  field = c("estimate", "std_error")
) {
  field <- match.arg(field)
  rows <- summary_metric_rows(metrics, level = level, metric = metric)
  values <- stats::setNames(
    rep(NA_real_, length(prediction_types)),
    prediction_types
  )

  values[rows$prediction] <- rows[[field]]
  values
}

summary_metric_matrix <- function(
  metrics,
  level,
  metric,
  prediction_types = unique(metrics$prediction),
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
        prediction_types = prediction_types,
        field = field
      ))
    })
  )

  rownames(values) <- metric
  colnames(values) <- prediction_types
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
