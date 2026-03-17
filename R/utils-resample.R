resolve_resample_folds <- function(resamples, kfolds, include_cases) {
  if (is.null(resamples)) {
    kfolds <- resolve_kfolds(
      validate_kfolds(kfolds),
      include_cases
    )
    if (kfolds > length(include_cases)) {
      stop("`kfolds` must be less than or equal to complete-case observations.")
    }
    folds <- crossv_kfold(include_cases, kfolds)
  } else {
    if (!is.null(kfolds)) {
      stop("Specify either `resamples` or `kfolds`, not both.")
    }
    folds <- validate_resamples(resamples, include_cases)
    kfolds <- length(folds)
  }

  train_sizes <- length(include_cases) - lengths(folds)
  if (any(train_sizes < 3L)) {
    stop(
      "Each resample must leave at least 3 complete-case training observations."
    )
  }

  list(
    folds = folds,
    kfolds = kfolds
  )
}

crossv_kfold <- function(x, k) {
  split(sample(x), cut(seq_along(x), breaks = k, labels = FALSE))
}

warn_large_edge_storage <- function(n_edges, kfolds, return_edges) {
  if (return_edges != "all") {
    return(invisible())
  }

  estimated_bytes <- as.double(n_edges) * length(edge_types) * kfolds * 4
  threshold_bytes <- 10 * 1024^2
  if (estimated_bytes > threshold_bytes) {
    warning(
      sprintf(
        paste0(
          "Storing fold-wise edges (`return_edges = \"all\"`) may consume ",
          "large memory (~%.1f MB). Consider `return_edges = \"sum\"` or ",
          "`return_edges = \"none\"` when edge storage is optional."
        ),
        estimated_bytes / 1024^2
      )
    )
  }

  invisible()
}

validate_resamples <- function(resamples, include_cases) {
  if (!is.list(resamples) || length(resamples) == 0L) {
    stop("`resamples` must be a non-empty list of assessment indices.")
  }
  if (length(resamples) < 2L) {
    stop("`resamples` must contain at least 2 assessment sets.")
  }

  normalized <- lapply(resamples, function(idx) {
    if (!is.numeric(idx) || anyNA(idx) || any(!is.finite(idx))) {
      stop("Each element in `resamples` must contain finite numeric indices.")
    }
    if (any(idx %% 1 != 0)) {
      stop("Each element in `resamples` must contain integer-valued indices.")
    }

    idx <- as.integer(idx)
    if (any(idx <= 0L)) {
      stop("Each element in `resamples` must contain positive indices.")
    }
    if (anyDuplicated(idx)) {
      stop("Each element in `resamples` must not contain duplicates.")
    }

    idx
  })

  all_assessment <- unlist(normalized, use.names = FALSE)
  include_cases <- sort(unique(include_cases))

  if (!all(all_assessment %in% include_cases)) {
    stop("All `resamples` indices must be contained in complete-case rows.")
  }
  if (length(all_assessment) != length(unique(all_assessment))) {
    stop("`resamples` indices must not overlap across folds.")
  }
  if (!identical(sort(all_assessment), include_cases)) {
    stop("`resamples` indices must cover all complete-case rows exactly once.")
  }

  normalized
}

validate_kfolds <- function(kfolds) {
  if (
    !is.null(kfolds) &&
      (!is.numeric(kfolds) ||
        length(kfolds) != 1L ||
        is.na(kfolds) ||
        !is.finite(kfolds) ||
        kfolds < 2 ||
        kfolds %% 1 != 0)
  ) {
    stop(
      "`kfolds` must be NULL or a single integer greater than or equal to 2."
    )
  }

  if (is.null(kfolds)) {
    return(NULL)
  }

  as.integer(kfolds)
}

resolve_kfolds <- function(kfolds, include_cases) {
  if (is.null(kfolds)) {
    return(length(include_cases))
  }

  kfolds
}

compute_resample_metric <- function(
  real,
  predicted,
  metric = c("rmse", "mae", "correlation"),
  correlation_method = c("pearson", "spearman")
) {
  metric <- match.arg(metric)

  switch(
    metric,
    rmse = safe_rmse(real, predicted),
    mae = safe_mae(real, predicted),
    correlation = safe_cor(
      real,
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
  if ("correlation" %in% metrics) {
    correlation_method <- match.arg(correlation_method)
  }
  metric_tables <- lapply(metrics, function(metric) {
    estimates <- vapply(
      prediction_types,
      function(prediction_type) {
        compute_resample_metric(
          predictions$real,
          predictions[[prediction_type]],
          metric = metric,
          correlation_method = correlation_method
        )
      },
      numeric(1)
    )

    data.frame(
      metric = metric,
      prediction = prediction_types,
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
  if ("correlation" %in% metrics) {
    correlation_method <- match.arg(correlation_method)
  }
  metric_tables <- lapply(metrics, function(metric) {
    fold_tables <- lapply(seq_along(folds), function(i) {
      rows <- folds[[i]]
      estimates <- vapply(
        prediction_types,
        function(prediction_type) {
          compute_resample_metric(
            predictions$real[rows],
            predictions[[prediction_type]][rows],
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
        prediction = prediction_types,
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

summarize_resample_edges <- function(edges, return_edges, kfolds) {
  if (is.null(edges) || return_edges == "none") {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(edges / kfolds)
  }

  apply(edges, c(1, 2), mean)
}
