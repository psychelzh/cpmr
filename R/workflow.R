run_cpm_workflow <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  resamples = 10L,
  selection_levels = NULL,
  selection_metric = c("rmse", "mae"),
  inner_resamples = 5L,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude"),
  call = NULL
) {
  return_edges <- match.arg(return_edges)
  inputs <- prepare_cpm_inputs(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action
  )

  folds <- resolve_assessment_folds(
    resamples = resamples,
    include_cases = inputs$include_cases
  )
  n_folds <- length(folds)
  warn_small_assessment_folds(folds)
  tuning <- normalize_tuning_settings(
    selection_levels = selection_levels,
    selection_metric = selection_metric,
    inner_resamples = inner_resamples,
    selection_spec = object$selection
  )

  warn_large_edge_storage(ncol(conmat), n_folds, return_edges)

  edges <- init_edge_storage(return_edges, conmat, n_folds)
  split_results <- vector("list", n_folds)
  tuning_results <- if (is.null(tuning)) NULL else vector("list", n_folds)

  for (fold in seq_len(n_folds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(inputs$include_cases, rows_test)

    if (is.null(tuning)) {
      split_result <- fit_split(
        conmat = conmat,
        behav = inputs$behav,
        covariates = inputs$covariates,
        rows_train = rows_train,
        rows_test = rows_test,
        selection_spec = object$selection,
        construction_spec = object$construction,
        model_spec = object$model
      )
    } else {
      tuned_split <- fit_tuned_outer_split(
        conmat = conmat,
        behav = inputs$behav,
        covariates = inputs$covariates,
        rows_train = rows_train,
        rows_test = rows_test,
        selection_spec = object$selection,
        construction_spec = object$construction,
        model_spec = object$model,
        tuning = tuning,
        outer_fold = fold
      )
      split_result <- tuned_split$split_result
      tuning_results[[fold]] <- tuned_split$tuning
    }

    if (return_edges == "all") {
      edges[,, fold] <- split_result$edge_mask
    } else if (return_edges == "sum") {
      edges <- edges + split_result$edge_mask
    }
    split_results[[fold]] <- split_result[c("observed", "predictions")]
    rm(split_result)
  }

  predictions <- assemble_fold_predictions(
    observed = inputs$behav,
    folds = folds,
    split_results = split_results
  )

  structure(
    list(
      call = call,
      spec = object,
      settings = list(
        covariates = !is.null(inputs$covariates),
        na_action = inputs$na_action,
        return_edges = return_edges,
        tuning = tuning
      ),
      predictions = predictions,
      edges = edges,
      folds = folds,
      tuning = if (is.null(tuning)) {
        NULL
      } else {
        do.call(rbind, tuning_results)
      }
    ),
    class = "cpm"
  )
}

normalize_tuning_settings <- function(
  selection_levels,
  selection_metric,
  inner_resamples,
  selection_spec
) {
  selection_metric <- match.arg(selection_metric, c("rmse", "mae"))
  if (is.null(selection_levels)) {
    return(NULL)
  }
  if (
    !is.numeric(selection_levels) ||
      !is.null(dim(selection_levels)) ||
      length(selection_levels) < 2L ||
      anyNA(selection_levels) ||
      any(!is.finite(selection_levels)) ||
      any(selection_levels < 0 | selection_levels > 1) ||
      anyDuplicated(selection_levels)
  ) {
    stop(
      "`selection_levels` must contain at least two unique finite numbers between 0 and 1.",
      call. = FALSE
    )
  }
  if (selection_spec$type != "cor") {
    stop(
      "Threshold tuning requires a correlation-based selection specification.",
      call. = FALSE
    )
  }
  if (any(selection_levels %in% c(0, 1))) {
    warning(
      "`selection_levels` includes a boundary value (0 or 1); selection may become degenerate.",
      call. = FALSE
    )
  }

  list(
    levels = selection_levels,
    metric = selection_metric,
    inner_resamples = normalize_fold_count(inner_resamples)
  )
}

fit_tuned_outer_split <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test,
  selection_spec,
  construction_spec,
  model_spec,
  tuning,
  outer_fold
) {
  if (tuning$inner_resamples > length(rows_train)) {
    stop(
      sprintf(
        paste0(
          "Each outer training set must contain at least %d observations ",
          "for `inner_resamples = %d`."
        ),
        tuning$inner_resamples,
        tuning$inner_resamples
      ),
      call. = FALSE
    )
  }
  inner_folds <- resolve_assessment_folds(
    resamples = tuning$inner_resamples,
    include_cases = rows_train
  )
  candidate_scores <- matrix(
    NA_real_,
    nrow = length(inner_folds),
    ncol = length(tuning$levels)
  )
  prediction_stream <- if (construction_spec$sign_mode == "net") {
    "net"
  } else {
    "joint"
  }

  for (inner_fold in seq_along(inner_folds)) {
    rows_inner_test <- inner_folds[[inner_fold]]
    rows_inner_train <- setdiff(rows_train, rows_inner_test)
    prepared <- prepare_split_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_inner_train,
      rows_test = rows_inner_test
    )
    candidate_spec <- selection_spec
    candidate_spec$level <- tuning$levels[[1]]
    validate_edge_selection(
      n_observations = nrow(prepared$conmat_train),
      selection_spec = candidate_spec,
      covariate_rank = prepared$covariate_rank
    )
    associations <- compute_edge_associations(
      prepared$conmat_train,
      prepared$behav_train,
      method = selection_spec$method
    )

    for (candidate in seq_along(tuning$levels)) {
      candidate_spec <- selection_spec
      candidate_spec$level <- tuning$levels[[candidate]]
      result <- fit_prepared_split(
        prepared = prepared,
        selection_spec = candidate_spec,
        construction_spec = construction_spec,
        model_spec = model_spec,
        associations = associations
      )
      candidate_scores[inner_fold, candidate] <- prediction_error(
        observed = result$observed,
        predicted = result$predictions[, prediction_stream],
        metric = tuning$metric
      )
    }
  }

  candidate_estimates <- colMeans(candidate_scores, na.rm = TRUE)
  candidate_estimates[is.nan(candidate_estimates)] <- NA_real_
  if (all(is.na(candidate_estimates))) {
    stop(
      "No candidate selection level produced valid inner-fold predictions.",
      call. = FALSE
    )
  }
  selected <- which.min(replace(
    candidate_estimates,
    is.na(candidate_estimates),
    Inf
  ))
  selected_spec <- selection_spec
  selected_spec$level <- tuning$levels[[selected]]

  list(
    split_result = fit_split(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = selected_spec,
      construction_spec = construction_spec,
      model_spec = model_spec
    ),
    tuning = data.frame(
      outer_fold = outer_fold,
      level = tuning$levels,
      metric = tuning$metric,
      prediction = prediction_stream,
      estimate = candidate_estimates,
      n_inner_folds = colSums(!is.na(candidate_scores)),
      selected = seq_along(tuning$levels) == selected,
      stringsAsFactors = FALSE
    )
  )
}

prediction_error <- function(observed, predicted, metric) {
  valid <- stats::complete.cases(observed, predicted) &
    is.finite(observed) &
    is.finite(predicted)
  if (!any(valid)) {
    return(NA_real_)
  }
  errors <- observed[valid] - predicted[valid]
  score <- switch(
    metric,
    rmse = sqrt(mean(errors^2)),
    mae = mean(abs(errors))
  )
  if (is.finite(score)) score else NA_real_
}

prepare_cpm_inputs <- function(
  conmat,
  behav,
  covariates,
  na_action
) {
  na_action <- match.arg(na_action, c("fail", "exclude"))
  inputs <- normalize_inputs(conmat, behav, covariates)
  include_cases <- complete_case_rows(
    conmat = conmat,
    behav = inputs$behav,
    covariates = inputs$covariates,
    na_action = na_action
  )

  if (length(include_cases) == 0L) {
    stop(
      "No complete-case observations available for CPM.",
      call. = FALSE
    )
  }

  list(
    behav = inputs$behav,
    covariates = inputs$covariates,
    include_cases = include_cases,
    na_action = na_action
  )
}

normalize_inputs <- function(conmat, behav, covariates = NULL) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.", call. = FALSE)
  }
  if (nrow(conmat) != length(behav)) {
    stop(
      "The number of observations in `conmat` and `behav` must match.",
      call. = FALSE
    )
  }
  if (!is.null(rownames(conmat)) && !is.null(names(behav))) {
    if (!identical(rownames(conmat), names(behav))) {
      stop(
        "Case names of `conmat` must match those of behavior data.",
        call. = FALSE
      )
    }
  }

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop(
        "The number of observations in `covariates` and `behav` must match.",
        call. = FALSE
      )
    }
    if (!is.null(rownames(covariates)) && !is.null(names(behav))) {
      if (!identical(rownames(covariates), names(behav))) {
        stop(
          "Case names of `covariates` must match those of behavior data.",
          call. = FALSE
        )
      }
    }
  }

  list(
    behav = behav,
    covariates = covariates
  )
}

complete_case_rows <- function(conmat, behav, covariates, na_action) {
  switch(
    na_action,
    fail = {
      if (anyNA(conmat)) {
        stop("Missing values found in `conmat`", call. = FALSE)
      }
      if (anyNA(behav)) {
        stop("Missing values found in `behav`", call. = FALSE)
      }
      if (!is.null(covariates) && anyNA(covariates)) {
        stop("Missing values found in `covariates`", call. = FALSE)
      }
      seq_along(behav)
    },
    exclude = {
      include_mask <- stats::complete.cases(conmat) & !is.na(behav)
      if (!is.null(covariates)) {
        include_mask <- include_mask & stats::complete.cases(covariates)
      }
      which(include_mask)
    }
  )
}

warn_large_edge_storage <- function(n_edges, n_folds, return_edges) {
  if (return_edges != "all") {
    return(invisible())
  }

  estimated_bytes <- as.double(n_edges) * length(edge_signs) * n_folds * 4
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

warn_small_assessment_folds <- function(folds) {
  small_folds <- sum(lengths(folds) < 3L)
  if (small_folds == 0L) {
    return(invisible(FALSE))
  }

  warning(
    sprintf(
      paste0(
        "%d of %d assessment folds contain fewer than 3 observations. ",
        "Fold-wise correlations for these folds are unavailable; RMSE and ",
        "MAE should be prioritized, and pooled out-of-fold correlations ",
        "should be treated as descriptive."
      ),
      small_folds,
      length(folds)
    ),
    call. = FALSE
  )
  invisible(TRUE)
}

init_edge_storage <- function(return_edges, conmat, n_folds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), n_folds),
      dimnames = list(NULL, edge_signs, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_signs)),
      dimnames = list(NULL, edge_signs)
    ),
    none = NULL
  )
}

assemble_fold_predictions <- function(observed, folds, split_results) {
  pred <- matrix(
    NA_real_,
    nrow = length(observed),
    ncol = ncol(split_results[[1]]$predictions),
    dimnames = list(
      prediction_row_names(observed),
      colnames(split_results[[1]]$predictions)
    )
  )
  fold_id <- rep(NA_integer_, length(observed))
  for (fold in seq_along(folds)) {
    rows_test <- folds[[fold]]
    pred[rows_test, ] <- split_results[[fold]]$predictions
    observed[rows_test] <- split_results[[fold]]$observed
    fold_id[rows_test] <- fold
  }

  prediction_frame(observed, pred, fold = fold_id)
}

prediction_frame <- function(observed, pred, fold) {
  prediction_streams <- colnames(pred)
  predictions <- data.frame(
    row = seq_along(observed),
    observed = observed,
    fold = fold,
    pred,
    row.names = prediction_row_names(observed)
  )

  predictions[, c("row", "fold", "observed", prediction_streams), drop = FALSE]
}

prediction_row_names <- function(observed) {
  if (!is.null(names(observed)) && !anyDuplicated(names(observed))) {
    names(observed)
  } else {
    NULL
  }
}

prediction_columns <- function(predictions) {
  setdiff(names(predictions), c("row", "fold", "observed"))
}
