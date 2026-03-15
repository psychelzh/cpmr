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

  estimated_bytes <- as.double(n_edges) * length(edge_signs) * kfolds * 4
  threshold_bytes <- 10 * 1024^2
  if (estimated_bytes > threshold_bytes) {
    warning(
      sprintf(
        paste0(
          "Storing fold-wise edges (`return_edges = \"all\"`) may consume ",
          "large memory (~%.1f MB). Consider `return_edges = \"sum\"`."
        ),
        estimated_bytes / 1024^2
      )
    )
  }

  invisible()
}

core_fit_resamples <- function(
  conmat,
  behav,
  resamples = NULL,
  kfolds = NULL,
  covariates = NULL,
  thresh_method = "alpha",
  thresh_level = 0.01,
  bias_correct = TRUE,
  network = "both",
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  thresh_method <- validate_thresh_method(thresh_method)
  thresh_level <- validate_thresh_level(thresh_level)
  bias_correct <- validate_bias_correct(bias_correct)
  network <- validate_network(network)
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)
  warn_extreme_thresh_level(thresh_method, thresh_level)

  normalized <- core_normalize_inputs(conmat, behav, covariates)
  conmat <- normalized$conmat
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action
  )
  if (length(include_cases) == 0L) {
    stop("No complete-case observations available for resampling.")
  }
  if (length(include_cases) < 2L) {
    stop("At least 2 complete-case observations are required for resampling.")
  }

  resolved <- resolve_resample_folds(
    resamples = resamples,
    kfolds = kfolds,
    include_cases = include_cases
  )
  folds <- resolved$folds
  kfolds <- resolved$kfolds

  warn_large_edge_storage(ncol(conmat), kfolds, return_edges)

  pred <- init_prediction_matrix(behav)
  edges <- init_edge_storage(return_edges, conmat, kfolds)
  real <- behav

  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    training <- core_prepare_training_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train
    )
    fold_edges <- core_select_edges(
      conmat = training$conmat,
      behav = training$behav,
      method = thresh_method,
      level = thresh_level
    )
    fold_model <- core_train_model(
      conmat = training$conmat,
      behav = training$behav,
      edges = fold_edges,
      bias_correct = bias_correct
    )
    assessment <- core_prepare_assessment_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      covariates_train = training$covariates
    )

    pred[rows_test, ] <- core_predict_model(fold_model, assessment$conmat)
    real[rows_test] <- assessment$behav

    if (return_edges == "all") {
      edges[,, fold] <- fold_edges
    } else if (return_edges == "sum") {
      edges <- edges + fold_edges
    }
  }

  list(
    folds = folds,
    edges = edges,
    metrics = compute_fold_metrics(real, pred, folds, network),
    predictions = compute_fold_predictions(real, pred, folds, network),
    params = list(
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      bias_correct = bias_correct,
      network = network,
      return_edges = return_edges,
      na_action = na_action
    )
  )
}
