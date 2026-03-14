core_normalize_inputs <- function(conmat, behav, covariates) {
  normalize_inputs(conmat, behav, covariates)
}

core_resolve_include_cases <- function(conmat, behav, covariates, na_action) {
  resolve_include_cases(conmat, behav, covariates, na_action)
}

core_resolve_kfolds <- function(kfolds, include_cases) {
  resolve_kfolds(kfolds, include_cases)
}

core_init_edges <- function(return_edges, conmat, kfolds) {
  init_edges(return_edges, conmat, kfolds)
}

core_init_pred <- function(behav) {
  init_pred(behav)
}

core_regress_covariates_by_train <- function(
  resp_train,
  resp_test,
  cov_train,
  cov_test
) {
  regress_covariates_by_train(
    resp_train = resp_train,
    resp_test = resp_test,
    cov_train = cov_train,
    cov_test = cov_test
  )
}

core_select_edges <- function(conmat, behav, method, level) {
  select_edges(conmat, behav, method, level)
}

core_train_model <- function(conmat, behav, edges, bias_correct) {
  train_cpm_model(
    conmat = conmat,
    behav = behav,
    edges = edges,
    bias_correct = bias_correct
  )
}

core_predict_model <- function(model, conmat_new) {
  predict_cpm_model(model = model, conmat_new = conmat_new)
}

core_prepare_training_data <- function(conmat, behav, covariates, rows_train) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows_train, , drop = FALSE],
      behav = behav[rows_train],
      covariates = NULL
    ))
  }

  covariates_train <- covariates[rows_train, , drop = FALSE]
  list(
    conmat = regress_covariates(
      conmat[rows_train, , drop = FALSE],
      covariates_train
    ),
    behav = drop(regress_covariates(
      behav[rows_train],
      covariates_train
    )),
    covariates = covariates_train
  )
}

core_prepare_assessment_data <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test,
  covariates_train = NULL
) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows_test, , drop = FALSE],
      behav = behav[rows_test]
    ))
  }

  if (is.null(covariates_train)) {
    covariates_train <- covariates[rows_train, , drop = FALSE]
  }
  covariates_test <- covariates[rows_test, , drop = FALSE]

  conmat_regressed <- core_regress_covariates_by_train(
    conmat[rows_train, , drop = FALSE],
    conmat[rows_test, , drop = FALSE],
    covariates_train,
    covariates_test
  )
  behav_regressed <- core_regress_covariates_by_train(
    behav[rows_train],
    behav[rows_test],
    covariates_train,
    covariates_test
  )

  list(
    conmat = conmat_regressed$test,
    behav = drop(behav_regressed$test)
  )
}

core_fit_single <- function(
  call,
  object,
  conmat,
  behav,
  covariates,
  return_edges,
  na_action
) {
  core_fit_single_impl(
    call = call,
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    return_edges = return_edges,
    na_action = na_action
  )
}

core_fit_single_impl <- function(
  call,
  object,
  conmat,
  behav,
  covariates,
  return_edges,
  na_action
) {
  params <- object$params

  normalized <- core_normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- core_resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action
  )
  if (length(include_cases) == 0L) {
    stop("No complete-case observations available for fitting.")
  }
  if (length(include_cases) < 3L) {
    stop("At least 3 complete-case observations are required for fitting.")
  }

  pred <- core_init_pred(behav)
  training <- core_prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = include_cases
  )

  cur_edges <- core_select_edges(
    conmat = training$conmat,
    behav = training$behav,
    method = params$thresh_method,
    level = params$thresh_level
  )
  model <- core_train_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = cur_edges,
    bias_correct = params$bias_correct
  )
  pred[include_cases, ] <- core_predict_model(model, training$conmat)

  edges <- switch(
    return_edges,
    none = NULL,
    sum = cur_edges,
    all = {
      edge_array <- array(
        dim = c(dim(cur_edges), 1L),
        dimnames = list(NULL, corr_types, NULL)
      )
      edge_array[,, 1] <- cur_edges
      edge_array
    }
  )

  real <- behav
  real[include_cases] <- training$behav

  new_cpm(
    call = call,
    folds = list(include_cases),
    behav = real,
    pred = pred,
    edges = edges,
    model = model,
    spec = object,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      return_edges = return_edges,
      na_action = na_action,
      bias_correct = params$bias_correct
    )
  )
}

core_fit_resamples <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  params <- object$params
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  normalized <- core_normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- core_resolve_include_cases(
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

  if (is.null(resamples)) {
    kfolds <- core_resolve_kfolds(validate_kfolds(kfolds), include_cases)
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

  maybe_warn_large_edge_storage(ncol(conmat), kfolds, return_edges)

  pred <- core_init_pred(behav)
  edges <- core_init_edges(return_edges, conmat, kfolds)
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
      method = params$thresh_method,
      level = params$thresh_level
    )
    fold_model <- core_train_model(
      conmat = training$conmat,
      behav = training$behav,
      edges = fold_edges,
      bias_correct = params$bias_correct
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

  metrics <- compute_fold_metrics(real, pred, folds)
  predictions <- compute_fold_predictions(real, pred, folds)

  new_cpm_resamples(
    spec = object,
    folds = folds,
    edges = edges,
    metrics = metrics,
    predictions = predictions,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      kfolds = kfolds,
      bias_correct = params$bias_correct,
      return_edges = return_edges,
      na_action = na_action
    )
  )
}
