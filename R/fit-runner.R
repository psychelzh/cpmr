run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  params <- object$params
  na_action <- match.arg(na_action)

  normalized <- normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(
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

  pred <- init_pred(behav)
  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = include_cases
  )

  cur_edges <- select_edges(
    conmat = training$conmat,
    behav = training$behav,
    method = params$thresh_method,
    level = params$thresh_level
  )
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = cur_edges,
    bias_correct = params$bias_correct
  )
  pred[include_cases, ] <- predict_model(model, training$conmat)

  real <- behav
  real[include_cases] <- training$behav

  new_cpm(
    call = call,
    behav = real,
    pred = pred,
    edges = cur_edges,
    model = model,
    spec = object,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      na_action = na_action,
      bias_correct = params$bias_correct
    )
  )
}

run_resample_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  folds,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  params <- object$params
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  normalized <- normalize_inputs(conmat, behav, covariates)
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

  folds <- validate_resamples(folds, include_cases)
  kfolds <- length(folds)

  warn_large_edge_storage(ncol(conmat), kfolds, return_edges)

  pred <- init_pred(behav)
  edges <- init_edges(return_edges, conmat, kfolds)
  real <- behav

  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    training <- prepare_training_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train
    )
    fold_edges <- select_edges(
      conmat = training$conmat,
      behav = training$behav,
      method = params$thresh_method,
      level = params$thresh_level
    )
    fold_model <- train_model(
      conmat = training$conmat,
      behav = training$behav,
      edges = fold_edges,
      bias_correct = params$bias_correct
    )
    assessment <- prepare_assessment_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      covariates_train = training$covariates
    )

    pred[rows_test, ] <- predict_model(fold_model, assessment$conmat)
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
