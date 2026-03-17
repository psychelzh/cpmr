run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  params <- object$params
  prediction_types <- prediction_types_for_feature_space(params$feature_space)
  fit_context <- resolve_fit_context(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "fitting",
    min_cases = 3L
  )
  behav <- fit_context$behav
  covariates <- fit_context$covariates
  include_cases <- fit_context$include_cases
  na_action <- fit_context$na_action

  pred <- init_pred(behav, prediction_types)
  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = include_cases
  )

  edge_screen <- screen_edges(
    conmat = training$conmat,
    behav = training$behav,
    association_method = params$association_method,
    threshold_method = params$threshold_method,
    threshold_level = params$threshold_level,
    edge_weighting = params$edge_weighting,
    weighting_scale = params$weighting_scale
  )
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_screen = edge_screen,
    bias_correct = params$bias_correct,
    feature_space = params$feature_space,
    model_spec = object$helpers$model
  )
  pred[include_cases, ] <- predict_model(model, training$conmat)

  real <- behav
  real[include_cases] <- training$behav
  predictions <- compute_single_predictions(real, pred)

  new_cpm(
    call = call,
    spec = object,
    params = new_fit_params(
      spec_params = params,
      covariates = covariates,
      na_action = na_action
    ),
    predictions = predictions,
    edges = edge_screen$mask,
    model = model
  )
}

run_resample_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  folds,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude"),
  fit_context = NULL,
  call = NULL
) {
  params <- object$params
  prediction_types <- prediction_types_for_feature_space(params$feature_space)
  return_edges <- match.arg(return_edges)
  if (is.null(fit_context)) {
    fit_context <- resolve_fit_context(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      na_action = na_action,
      action = "resampling",
      min_cases = 2L
    )
  }
  behav <- fit_context$behav
  covariates <- fit_context$covariates
  include_cases <- fit_context$include_cases
  na_action <- fit_context$na_action

  folds <- validate_resamples(folds, include_cases)
  kfolds <- length(folds)

  warn_large_edge_storage(ncol(conmat), kfolds, return_edges)

  pred <- init_pred(behav, prediction_types)
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
    fold_screen <- screen_edges(
      conmat = training$conmat,
      behav = training$behav,
      association_method = params$association_method,
      threshold_method = params$threshold_method,
      threshold_level = params$threshold_level,
      edge_weighting = params$edge_weighting,
      weighting_scale = params$weighting_scale
    )
    fold_model <- train_model(
      conmat = training$conmat,
      behav = training$behav,
      edge_screen = fold_screen,
      bias_correct = params$bias_correct,
      feature_space = params$feature_space,
      model_spec = object$helpers$model
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
      edges[,, fold] <- fold_screen$mask
    } else if (return_edges == "sum") {
      edges <- edges + fold_screen$mask
    }
  }

  predictions <- compute_fold_predictions(real, pred, folds)

  new_cpm_resamples(
    call = call,
    spec = object,
    params = new_fit_params(
      spec_params = params,
      covariates = covariates,
      na_action = na_action,
      extras = list(
        kfolds = kfolds,
        return_edges = return_edges
      )
    ),
    predictions = predictions,
    edges = edges,
    folds = folds
  )
}

new_fit_params <- function(
  spec_params,
  covariates,
  na_action,
  extras = list()
) {
  c(
    list(
      covariates = !is.null(covariates),
      association_method = spec_params$association_method,
      threshold_method = spec_params$threshold_method,
      threshold_level = spec_params$threshold_level,
      feature_space = spec_params$feature_space,
      edge_weighting = spec_params$edge_weighting,
      weighting_scale = spec_params$weighting_scale,
      model = spec_params$model,
      na_action = na_action,
      bias_correct = spec_params$bias_correct
    ),
    extras
  )
}

init_pred <- function(behav, prediction_types) {
  matrix(
    nrow = length(behav),
    ncol = length(prediction_types),
    dimnames = list(names(behav), prediction_types)
  )
}

init_edges <- function(return_edges, conmat, kfolds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_types), kfolds),
      dimnames = list(NULL, edge_types, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_types)),
      dimnames = list(NULL, edge_types)
    ),
    none = NULL
  )
}
