run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  params <- object$params
  prediction_streams <- prediction_streams_for_polarity(
    params$construction$polarity
  )
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

  pred <- init_pred(behav, prediction_streams)
  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = include_cases
  )

  edge_selection <- run_edge_selection(
    conmat = training$conmat,
    behav = training$behav,
    selection_method = params$selection$method,
    selection_criterion = params$selection$criterion,
    selection_level = params$selection$level
  )
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_selection = edge_selection,
    construction_spec = params$construction,
    model_spec = params$model
  )
  pred[include_cases, ] <- predict_model(model, training$conmat)

  observed <- behav
  observed[include_cases] <- training$behav
  predictions <- compute_single_predictions(observed, pred)

  new_cpm(
    call = call,
    spec = object,
    params = new_fit_params(
      spec_params = params,
      covariates = covariates,
      na_action = na_action
    ),
    predictions = predictions,
    edges = edge_selection$mask,
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
  prediction_streams <- prediction_streams_for_polarity(
    params$construction$polarity
  )
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

  pred <- init_pred(behav, prediction_streams)
  edges <- init_edges(return_edges, conmat, kfolds)
  observed <- behav

  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    training <- prepare_training_data(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train
    )
    edge_selection <- run_edge_selection(
      conmat = training$conmat,
      behav = training$behav,
      selection_method = params$selection$method,
      selection_criterion = params$selection$criterion,
      selection_level = params$selection$level
    )
    fold_model <- train_model(
      conmat = training$conmat,
      behav = training$behav,
      edge_selection = edge_selection,
      construction_spec = params$construction,
      model_spec = params$model
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
    observed[rows_test] <- assessment$behav

    if (return_edges == "all") {
      edges[,, fold] <- edge_selection$mask
    } else if (return_edges == "sum") {
      edges <- edges + edge_selection$mask
    }
  }

  predictions <- compute_fold_predictions(observed, pred, folds)

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
  utils::modifyList(
    spec_params,
    c(
      list(
        covariates = !is.null(covariates),
        na_action = na_action
      ),
      extras
    )
  )
}

init_pred <- function(behav, prediction_streams) {
  matrix(
    nrow = length(behav),
    ncol = length(prediction_streams),
    dimnames = list(names(behav), prediction_streams)
  )
}

init_edges <- function(return_edges, conmat, kfolds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), kfolds),
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
