run_fit_split <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  selection_spec,
  construction_spec,
  model_spec,
  rows_test = NULL
) {
  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  edge_selection <- run_edge_selection(
    conmat = training$conmat,
    behav = training$behav,
    selection_spec = selection_spec
  )
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_selection = edge_selection,
    construction_spec = construction_spec,
    model_spec = model_spec
  )

  if (is.null(rows_test)) {
    return(list(
      edge_selection = edge_selection,
      fitted_model = model,
      observed = training$behav,
      predictions = predict_model(model, training$conmat)
    ))
  }

  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )

  list(
    edge_selection = edge_selection,
    fitted_model = model,
    observed = assessment$behav,
    predictions = predict_model(model, assessment$conmat)
  )
}

train_model <- function(
  conmat,
  behav,
  edge_selection,
  construction_spec,
  model_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)
  model_spec <- validate_model_spec(model_spec)
  construction_model <- build_construction_model(
    conmat = conmat,
    edge_selection = edge_selection,
    construction_spec = construction_spec
  )

  outcome_models <- lapply(
    construction_model$prediction_streams,
    function(
      prediction_stream
    ) {
      fit_stream_model(
        construction_model = construction_model,
        behav = behav,
        prediction_stream = prediction_stream,
        model_spec = model_spec
      )
    }
  )
  names(outcome_models) <- construction_model$prediction_streams

  utils::modifyList(
    construction_model,
    list(
      edges = edge_selection$mask,
      selection_thresholds = edge_selection$thresholds,
      outcome_models = outcome_models
    )
  )
}

predict_model <- function(model, conmat_new) {
  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(model$prediction_streams),
    dimnames = list(NULL, model$prediction_streams)
  )
  for (prediction_stream in model$prediction_streams) {
    pred[, prediction_stream] <- predict_stream_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      construction_model = model,
      conmat_new = conmat_new,
      prediction_stream = prediction_stream
    )
  }

  pred
}
