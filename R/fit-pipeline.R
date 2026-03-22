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
  fitted_model <- fit_split_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_selection = edge_selection,
    construction_spec = construction_spec,
    model_spec = model_spec
  )

  if (is.null(rows_test)) {
    return(list(
      edge_selection = edge_selection,
      fitted_model = fitted_model,
      observed = training$behav,
      predictions = predict_split_model(fitted_model, training$conmat)
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
    fitted_model = fitted_model,
    observed = assessment$behav,
    predictions = predict_split_model(fitted_model, assessment$conmat)
  )
}

fit_split_model <- function(
  conmat,
  behav,
  edge_selection,
  construction_spec,
  model_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)
  model_spec <- validate_model_spec(model_spec)
  construction_state <- build_construction_state(
    conmat = conmat,
    edge_selection = edge_selection,
    construction_spec = construction_spec
  )

  outcome_models <- stats::setNames(
    lapply(
      construction_state$construction$prediction_streams,
      function(prediction_stream) {
        fit_stream_model(
          construction_state = construction_state,
          behav = behav,
          prediction_stream = prediction_stream,
          model_spec = model_spec
        )
      }
    ),
    construction_state$construction$prediction_streams
  )

  c(
    construction_state,
    list(
      edges = edge_selection$mask,
      selection_thresholds = edge_selection$thresholds,
      outcome_models = outcome_models
    )
  )
}

predict_split_model <- function(model, conmat_new) {
  features <- construction_features(
    construction_state = model,
    conmat_new = conmat_new
  )
  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(model$construction$prediction_streams),
    dimnames = list(NULL, model$construction$prediction_streams)
  )
  for (prediction_stream in model$construction$prediction_streams) {
    pred[, prediction_stream] <- predict_stream_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      construction_state = model,
      features = features
    )
  }

  pred
}

fit_stream_model <- function(
  construction_state,
  behav,
  prediction_stream,
  model_spec
) {
  features <- construction_stream_features(
    construction_state = construction_state,
    prediction_stream = prediction_stream
  )

  c(
    fit_outcome_model(
      features = features,
      behav = behav,
      model_spec = model_spec
    ),
    list(prediction_stream = prediction_stream)
  )
}

predict_stream_model <- function(
  fitted_model,
  construction_state,
  conmat_new = NULL,
  features = NULL
) {
  if (is.null(features)) {
    features <- construction_features(
      construction_state = construction_state,
      conmat_new = conmat_new
    )
  }

  stream_features <- construction_stream_features(
    construction_state = construction_state,
    prediction_stream = fitted_model$prediction_stream,
    features = features
  )

  predict_outcome_model(
    fitted_model = fitted_model,
    features = stream_features
  )
}

fit_outcome_model <- function(features, behav, model_spec) {
  switch(
    model_spec$type,
    lm = fit_lm_outcome_model(features, behav),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

predict_outcome_model <- function(fitted_model, features) {
  switch(
    fitted_model$type,
    lm = predict_lm_outcome_model(fitted_model, features),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

lm_design_matrix <- function(features) {
  features <- as.matrix(features)
  cbind("(Intercept)" = 1, features)
}

fit_lm_outcome_model <- function(features, behav) {
  design <- lm_design_matrix(features)
  fit <- stats::lm.fit(design, behav)
  prediction_coefficients <- fit$coefficients
  prediction_coefficients[is.na(prediction_coefficients)] <- 0

  list(
    type = "lm",
    feature_names = colnames(features),
    coefficients = fit$coefficients,
    prediction_coefficients = prediction_coefficients,
    rank = fit$rank
  )
}

predict_lm_outcome_model <- function(fitted_model, features) {
  design <- lm_design_matrix(features)
  drop(design %*% fitted_model$prediction_coefficients)
}
