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
  construction_state <- build_construction_state(
    conmat = conmat,
    edge_selection = edge_selection,
    construction_spec = construction_spec
  )
  feature_sets <- construction_features(construction_state)

  outcome_models <- lapply(
    feature_sets,
    fit_outcome_model,
    behav = behav,
    model_spec = model_spec
  )

  construction_state$edges <- edge_selection$mask
  construction_state$selection_thresholds <- edge_selection$thresholds
  construction_state$outcome_models <- outcome_models
  construction_state
}

predict_split_model <- function(model, conmat_new) {
  feature_sets <- construction_features(
    construction_state = model,
    conmat_new = conmat_new
  )
  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(feature_sets),
    dimnames = list(NULL, names(feature_sets))
  )
  for (prediction_stream in names(feature_sets)) {
    pred[, prediction_stream] <- predict_outcome_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      features = feature_sets[[prediction_stream]]
    )
  }

  pred
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
