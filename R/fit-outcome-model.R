# Internal CPM outcome-model helpers.
# These functions fit and apply the second-stage outcome models for each
# prediction stream.

fit_stream_model <- function(
  construction_model,
  behav,
  prediction_stream,
  model_spec
) {
  features <- construction_stream_features(
    construction_model = construction_model,
    prediction_stream = prediction_stream
  )

  fit_outcome_model(
    features = features,
    behav = behav,
    model_spec = model_spec
  )
}

predict_stream_model <- function(
  fitted_model,
  construction_model,
  conmat_new,
  prediction_stream
) {
  features <- construction_stream_features(
    construction_model = construction_model,
    conmat_new = conmat_new,
    prediction_stream = prediction_stream
  )

  predict_outcome_model(
    fitted_model = fitted_model,
    features = features
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
  coefficients <- fit$coefficients
  prediction_coefficients <- coefficients
  prediction_coefficients[is.na(prediction_coefficients)] <- 0

  list(
    type = "lm",
    feature_names = colnames(features),
    coefficients = coefficients,
    prediction_coefficients = prediction_coefficients,
    rank = fit$rank
  )
}

predict_lm_outcome_model <- function(fitted_model, features) {
  design <- lm_design_matrix(features)
  drop(design %*% fitted_model$prediction_coefficients)
}
