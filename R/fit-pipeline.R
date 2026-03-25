fit_split <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  selection_spec,
  construction_spec,
  model_spec,
  rows_test = NULL
) {
  conmat_train <- conmat[rows_train, , drop = FALSE]
  behav_train <- behav[rows_train]
  conmat_test <- NULL
  behav_test <- NULL

  if (!is.null(rows_test)) {
    conmat_test <- conmat[rows_test, , drop = FALSE]
    behav_test <- behav[rows_test]
  }

  if (!is.null(covariates)) {
    covariates_train <- covariates[rows_train, , drop = FALSE]
    covariates_test <- NULL
    if (!is.null(rows_test)) {
      covariates_test <- covariates[rows_test, , drop = FALSE]
    }

    conmat_resid <- regress_covariates_by_train(
      resp_train = conmat_train,
      cov_train = covariates_train,
      resp_test = conmat_test,
      cov_test = covariates_test
    )
    behav_resid <- regress_covariates_by_train(
      resp_train = behav_train,
      cov_train = covariates_train,
      resp_test = behav_test,
      cov_test = covariates_test
    )
    conmat_train <- conmat_resid$train
    behav_train <- drop(behav_resid$train)
    if (!is.null(rows_test)) {
      conmat_test <- conmat_resid$test
      behav_test <- drop(behav_resid$test)
    }
  }

  edge_selection <- run_edge_selection(
    conmat = conmat_train,
    behav = behav_train,
    selection_spec = selection_spec
  )
  fitted_model <- fit_split_model(
    conmat = conmat_train,
    behav = behav_train,
    edge_selection = edge_selection,
    construction_spec = construction_spec,
    model_spec = model_spec
  )

  observed <- behav_train
  conmat_predict <- conmat_train
  if (!is.null(rows_test)) {
    observed <- behav_test
    conmat_predict <- conmat_test
  }

  list(
    edge_selection = edge_selection,
    fitted_model = fitted_model,
    observed = observed,
    predictions = if (is.null(rows_test)) {
      predict_split_model(fitted_model)
    } else {
      predict_split_model(fitted_model, conmat_predict)
    }
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
  construction_state$outcome_models <- outcome_models
  construction_state
}

predict_split_model <- function(model, conmat_new = NULL) {
  if (!is.null(conmat_new)) {
    if (model$construction$standardize_edges) {
      conmat_new <- fscale(
        conmat_new,
        model$center,
        model$scale
      )
    }

    model$summaries <- feature_matrix_summary(
      conmat = conmat_new,
      edge_mask = model$edge_mask,
      edge_weights = model$edge_weights
    )
  }
  feature_sets <- construction_features(model)

  pred <- matrix(
    nrow = nrow(feature_sets[[1]]),
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
