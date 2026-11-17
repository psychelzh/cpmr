fit_split <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test,
  selection_spec,
  construction_spec,
  model_spec
) {
  prepared <- prepare_split_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test
  )

  fit_prepared_split(
    prepared = prepared,
    selection_spec = selection_spec,
    construction_spec = construction_spec,
    model_spec = model_spec
  )
}

prepare_split_data <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test
) {
  if (length(rows_train) < 3L) {
    stop(
      "CPM fitting requires at least 3 training observations.",
      call. = FALSE
    )
  }

  conmat_train <- conmat[rows_train, , drop = FALSE]
  behav_train <- behav[rows_train]
  conmat_test <- conmat[rows_test, , drop = FALSE]
  behav_test <- behav[rows_test]

  covariate_rank <- 0L
  if (!is.null(covariates)) {
    covariates_train <- covariates[rows_train, , drop = FALSE]
    covariates_test <- covariates[rows_test, , drop = FALSE]

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
    conmat_test <- conmat_resid$test
    behav_test <- drop(behav_resid$test)
    covariate_rank <- conmat_resid$rank - 1L
  }

  list(
    conmat_train = conmat_train,
    behav_train = behav_train,
    conmat_test = conmat_test,
    behav_test = behav_test,
    covariate_rank = covariate_rank
  )
}

fit_prepared_split <- function(
  prepared,
  selection_spec,
  construction_spec,
  model_spec,
  associations = NULL
) {
  edge_selection <- if (is.null(associations)) {
    run_edge_selection(
      conmat = prepared$conmat_train,
      behav = prepared$behav_train,
      selection_spec = selection_spec,
      covariate_rank = prepared$covariate_rank
    )
  } else {
    select_edges_from_associations(
      associations = associations,
      n_observations = nrow(prepared$conmat_train),
      selection_spec = selection_spec,
      covariate_rank = prepared$covariate_rank
    )
  }

  fitted_model <- fit_split_model(
    conmat = prepared$conmat_train,
    behav = prepared$behav_train,
    edge_selection = edge_selection,
    construction_spec = construction_spec,
    model_spec = model_spec
  )

  list(
    edge_mask = edge_selection$mask,
    observed = prepared$behav_test,
    predictions = predict_split_model(fitted_model, prepared$conmat_test)
  )
}

regress_covariates_by_train <- function(
  resp_train,
  cov_train,
  resp_test = NULL,
  cov_test = NULL
) {
  x_train <- cbind(1, cov_train)
  model <- stats::.lm.fit(x_train, resp_train)

  if (is.null(resp_test)) {
    return(list(train = model$residuals, rank = model$rank))
  }

  x_test <- cbind(1, cov_test)[, model$pivot, drop = FALSE]
  list(
    train = model$residuals,
    test = resp_test - x_test %*% model$coefficients,
    rank = model$rank
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

predict_split_model <- function(model, conmat_new) {
  model$summaries <- feature_matrix_summary(
    conmat = conmat_new,
    edge_mask = model$edge_mask,
    edge_weights = model$edge_weights,
    center = model$center,
    scale = model$scale
  )
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
