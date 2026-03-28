run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  inputs <- prepare_runner_inputs(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "fitting"
  )

  split_fit <- fit_split(
    conmat = conmat,
    behav = inputs$behav,
    covariates = inputs$covariates,
    rows_train = inputs$include_cases,
    selection_spec = object$selection,
    construction_spec = object$construction,
    model_spec = object$model
  )
  predictions <- assemble_single_predictions(
    observed = inputs$behav,
    include_cases = inputs$include_cases,
    split_fit = split_fit
  )

  structure(
    list(
      call = call,
      spec = object,
      settings = list(
        covariates = !is.null(inputs$covariates),
        na_action = inputs$na_action
      ),
      predictions = predictions,
      edges = split_fit$edge_selection$mask,
      model = split_fit$fitted_model,
      folds = list(inputs$include_cases)
    ),
    class = "cpm"
  )
}

run_resample_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  resamples = NULL,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude"),
  call = NULL
) {
  return_edges <- match.arg(return_edges)
  inputs <- prepare_runner_inputs(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "resampling"
  )

  folds <- resolve_resample_folds(
    resamples = resamples,
    include_cases = inputs$include_cases
  )
  n_folds <- length(folds)

  warn_large_edge_storage(ncol(conmat), n_folds, return_edges)

  edges <- init_edge_storage(return_edges, conmat, n_folds)
  split_results <- vector("list", n_folds)

  for (fold in seq_len(n_folds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(inputs$include_cases, rows_test)

    split_results[[fold]] <- fit_split(
      conmat = conmat,
      behav = inputs$behav,
      covariates = inputs$covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = object$selection,
      construction_spec = object$construction,
      model_spec = object$model
    )

    edges <- update_edge_storage(
      edges = edges,
      return_edges = return_edges,
      fold = fold,
      edge_mask = split_results[[fold]]$edge_selection$mask
    )
  }

  predictions <- assemble_fold_predictions(
    observed = inputs$behav,
    folds = folds,
    split_results = split_results
  )

  structure(
    list(
      call = call,
      spec = object,
      settings = list(
        covariates = !is.null(inputs$covariates),
        na_action = inputs$na_action,
        return_edges = return_edges
      ),
      predictions = predictions,
      edges = edges,
      folds = folds
    ),
    class = "cpm"
  )
}

prepare_runner_inputs <- function(
  conmat,
  behav,
  covariates,
  na_action,
  action
) {
  na_action <- match.arg(na_action, c("fail", "exclude"))
  inputs <- normalize_inputs(conmat, behav, covariates)
  include_cases <- complete_case_rows(
    conmat = conmat,
    behav = inputs$behav,
    covariates = inputs$covariates,
    na_action = na_action
  )

  if (length(include_cases) == 0L) {
    stop(
      sprintf("No complete-case observations available for %s.", action),
      call. = FALSE
    )
  }

  list(
    behav = inputs$behav,
    covariates = inputs$covariates,
    include_cases = include_cases,
    na_action = na_action
  )
}

normalize_inputs <- function(conmat, behav, covariates = NULL) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.", call. = FALSE)
  }
  if (nrow(conmat) != length(behav)) {
    stop(
      "The number of observations in `conmat` and `behav` must match.",
      call. = FALSE
    )
  }
  if (!is.null(rownames(conmat)) && !is.null(names(behav))) {
    if (!identical(rownames(conmat), names(behav))) {
      stop(
        "Case names of `conmat` must match those of behavior data.",
        call. = FALSE
      )
    }
  }

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop(
        "The number of observations in `covariates` and `behav` must match.",
        call. = FALSE
      )
    }
    if (!is.null(rownames(covariates)) && !is.null(names(behav))) {
      if (!identical(rownames(covariates), names(behav))) {
        stop(
          "Case names of `covariates` must match those of behavior data.",
          call. = FALSE
        )
      }
    }
  }

  list(
    behav = behav,
    covariates = covariates
  )
}

complete_case_rows <- function(conmat, behav, covariates, na_action) {
  switch(
    na_action,
    fail = {
      if (anyNA(conmat)) {
        stop("Missing values found in `conmat`", call. = FALSE)
      }
      if (anyNA(behav)) {
        stop("Missing values found in `behav`", call. = FALSE)
      }
      if (!is.null(covariates) && anyNA(covariates)) {
        stop("Missing values found in `covariates`", call. = FALSE)
      }
      seq_along(behav)
    },
    exclude = {
      include_mask <- stats::complete.cases(conmat) & !is.na(behav)
      if (!is.null(covariates)) {
        include_mask <- include_mask & stats::complete.cases(covariates)
      }
      which(include_mask)
    }
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
    return(list(train = model$residuals))
  }

  x_test <- cbind(1, cov_test)
  list(
    train = model$residuals,
    test = resp_test - x_test %*% model$coefficients
  )
}

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
  if (length(rows_train) < 3L) {
    stop(
      "CPM fitting requires at least 3 training observations.",
      call. = FALSE
    )
  }

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

assemble_single_predictions <- function(
  observed,
  include_cases,
  split_fit
) {
  pred <- matrix(
    NA_real_,
    nrow = length(observed),
    ncol = ncol(split_fit$predictions),
    dimnames = list(
      prediction_row_names(observed),
      colnames(split_fit$predictions)
    )
  )
  pred[include_cases, ] <- split_fit$predictions

  observed[include_cases] <- split_fit$observed
  prediction_frame(observed, pred)
}

assemble_fold_predictions <- function(observed, folds, split_results) {
  pred <- matrix(
    NA_real_,
    nrow = length(observed),
    ncol = ncol(split_results[[1]]$predictions),
    dimnames = list(
      prediction_row_names(observed),
      colnames(split_results[[1]]$predictions)
    )
  )
  fold_id <- rep(NA_integer_, length(observed))
  for (fold in seq_along(folds)) {
    rows_test <- folds[[fold]]
    pred[rows_test, ] <- split_results[[fold]]$predictions
    observed[rows_test] <- split_results[[fold]]$observed
    fold_id[rows_test] <- fold
  }

  prediction_frame(observed, pred, fold = fold_id)
}

prediction_frame <- function(observed, pred, fold = NULL) {
  prediction_streams <- colnames(pred)
  predictions <- data.frame(
    row = seq_along(observed),
    observed = observed,
    pred,
    row.names = prediction_row_names(observed)
  )

  if (is.null(fold)) {
    return(predictions)
  }

  predictions$fold <- fold
  predictions[, c("row", "fold", "observed", prediction_streams), drop = FALSE]
}

prediction_row_names <- function(observed) {
  if (!is.null(names(observed)) && !anyDuplicated(names(observed))) {
    names(observed)
  } else {
    NULL
  }
}

prediction_columns <- function(predictions) {
  setdiff(names(predictions), c("row", "fold", "observed"))
}
