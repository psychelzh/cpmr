run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  runner_state <- resolve_runner_state(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "fitting",
    min_cases = 3L
  )
  params <- runner_state$params
  behav <- runner_state$behav
  covariates <- runner_state$covariates
  include_cases <- runner_state$include_cases
  na_action <- runner_state$na_action

  pred_matrix <- init_pred(behav, runner_state$prediction_streams)
  split_fit <- run_fit_split(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = include_cases,
    selection_spec = params$selection,
    construction_spec = runner_state$construction_spec,
    model_spec = params$model
  )
  pred_matrix[include_cases, ] <- split_fit$predictions

  observed <- behav
  observed[include_cases] <- split_fit$observed
  predictions <- compute_single_predictions(observed, pred_matrix)

  new_cpm(
    call = call,
    spec = object,
    params = new_fit_params(
      spec_params = params,
      covariates = covariates,
      na_action = na_action
    ),
    predictions = predictions,
    edges = split_fit$edge_selection$mask,
    model = split_fit$fitted_model
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
  return_edges <- match.arg(return_edges)
  runner_state <- resolve_runner_state(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "resampling",
    min_cases = 2L,
    fit_context = fit_context
  )
  params <- runner_state$params
  behav <- runner_state$behav
  covariates <- runner_state$covariates
  include_cases <- runner_state$include_cases
  na_action <- runner_state$na_action

  folds <- assert_normalized_resample_folds(folds)
  n_folds <- length(folds)

  warn_large_edge_storage(ncol(conmat), n_folds, return_edges)

  pred_matrix <- init_pred(behav, runner_state$prediction_streams)
  edges <- init_edges(return_edges, conmat, n_folds)
  observed <- behav

  for (fold in seq_len(n_folds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    split_fit <- run_fit_split(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = params$selection,
      construction_spec = runner_state$construction_spec,
      model_spec = params$model
    )

    pred_matrix[rows_test, ] <- split_fit$predictions
    observed[rows_test] <- split_fit$observed
    edges <- update_edge_storage(
      edges = edges,
      return_edges = return_edges,
      fold = fold,
      edge_mask = split_fit$edge_selection$mask
    )
  }

  predictions <- compute_fold_predictions(observed, pred_matrix, folds)

  new_cpm_resamples(
    call = call,
    spec = object,
    params = new_fit_params(
      spec_params = params,
      covariates = covariates,
      na_action = na_action,
      extras = list(
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

resolve_runner_state <- function(
  object,
  conmat,
  behav,
  covariates,
  na_action,
  action,
  min_cases,
  fit_context = NULL
) {
  params <- object$params
  construction_spec <- params$construction

  if (is.null(fit_context)) {
    fit_context <- resolve_fit_context(
      conmat = conmat,
      behav = behav,
      covariates = covariates,
      na_action = na_action,
      action = action,
      min_cases = min_cases
    )
  }

  list(
    params = params,
    construction_spec = construction_spec,
    prediction_streams = construction_prediction_streams(construction_spec),
    behav = fit_context$behav,
    covariates = fit_context$covariates,
    include_cases = fit_context$include_cases,
    na_action = fit_context$na_action
  )
}

init_pred <- function(behav, prediction_streams) {
  matrix(
    nrow = length(behav),
    ncol = length(prediction_streams),
    dimnames = list(names(behav), prediction_streams)
  )
}

init_edges <- function(return_edges, conmat, n_folds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), n_folds),
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

update_edge_storage <- function(edges, return_edges, fold, edge_mask) {
  switch(
    return_edges,
    all = {
      edges[,, fold] <- edge_mask
      edges
    },
    sum = edges + edge_mask,
    none = edges
  )
}
