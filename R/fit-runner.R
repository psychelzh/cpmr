run_single_fit <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  na_action = c("fail", "exclude"),
  call = NULL
) {
  context <- resolve_data_context(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action
  )
  require_single_fit_cases(context$include_cases)

  pred_matrix <- init_pred(
    context$behav,
    construction_prediction_streams(object$construction)
  )
  split_fit <- run_fit_split(
    conmat = conmat,
    behav = context$behav,
    covariates = context$covariates,
    rows_train = context$include_cases,
    selection_spec = object$selection,
    construction_spec = object$construction,
    model_spec = object$model
  )
  pred_matrix[context$include_cases, ] <- split_fit$predictions

  observed <- context$behav
  observed[context$include_cases] <- split_fit$observed
  predictions <- compute_single_predictions(observed, pred_matrix)

  new_cpm(
    call = call,
    spec = object,
    settings = list(
      covariates = !is.null(context$covariates),
      na_action = context$na_action
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
  resamples = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude"),
  call = NULL
) {
  return_edges <- match.arg(return_edges)
  context <- resolve_data_context(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action
  )
  require_resample_cases(context$include_cases)

  folds <- resolve_resample_folds(
    resamples = resamples,
    include_cases = context$include_cases
  )$folds
  folds <- assert_normalized_resample_folds(folds)
  n_folds <- length(folds)

  warn_large_edge_storage(ncol(conmat), n_folds, return_edges)

  pred_matrix <- init_pred(
    context$behav,
    construction_prediction_streams(object$construction)
  )
  edges <- init_edges(return_edges, conmat, n_folds)
  observed <- context$behav

  for (fold in seq_len(n_folds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(context$include_cases, rows_test)

    split_fit <- run_fit_split(
      conmat = conmat,
      behav = context$behav,
      covariates = context$covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = object$selection,
      construction_spec = object$construction,
      model_spec = object$model
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
    settings = list(
      covariates = !is.null(context$covariates),
      na_action = context$na_action,
      return_edges = return_edges
    ),
    predictions = predictions,
    edges = edges,
    folds = folds
  )
}

require_single_fit_cases <- function(include_cases) {
  require_complete_cases_for(
    include_cases = include_cases,
    min_cases = 3L,
    action = "fitting"
  )
}

require_resample_cases <- function(include_cases) {
  require_complete_cases_for(
    include_cases = include_cases,
    min_cases = 2L,
    action = "resampling"
  )
}

require_complete_cases_for <- function(include_cases, min_cases, action) {
  if (length(include_cases) == 0L) {
    stop(sprintf("No complete-case observations available for %s.", action))
  }
  if (length(include_cases) < min_cases) {
    stop(sprintf(
      "At least %d complete-case observations are required for %s.",
      min_cases,
      action
    ))
  }

  invisible(include_cases)
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
