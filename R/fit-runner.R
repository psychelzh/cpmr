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
  return_edges = c("none", "sum", "all"),
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

init_edge_storage <- function(return_edges, conmat, n_folds) {
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
