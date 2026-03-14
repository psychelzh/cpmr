normalize_inputs <- function(conmat, behav, covariates) {
  core_normalize_inputs(conmat, behav, covariates)
}

resolve_include_cases <- function(conmat, behav, covariates, na_action) {
  core_resolve_include_cases(conmat, behav, covariates, na_action)
}

resolve_kfolds <- function(kfolds, include_cases) {
  core_resolve_kfolds(kfolds, include_cases)
}

init_edges <- function(return_edges, conmat, kfolds) {
  core_init_edges(return_edges, conmat, kfolds)
}

init_pred <- function(behav) {
  core_init_pred(behav)
}

regress_covariates <- function(resp, covariates) {
  core_regress_covariates(resp, covariates)
}

regress_covariates_by_train <- function(
  resp_train,
  resp_test,
  cov_train,
  cov_test
) {
  core_regress_covariates_by_train(
    resp_train = resp_train,
    resp_test = resp_test,
    cov_train = cov_train,
    cov_test = cov_test
  )
}

select_edges <- function(conmat, behav, method, level) {
  core_select_edges(conmat, behav, method, level)
}

critical_r <- function(n, alpha) {
  core_critical_r(n, alpha)
}

fscale <- function(x, center, scale) {
  core_fscale(x, center, scale)
}

crossv_kfold <- function(x, k) {
  core_crossv_kfold(x, k)
}

validate_kfolds <- function(kfolds) {
  core_validate_kfolds(kfolds)
}

validate_resamples <- function(resamples, include_cases) {
  core_validate_resamples(resamples, include_cases)
}

maybe_warn_large_edge_storage <- function(n_edges, kfolds, return_edges) {
  core_warn_large_edge_storage(n_edges, kfolds, return_edges)
}

prepare_training_data <- function(conmat, behav, covariates, rows_train) {
  core_prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
}

prepare_assessment_data <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test,
  covariates_train = NULL
) {
  core_prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = covariates_train
  )
}

fit_cpm_single <- function(
  call,
  object,
  conmat,
  behav,
  covariates,
  return_edges,
  na_action
) {
  core_fit_single(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    return_edges = return_edges,
    na_action = na_action,
    call = call
  )
}

train_cpm_model <- function(conmat, behav, edges, bias_correct) {
  core_train_model(conmat, behav, edges, bias_correct)
}

predict_cpm_model <- function(model, conmat_new) {
  core_predict_model(model, conmat_new)
}

new_cpm <- function(call, folds, behav, pred, edges, model, spec, params) {
  core_new_cpm(call, folds, behav, pred, edges, model, spec, params)
}

new_cpm_resamples <- function(
  spec,
  folds,
  edges,
  metrics,
  predictions,
  params
) {
  core_new_cpm_resamples(spec, folds, edges, metrics, predictions, params)
}
