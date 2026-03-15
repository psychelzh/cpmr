core_fit_xy <- function(
  conmat,
  behav,
  thresh_method = "alpha",
  thresh_level = 0.01,
  bias_correct = TRUE,
  network = "both"
) {
  thresh_method <- core_validate_thresh_method(thresh_method)
  thresh_level <- core_validate_thresh_level(thresh_level)
  bias_correct <- core_validate_bias_correct(bias_correct)
  network <- core_validate_network(network)

  normalized <- core_normalize_inputs(conmat, behav)
  conmat <- normalized$conmat
  behav <- normalized$behav

  include_cases <- core_resolve_include_cases(
    conmat,
    behav,
    covariates = NULL,
    na_action = "fail"
  )
  if (length(include_cases) < 3L) {
    stop("At least 3 complete observations are required to fit CPM.")
  }

  edges <- core_select_edges(
    conmat = conmat,
    behav = behav,
    method = thresh_method,
    level = thresh_level
  )
  model <- core_train_model(
    conmat = conmat,
    behav = behav,
    edges = edges,
    bias_correct = bias_correct
  )

  predictor_names <- colnames(as.matrix(conmat))
  outcome_name <- names(behav)

  new_cpm_fit(
    model = model,
    edges = edges,
    network = network,
    predictors = predictor_names,
    outcome = outcome_name,
    params = list(
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      bias_correct = bias_correct
    )
  )
}

core_predict_networks <- function(object, new_data) {
  predictors <- core_prepare_prediction_matrix(new_data, object$predictors)
  core_predict_model(object$model, predictors)
}
