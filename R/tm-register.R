# nocov start
register_cpm_reg <- function() {
  model_name <- "cpm_reg"
  current_models <- parsnip::get_model_env()$models
  if (model_name %in% names(current_models)) {
    return(invisible())
  }

  parsnip::set_new_model(model_name)
  parsnip::set_model_mode(model_name, "regression")
  parsnip::set_model_engine(model_name, mode = "regression", eng = "cpmr")

  parsnip::set_model_arg(
    model = model_name,
    eng = "cpmr",
    parsnip = "thresh_method",
    original = "thresh_method",
    func = list(pkg = "cpmr", fun = "cpm_thresh_method"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = model_name,
    eng = "cpmr",
    parsnip = "thresh_level",
    original = "thresh_level",
    func = list(pkg = "dials", fun = "threshold"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = model_name,
    eng = "cpmr",
    parsnip = "bias_correct",
    original = "bias_correct",
    func = list(pkg = "cpmr", fun = "cpm_bias_correct"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = model_name,
    eng = "cpmr",
    parsnip = "network",
    original = "network",
    func = list(pkg = "cpmr", fun = "cpm_network"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = model_name,
    mode = "regression",
    eng = "cpmr",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "cpm_engine_fit"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = model_name,
    mode = "regression",
    eng = "cpmr",
    type = "numeric",
    value = parsnip::pred_value_template(
      func = c(fun = "predict"),
      object = rlang::expr(object$fit),
      new_data = rlang::expr(new_data),
      type = "numeric"
    )
  )
  parsnip::set_pred(
    model = model_name,
    mode = "regression",
    eng = "cpmr",
    type = "raw",
    value = parsnip::pred_value_template(
      func = c(fun = "predict"),
      object = rlang::expr(object$fit),
      new_data = rlang::expr(new_data),
      type = "raw"
    )
  )

  parsnip::set_encoding(
    model = model_name,
    mode = "regression",
    eng = "cpmr",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_dependency(model_name, eng = "cpmr", pkg = "cpmr")

  invisible()
}
# nocov end
