#' CPM regression model specification
#'
#' Create a parsnip model specification for connectome-based predictive
#' modeling.
#'
#' @param mode The model mode. Only `"regression"` is supported.
#' @param engine The engine name. Defaults to `"cpmr"`.
#' @param thresh_method Edge-selection strategy: `"alpha"` or `"sparsity"`.
#' @param thresh_level Edge-selection threshold level.
#' @param bias_correct Should predictors be centered and scaled using training
#'   fold statistics before edge aggregation?
#' @param network Which CPM network to use as the main prediction target:
#'   `"both"`, `"pos"`, or `"neg"`.
#'
#' @return A `cpm_reg` model specification.
#' @export
#'
#' @examples
#' spec <- cpm_reg(network = "both")
#' spec
cpm_reg <- function(
  mode = "regression",
  engine = "cpmr",
  thresh_method = "alpha",
  thresh_level = 0.01,
  bias_correct = TRUE,
  network = "both"
) {
  args <- list(
    thresh_method = rlang::enquo(thresh_method),
    thresh_level = rlang::enquo(thresh_level),
    bias_correct = rlang::enquo(bias_correct),
    network = rlang::enquo(network)
  )

  parsnip::new_model_spec(
    "cpm_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    user_specified_mode = !missing(mode),
    method = NULL,
    engine = engine,
    user_specified_engine = !missing(engine)
  )
}

#' @export
update.cpm_reg <- function(
  object,
  parameters = NULL,
  thresh_method = NULL,
  thresh_level = NULL,
  bias_correct = NULL,
  network = NULL,
  fresh = FALSE,
  ...
) {
  args <- list(
    thresh_method = rlang::enquo(thresh_method),
    thresh_level = rlang::enquo(thresh_level),
    bias_correct = rlang::enquo(bias_correct),
    network = rlang::enquo(network)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "cpm_reg",
    ...
  )
}

#' Internal parsnip engine bridge for `cpm_reg()`
#'
#' @param x Predictor matrix.
#' @param y Outcome vector.
#' @param thresh_method,thresh_level,bias_correct,network CPM engine
#'   parameters.
#'
#' @return A `cpm_fit` object.
#' @export
cpm_engine_fit <- function(
  x,
  y,
  thresh_method = "alpha",
  thresh_level = 0.01,
  bias_correct = TRUE,
  network = "both"
) {
  core_fit_xy(
    conmat = x,
    behav = y,
    thresh_method = thresh_method,
    thresh_level = thresh_level,
    bias_correct = bias_correct,
    network = network
  )
}
