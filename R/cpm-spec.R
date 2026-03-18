#' Define a CPM model specification
#'
#' Create a lightweight specification object that stores the modeling
#' parameters required to fit a connectome-based predictive model later with
#' [fit()] or [fit_resamples()].
#'
#' `cpm_spec()` keeps the main CPM decisions visible at the top level while
#' grouping naturally paired settings into helpers:
#'
#' - [cpm_screen()] bundles the edge association measure with its threshold rule.
#' - [cpm_weighting()] bundles the edge-weighting method with its scale.
#' - [cpm_model_lm()] defines the outcome model fitted on CPM-derived features.
#'
#' @param screen Edge-screening helper created by [cpm_screen()].
#' @param feature_space How screened edges are turned into subject-level
#'   predictors after CPM edge selection. Positive edges are edges whose
#'   screening association with the outcome is positive and passes the threshold;
#'   negative edges are the corresponding negatively associated edges.
#'   `"separate"` constructs a positive strength and a negative strength for
#'   each subject, fits a `joint` stream from both together, and also returns
#'   `positive` and `negative` single-strength diagnostic streams. `"net"`
#'   constructs one `net_strength = positive_strength - negative_strength`
#'   feature and returns a single `net` stream.
#' @param weighting Edge-weighting helper created by [cpm_weighting()].
#' @param model Outcome-model helper created by [cpm_model_lm()]. This stage
#'   maps CPM-derived subject-level features to the behavioral outcome.
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data before model fitting and
#'   prediction. See Rapuano et al. (2020) for more details.
#'
#' @examples
#' spec <- cpm_spec(
#'   screen = cpm_screen(
#'     association = "spearman",
#'     threshold = cpm_threshold("effect_size", level = 0.1)
#'   ),
#'   feature_space = "net",
#'   weighting = cpm_weighting("sigmoid", scale = 0.03),
#'   model = cpm_model_lm()
#' )
#' spec
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' fit_obj <- fit(spec, conmat = conmat, behav = behav)
#' summary(fit_obj)
#'
#' resample_obj <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)
#' summary(resample_obj)
#' @export
cpm_spec <- function(
  screen = cpm_screen(),
  feature_space = c("separate", "net"),
  weighting = cpm_weighting(),
  model = cpm_model_lm(),
  bias_correct = TRUE
) {
  feature_space <- match.arg(feature_space)
  validate_cpm_component(
    screen,
    class = "cpm_screen_spec",
    message = "`screen` must be created by `cpm_screen()`."
  )
  validate_cpm_component(
    weighting,
    class = "cpm_weighting_spec",
    message = "`weighting` must be created by `cpm_weighting()`."
  )
  validate_cpm_component(
    model,
    class = "cpm_model_spec",
    message = "`model` must be created by `cpm_model_lm()`."
  )
  validate_bias_correct(bias_correct)

  new_cpm_spec(
    params = list(
      association_method = screen$association,
      threshold_method = screen$threshold$method,
      threshold_level = screen$threshold$level,
      feature_space = feature_space,
      edge_weighting = weighting$method,
      weighting_scale = weighting$scale,
      model = model$type,
      bias_correct = bias_correct
    ),
    helpers = list(
      screen = screen,
      weighting = weighting,
      model = model
    )
  )
}

#' Define CPM edge-screening settings
#'
#' Build the screening portion of a `cpm_spec()`. This stage chooses the
#' association measure used during edge screening and the threshold rule used to
#' keep positive and negative edges.
#'
#' @param association Association measure used during edge screening.
#'   `"pearson"` uses linear correlation and `"spearman"` uses rank-based
#'   correlation.
#' @param threshold Threshold helper created by [cpm_threshold()].
#'
#' @examples
#' cpm_screen(
#'   association = "spearman",
#'   threshold = cpm_threshold("effect_size", level = 0.1)
#' )
#' @export
cpm_screen <- function(
  association = c("pearson", "spearman"),
  threshold = cpm_threshold()
) {
  association <- match.arg(association)
  validate_cpm_component(
    threshold,
    class = "cpm_threshold_spec",
    message = "`threshold` must be created by `cpm_threshold()`."
  )

  structure(
    list(
      association = association,
      threshold = threshold
    ),
    class = "cpm_screen_spec"
  )
}

#' Define CPM edge-threshold settings
#'
#' Build the thresholding rule used during the screening stage.
#'
#' @param method Threshold method used in edge selection. With `"alpha"`, edges
#'   are selected by thresholding the absolute association against a critical
#'   value implied by `level`. With `"sparsity"`, `level` is treated as a
#'   per-sign proportion and edges are retained from the positive and negative
#'   tails separately. With `"effect_size"`, `level` is treated as a direct
#'   absolute association cutoff.
#' @param level Numeric threshold level associated with `method`.
#'
#' @examples
#' cpm_threshold("alpha", level = 0.05)
#' cpm_threshold("sparsity", level = 0.1)
#' @export
cpm_threshold <- function(
  method = c("alpha", "sparsity", "effect_size"),
  level = 0.01
) {
  method <- match.arg(method)
  validate_threshold_level(level)

  structure(
    list(
      method = method,
      level = level
    ),
    class = "cpm_threshold_spec"
  )
}

#' Define CPM edge-weight settings
#'
#' Build the edge-weighting rule used during CPM feature construction.
#'
#' @param method How edge-level statistics are converted into weights before
#'   CPM feature construction. `"binary"` uses the hard thresholded edge mask.
#'   `"sigmoid"` uses a smooth sigmoid weight centered on the threshold, so
#'   edges closer to or beyond the cutoff contribute more strongly.
#' @param scale Positive scale parameter used when `method = "sigmoid"`.
#'   Smaller values make the weighting curve sharper around the threshold.
#'
#' @examples
#' cpm_weighting("binary")
#' cpm_weighting("sigmoid", scale = 0.03)
#' @export
cpm_weighting <- function(
  method = c("binary", "sigmoid"),
  scale = 0.05
) {
  method <- match.arg(method)
  validate_weighting_scale(scale)

  structure(
    list(
      method = method,
      scale = scale
    ),
    class = "cpm_weighting_spec"
  )
}

#' Define the outcome model used after CPM feature construction
#'
#' Build the second-stage model used after CPM has converted selected edges
#' into subject-level predictors. `cpm_model_lm()` fits an intercept-inclusive
#' linear regression with the CPM-derived features for each prediction stream.
#'
#' @examples
#' cpm_model_lm()
#' @export
cpm_model_lm <- function() {
  structure(
    list(type = "lm"),
    class = "cpm_model_spec"
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM specification:\n")
  cat("  Screening:\n")
  cat(sprintf(
    "    Association:      %s\n",
    x$params$association_method
  ))
  cat(sprintf(
    "    Threshold method: %s\n",
    x$params$threshold_method
  ))
  cat(sprintf(
    "    Threshold level:  %s\n",
    format_threshold_level(x$params$threshold_level)
  ))
  cat("  Model:\n")
  cat(sprintf(
    "    Feature space:    %s\n",
    x$params$feature_space
  ))
  cat(sprintf(
    "    Edge weighting:   %s\n",
    x$params$edge_weighting
  ))
  cat(sprintf(
    "    Weighting scale:  %s\n",
    format_threshold_level(x$params$weighting_scale)
  ))
  cat(sprintf(
    "    Outcome model:    %s\n",
    format_model_type(x$params$model)
  ))
  cat(sprintf(
    "    Streams:          %s\n",
    format_prediction_streams(
      prediction_streams_for_feature_space(x$params$feature_space)
    )
  ))
  cat(sprintf(
    "    Bias correction:  %s\n",
    format_yes_no(x$params$bias_correct)
  ))
  invisible(x)
}

#' Fit a CPM model specification
#'
#' @rdname cpm_spec
#' @param object A `cpm_spec` object.
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column.
#' @param behav A numeric outcome vector with one value per observation in
#'   `conmat`. Row or column matrices are accepted and converted with [drop()].
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Vectors are converted to
#'   single-column matrices.
#' @param na_action A character string indicating the action when missing values
#'   are found in the inputs. `"fail"` stops immediately when any required value
#'   is missing. `"exclude"` fits on complete cases and keeps the original row
#'   layout in the returned predictions.
#' @export
fit.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  call[[1]] <- quote(fit)

  run_single_fit(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    call = call
  )
}

#' Fit a CPM model specification on resamples
#'
#' @rdname cpm_spec
#' @param resamples Optional list of assessment indices defining resamples.
#'   Each element must be an integer vector indexing rows in `conmat`. If
#'   `NULL`, folds are generated from `kfolds`.
#' @param kfolds Number of folds used when `resamples` is `NULL`. If `NULL`,
#'   it is set to the number of complete-case observations (LOOCV).
#' @param return_edges A character string indicating the return value of the
#'   selected edges. `"none"` skips edge storage, `"sum"` stores fold counts for
#'   each selected edge, and `"all"` stores the fold-wise edge masks.
#' @param na_action A character string indicating the action when missing values
#'   are found in the inputs. `"fail"` stops immediately when any required value
#'   is missing. `"exclude"` fits on complete cases and keeps the original row
#'   layout in the returned predictions.
#'
#' @return
#' `cpm_spec()` returns a `cpm_spec` object that can be reused across calls to
#' [fit()] and [fit_resamples()].
#'
#' `fit()` returns a `cpm` object from a single in-sample fit. Single-fit CPM
#' objects always store the selected edge mask.
#'
#' `fit_resamples()` returns a `cpm_resamples` object containing
#' observation-level predictions, resampling folds, and optional stored edges.
#' Call [summary.cpm_resamples()] for the default aggregate report, or
#' [resample_metrics()] when you want pooled or fold-wise metrics in tabular
#' form.
#' @export
fit_resamples.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  resamples = NULL,
  kfolds = NULL,
  return_edges = c("none", "sum", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  call[[1]] <- quote(fit_resamples)
  return_edges <- match.arg(return_edges)
  fit_context <- resolve_fit_context(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    action = "resampling",
    min_cases = 2L
  )

  resolved <- resolve_resample_folds(
    resamples = resamples,
    kfolds = kfolds,
    include_cases = fit_context$include_cases
  )

  run_resample_fit(
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    folds = resolved$folds,
    return_edges = return_edges,
    na_action = fit_context$na_action,
    fit_context = fit_context,
    call = call
  )
}

new_cpm_spec <- function(params, helpers = cpm_helpers_from_params(params)) {
  structure(
    list(
      params = params,
      helpers = helpers
    ),
    class = "cpm_spec"
  )
}

cpm_helpers_from_params <- function(params) {
  list(
    screen = cpm_screen(
      association = params$association_method,
      threshold = cpm_threshold(
        method = params$threshold_method,
        level = params$threshold_level
      )
    ),
    weighting = cpm_weighting(
      method = params$edge_weighting,
      scale = params$weighting_scale
    ),
    model = cpm_model_from_params(params$model)
  )
}

cpm_model_from_params <- function(model_type) {
  switch(
    model_type,
    lm = cpm_model_lm(),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

validate_cpm_component <- function(x, class, message) {
  if (!inherits(x, class)) {
    stop(message, call. = FALSE)
  }

  invisible(x)
}

validate_threshold_level <- function(level) {
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level) ||
      level < 0 ||
      level > 1
  ) {
    stop("`level` must be a single number between 0 and 1.", call. = FALSE)
  }

  invisible(level)
}

validate_weighting_scale <- function(scale) {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      !is.finite(scale) ||
      scale <= 0
  ) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }

  invisible(scale)
}

validate_bias_correct <- function(bias_correct) {
  if (
    !is.logical(bias_correct) ||
      length(bias_correct) != 1L ||
      is.na(bias_correct)
  ) {
    stop("`bias_correct` must be either TRUE or FALSE.", call. = FALSE)
  }

  invisible(bias_correct)
}

format_model_type <- function(model_type) {
  switch(
    model_type,
    lm = "linear regression",
    model_type
  )
}
