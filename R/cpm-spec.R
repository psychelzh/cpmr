#' Define a CPM model specification
#'
#' Create a lightweight specification object that stores the modeling
#' parameters required to fit a connectome-based predictive model later with
#' [fit()] or [fit_resamples()].
#'
#' @param association_method Association measure used during edge screening.
#'   `"pearson"` uses linear correlation and `"spearman"` uses rank-based
#'   correlation.
#' @param threshold_method,threshold_level The threshold method and level used
#'   in edge selection. With `"alpha"`, edges are selected by thresholding the
#'   absolute association against a critical value implied by
#'   `threshold_level`. With `"sparsity"`, `threshold_level` is treated as a
#'   per-sign proportion and edges are retained from the positive and negative
#'   tails separately. With `"effect_size"`, `threshold_level` is treated as a
#'   direct absolute association cutoff.
#' @param network_summary How selected positive and negative edges are turned
#'   into subject-level predictors. `"separate"` keeps the classic CPM positive
#'   and negative sums and fits a combined stream from both. `"difference"`
#'   uses a single `positive - negative` strength.
#' @param edge_weighting How edge-level statistics are converted into weights
#'   before network summarization. `"binary"` uses the hard thresholded edge
#'   mask. `"sigmoid"` uses a smooth sigmoid weight centered on the threshold,
#'   so edges closer to or beyond the cutoff contribute more strongly.
#' @param weighting_scale Positive scale parameter used when
#'   `edge_weighting = "sigmoid"`. Smaller values make the weighting curve
#'   sharper around the threshold.
#' @param prediction_head Final prediction head fit on the summarized network
#'   features. `"linear"` fits the usual intercept-inclusive linear model.
#'   `"linear_no_intercept"` fits a no-intercept linear model.
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data before model fitting and
#'   prediction. See Rapuano et al. (2020) for more details.
#'
#' @examples
#' spec <- cpm_spec(
#'   association_method = "spearman",
#'   threshold_method = "effect_size",
#'   threshold_level = 0.1,
#'   edge_weighting = "sigmoid"
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
  association_method = c("pearson", "spearman"),
  threshold_method = c("alpha", "sparsity", "effect_size"),
  threshold_level = 0.01,
  network_summary = c("separate", "difference"),
  edge_weighting = c("binary", "sigmoid"),
  weighting_scale = 0.05,
  prediction_head = c("linear", "linear_no_intercept"),
  bias_correct = TRUE
) {
  association_method <- match.arg(association_method)
  threshold_method <- match.arg(threshold_method)
  network_summary <- match.arg(network_summary)
  edge_weighting <- match.arg(edge_weighting)
  prediction_head <- match.arg(prediction_head)

  validate_cpm_spec_params(
    threshold_level = threshold_level,
    weighting_scale = weighting_scale,
    bias_correct = bias_correct
  )

  new_cpm_spec(
    params = list(
      association_method = association_method,
      threshold_method = threshold_method,
      threshold_level = threshold_level,
      network_summary = network_summary,
      edge_weighting = edge_weighting,
      weighting_scale = weighting_scale,
      prediction_head = prediction_head,
      bias_correct = bias_correct
    )
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM specification:\n")
  cat(sprintf(
    "  Association method: %s\n",
    x$params$association_method
  ))
  cat(sprintf("  Threshold method:   %s\n", x$params$threshold_method))
  cat(sprintf(
    "  Threshold level:    %s\n",
    format_threshold_level(x$params$threshold_level)
  ))
  cat(sprintf(
    "  Network summary:    %s\n",
    x$params$network_summary
  ))
  cat(sprintf(
    "  Edge weighting:     %s\n",
    x$params$edge_weighting
  ))
  cat(sprintf(
    "  Weighting scale:    %s\n",
    format_threshold_level(x$params$weighting_scale)
  ))
  cat(sprintf(
    "  Prediction head:    %s\n",
    x$params$prediction_head
  ))
  cat(sprintf(
    "  Prediction streams: %s\n",
    format_prediction_streams(
      prediction_types_for_summary(x$params$network_summary)
    )
  ))
  cat(sprintf(
    "  Bias correction:    %s\n",
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

new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

validate_cpm_spec_params <- function(
  threshold_level,
  weighting_scale,
  bias_correct
) {
  if (
    !is.numeric(threshold_level) ||
      length(threshold_level) != 1L ||
      is.na(threshold_level) ||
      !is.finite(threshold_level) ||
      threshold_level < 0 ||
      threshold_level > 1
  ) {
    stop("`threshold_level` must be a single number between 0 and 1.")
  }

  if (
    !is.numeric(weighting_scale) ||
      length(weighting_scale) != 1L ||
      is.na(weighting_scale) ||
      !is.finite(weighting_scale) ||
      weighting_scale <= 0
  ) {
    stop("`weighting_scale` must be a single positive number.")
  }

  if (
    !is.logical(bias_correct) ||
      length(bias_correct) != 1L ||
      is.na(bias_correct)
  ) {
    stop("`bias_correct` must be either TRUE or FALSE.")
  }

  invisible()
}
