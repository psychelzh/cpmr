#' @importFrom generics fit
#' @export
generics::fit

#' Define a CPM model specification
#'
#' Create a lightweight specification object that stores the modeling
#' parameters required to fit a connectome-based predictive model later with
#' [fit()].
#'
#' @param thresh_method,thresh_level The threshold method and level used in edge
#'   selection. If method is set to be `"alpha"`, the edge selection is based on
#'   the critical value of correlation coefficient. If method is set to be
#'   `"sparsity"`, the edge selection is based on the quantile of correlation
#'   coefficient, thus network sparsity is controlled.
#' @param kfolds Folds number of cross-validation. If `NULL`, it will be set to
#'   be equal to the number of observations, i.e., leave-one-subject-out.
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data before model fitting and
#'   prediction. See Rapuano et al. (2020) for more details.
#' @param return_edges A character string indicating the return value of the
#'   selected edges. If `"none"`, no edges are returned. If `"sum"`, the sum of
#'   selected edges across folds is returned. If `"all"`, the selected edges for
#'   each fold is returned, which is a 3D array and memory-consuming.
#' @param na_action A character string indicating the action when missing values
#'   are found in `behav`. If `"fail"`, an error will be thrown. If `"exclude"`,
#'   missing values will be excluded from the analysis but kept in the output.
#'
#' @return A `cpm_spec` object storing parameters for later fitting.
#'
#' @examples
#' spec <- cpm_spec(kfolds = 10, return_edges = "sum")
#' spec
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' fit(spec, conmat = conmat, behav = behav)
#' @export
cpm_spec <- function(
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  kfolds = NULL,
  bias_correct = TRUE,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  validate_cpm_spec_params(
    thresh_level = thresh_level,
    kfolds = kfolds,
    bias_correct = bias_correct
  )

  thresh_method <- match.arg(thresh_method)
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  new_cpm_spec(
    params = list(
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      kfolds = kfolds,
      bias_correct = bias_correct,
      return_edges = return_edges,
      na_action = na_action
    )
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM model specification:\n")
  cat(sprintf("  Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("  Threshold level:  %.2f\n", x$params$thresh_level))
  cat(sprintf("  CV folds:         %s\n", x$params$kfolds %||% "auto"))
  cat(sprintf("  Bias correction:  %s\n", x$params$bias_correct))
  cat(sprintf("  Return edges:     %s\n", x$params$return_edges))
  cat(sprintf("  NA action:        %s\n", x$params$na_action))
  invisible(x)
}

#' Fit a CPM model specification
#'
#' @rdname cpm_spec
#' @param object A `cpm_spec` object.
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column (assumed that duplicated edges are removed).
#' @param behav A numeric vector contains behavior data. Length must equal to
#'   number of observations in `conmat`. Note `behav` could also be a row/column
#'   matrix, which will be converted to a vector using [drop()].
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Note if a vector is provided, it
#'   will be converted to a column matrix.
#'
#' @return A fitted `cpm` object.
#' @export
fit.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL
) {
  call <- match.call()
  call[[1]] <- quote(fit)
  fit_cpm_workflow(
    call = call,
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )
}

new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

validate_cpm_spec_params <- function(thresh_level, kfolds, bias_correct) {
  if (!is.numeric(thresh_level) || length(thresh_level) != 1L ||
    is.na(thresh_level) || !is.finite(thresh_level) ||
    thresh_level < 0 || thresh_level > 1) {
    stop("`thresh_level` must be a single number between 0 and 1.")
  }

  if (!is.null(kfolds) &&
    (!is.numeric(kfolds) || length(kfolds) != 1L || is.na(kfolds) ||
      !is.finite(kfolds) || kfolds < 2 || kfolds %% 1 != 0)) {
    stop("`kfolds` must be NULL or a single integer greater than or equal to 2.")
  }

  if (!is.logical(bias_correct) || length(bias_correct) != 1L ||
    is.na(bias_correct)) {
    stop("`bias_correct` must be either TRUE or FALSE.")
  }

  invisible()
}

fit_cpm_workflow <- function(
  call,
  object,
  conmat,
  behav,
  covariates
) {
  params <- object$params

  normalized <- normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    params$na_action
  )

  kfolds <- resolve_kfolds(params$kfolds, include_cases)
  folds <- crossv_kfold(include_cases, kfolds)
  edges <- init_edges(params$return_edges, conmat, kfolds)
  pred <- init_pred(behav)
  cv_result <- fit_predict_cv(
    conmat,
    behav,
    covariates,
    include_cases,
    folds,
    params$thresh_method,
    params$thresh_level,
    params$bias_correct,
    params$return_edges,
    pred,
    edges
  )

  new_cpm(
    call = call,
    folds = folds,
    behav = cv_result$real,
    pred = cv_result$pred,
    edges = cv_result$edges,
    spec = object,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      kfolds = kfolds,
      bias_correct = params$bias_correct
    )
  )
}
