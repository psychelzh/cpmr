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
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data before model fitting and
#'   prediction. See Rapuano et al. (2020) for more details.
#'
#' @return A `cpm_spec` object storing parameters for later fitting.
#'
#' @examples
#' spec <- cpm_spec(thresh_level = 0.01)
#' spec
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' fit(spec, conmat = conmat, behav = behav)
#' @export
cpm_spec <- function(
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  bias_correct = TRUE
) {
  validate_cpm_spec_params(
    thresh_level = thresh_level,
    bias_correct = bias_correct
  )

  thresh_method <- match.arg(thresh_method)

  new_cpm_spec(
    params = list(
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      bias_correct = bias_correct
    )
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM model specification:\n")
  cat(sprintf("  Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("  Threshold level:  %.2f\n", x$params$thresh_level))
  cat(sprintf("  Bias correction:  %s\n", x$params$bias_correct))
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
#' @param return_edges A character string indicating the return value of the
#'   selected edges. If `"none"`, no edges are returned/stored. If `"sum"`, edge
#'   masks are returned for single-fit and summed across folds for resampling.
#'   If `"all"`, single-fit stores a 3D array with a singleton third dimension
#'   while resampling stores fold-wise edge arrays.
#' @param na_action A character string indicating the action when missing values
#'   are found in `behav`. If `"fail"`, an error will be thrown. If `"exclude"`,
#'   missing values will be excluded from the analysis but kept in the output.
#'
#' @return A fitted `cpm` object from a single in-sample fit.
#' @export
fit.cpm_spec <- function(
  object,
  conmat,
  behav,
  ...,
  covariates = NULL,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  call[[1]] <- quote(fit)

  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  fit_cpm_single(
    call = call,
    object = object,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    return_edges = return_edges,
    na_action = na_action
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
#' @param na_action A character string indicating the action when missing values
#'   are found in `behav`. If `"fail"`, an error will be thrown. If `"exclude"`,
#'   missing values will be excluded from the analysis but kept in the output.
#'
#' @return A `cpm_resamples` object containing fold-level metrics and
#'   observation-level predictions.
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
  params <- object$params
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  normalized <- normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action
  )

  if (length(include_cases) == 0L) {
    stop("No complete-case observations available for resampling.")
  }
  if (length(include_cases) < 2L) {
    stop("At least 2 complete-case observations are required for resampling.")
  }

  if (is.null(resamples)) {
    kfolds <- resolve_kfolds(validate_kfolds(kfolds), include_cases)
    if (kfolds > length(include_cases)) {
      stop("`kfolds` must be less than or equal to complete-case observations.")
    }
    folds <- crossv_kfold(include_cases, kfolds)
  } else {
    if (!is.null(kfolds)) {
      stop("Specify either `resamples` or `kfolds`, not both.")
    }
    folds <- validate_resamples(resamples, include_cases)
    kfolds <- length(folds)
  }

  maybe_warn_large_edge_storage(ncol(conmat), kfolds, return_edges)

  pred <- init_pred(behav)
  edges <- init_edges(return_edges, conmat, kfolds)
  real <- behav

  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    fold_fit <- fit(
      object,
      conmat = conmat[rows_train, , drop = FALSE],
      behav = behav[rows_train],
      return_edges = if (return_edges == "none") "none" else "sum",
      na_action = "fail",
      covariates = if (is.null(covariates)) {
        NULL
      } else {
        covariates[rows_train, , drop = FALSE]
      }
    )

    if (is.null(covariates)) {
      conmat_test <- conmat[rows_test, , drop = FALSE]
      behav_test <- behav[rows_test]
    } else {
      covariates_train <- covariates[rows_train, , drop = FALSE]
      covariates_test <- covariates[rows_test, , drop = FALSE]

      conmat_regressed <- regress_covariates_by_train(
        conmat[rows_train, , drop = FALSE],
        conmat[rows_test, , drop = FALSE],
        covariates_train,
        covariates_test
      )
      behav_regressed <- regress_covariates_by_train(
        behav[rows_train],
        behav[rows_test],
        covariates_train,
        covariates_test
      )

      conmat_test <- conmat_regressed$test
      behav_test <- drop(behav_regressed$test)
    }

    pred[rows_test, ] <- predict_cpm_model(fold_fit$model, conmat_test)
    real[rows_test] <- behav_test

    if (return_edges == "all") {
      edges[,, fold] <- fold_fit$edges
    } else if (return_edges == "sum") {
      edges <- edges + fold_fit$edges
    }
  }

  metrics <- compute_fold_metrics(real, pred, folds)
  predictions <- compute_fold_predictions(real, pred, folds)

  new_cpm_resamples(
    spec = object,
    folds = folds,
    edges = edges,
    metrics = metrics,
    predictions = predictions,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      kfolds = kfolds,
      bias_correct = params$bias_correct,
      return_edges = return_edges,
      na_action = na_action
    )
  )
}

new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

validate_cpm_spec_params <- function(thresh_level, bias_correct) {
  if (
    !is.numeric(thresh_level) ||
      length(thresh_level) != 1L ||
      is.na(thresh_level) ||
      !is.finite(thresh_level) ||
      thresh_level < 0 ||
      thresh_level > 1
  ) {
    stop("`thresh_level` must be a single number between 0 and 1.")
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
