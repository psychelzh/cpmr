#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' @export
collect_metrics <- function(x, ...) {
  UseMethod("collect_metrics")
}

#' @export
collect_predictions <- function(x, ...) {
  UseMethod("collect_predictions")
}

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

#' Fit a CPM model specification on resamples
#'
#' @rdname cpm_spec
#' @param resamples Optional list of assessment indices defining resamples.
#'   Each element must be an integer vector indexing rows in `conmat`. If
#'   `NULL`, folds are generated from `kfolds` in `object`.
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
  resamples = NULL
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

  if (is.null(resamples)) {
    kfolds <- resolve_kfolds(params$kfolds, include_cases)
    folds <- crossv_kfold(include_cases, kfolds)
  } else {
    folds <- validate_resamples(resamples, include_cases)
    kfolds <- length(folds)
  }

  pred <- init_pred(behav)
  edges <- init_edges(params$return_edges, conmat, kfolds)
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

  metrics <- compute_fold_metrics(cv_result$real, cv_result$pred, folds)
  predictions <- compute_fold_predictions(cv_result$real, cv_result$pred, folds)

  new_cpm_resamples(
    spec = object,
    folds = folds,
    metrics = metrics,
    predictions = predictions,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      kfolds = kfolds,
      bias_correct = params$bias_correct,
      return_edges = params$return_edges,
      na_action = params$na_action
    )
  )
}

#' @export
print.cpm_resamples <- function(x, ...) {
  cat("CPM resample results:\n")
  cat(sprintf("  Number of folds: %d\n", length(x$folds)))
  cat(sprintf("  Number of observations: %d\n", nrow(x$predictions)))
  cat("  Mean correlations:\n")
  cat(sprintf("    Both: %.3f\n", mean(x$metrics$both, na.rm = TRUE)))
  cat(sprintf("    Pos:  %.3f\n", mean(x$metrics$pos, na.rm = TRUE)))
  cat(sprintf("    Neg:  %.3f\n", mean(x$metrics$neg, na.rm = TRUE)))
  invisible(x)
}

#' Collect fold-level metrics from CPM resamples
#'
#' @param x A `cpm_resamples` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A [tibble][tibble::tibble-package] with one row per fold.
#' @export
collect_metrics.cpm_resamples <- function(x, ...) {
  tibble::as_tibble(x$metrics)
}

#' Collect observation-level predictions from CPM resamples
#'
#' @param x A `cpm_resamples` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A [tibble][tibble::tibble-package] with one row per observation.
#' @export
collect_predictions.cpm_resamples <- function(x, ...) {
  tibble::as_tibble(x$predictions)
}

new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

validate_cpm_spec_params <- function(thresh_level, kfolds, bias_correct) {
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
    !is.null(kfolds) &&
      (!is.numeric(kfolds) ||
        length(kfolds) != 1L ||
        is.na(kfolds) ||
        !is.finite(kfolds) ||
        kfolds < 2 ||
        kfolds %% 1 != 0)
  ) {
    stop(
      "`kfolds` must be NULL or a single integer greater than or equal to 2."
    )
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

validate_resamples <- function(resamples, include_cases) {
  if (!is.list(resamples) || length(resamples) == 0L) {
    stop("`resamples` must be a non-empty list of assessment indices.")
  }

  normalized <- lapply(resamples, function(idx) {
    if (!is.numeric(idx) || anyNA(idx) || any(!is.finite(idx))) {
      stop("Each element in `resamples` must contain finite numeric indices.")
    }
    unique(as.integer(idx))
  })

  all_assessment <- unlist(normalized, use.names = FALSE)
  include_cases <- sort(unique(include_cases))

  if (!all(all_assessment %in% include_cases)) {
    stop("All `resamples` indices must be contained in complete-case rows.")
  }
  if (length(all_assessment) != length(unique(all_assessment))) {
    stop("`resamples` indices must not overlap across folds.")
  }
  if (!identical(sort(all_assessment), include_cases)) {
    stop("`resamples` indices must cover all complete-case rows exactly once.")
  }

  normalized
}

compute_fold_metrics <- function(real, pred, folds) {
  fold_metrics <- lapply(seq_along(folds), function(i) {
    rows <- folds[[i]]
    data.frame(
      fold = i,
      n_assess = length(rows),
      both = safe_cor(real[rows], pred[rows, "both"]),
      pos = safe_cor(real[rows], pred[rows, "pos"]),
      neg = safe_cor(real[rows], pred[rows, "neg"])
    )
  })
  do.call(rbind, fold_metrics)
}

compute_fold_predictions <- function(real, pred, folds) {
  fold_id <- rep(NA_integer_, length(real))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  data.frame(
    row = seq_along(real),
    fold = fold_id,
    real = real,
    both = pred[, "both"],
    pos = pred[, "pos"],
    neg = pred[, "neg"]
  )
}

safe_cor <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }

  x <- x[valid]
  y <- y[valid]
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y)
}

new_cpm_resamples <- function(spec, folds, metrics, predictions, params) {
  structure(
    list(
      spec = spec,
      folds = folds,
      metrics = metrics,
      predictions = predictions,
      params = params
    ),
    class = "cpm_resamples"
  )
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
