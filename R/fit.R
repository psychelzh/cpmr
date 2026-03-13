#' @importFrom generics fit
#' @export
generics::fit

#' Fit a model specification on resamples
#'
#' Generic for fitting model specifications across resamples.
#'
#' @param object A model specification object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A resampling result object.
#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' Collect fold-level metrics from resampling results
#'
#' Generic for extracting fold-level metrics from resampling result objects.
#'
#' @param x A resampling result object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A data frame or tibble with one row per fold.
#' @export
collect_metrics <- function(x, ...) {
  UseMethod("collect_metrics")
}

#' Collect observation-level predictions from resampling results
#'
#' Generic for extracting observation-level predictions from resampling result
#' objects.
#'
#' @param x A resampling result object.
#' @param ... Additional arguments passed to method implementations.
#'
#' @return A data frame or tibble with one row per observation.
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
#' @param kfolds Number of folds used by `fit_resamples()` when `resamples` is
#'   `NULL`. If `NULL`, it will be set to be equal to the number of
#'   observations, i.e., leave-one-subject-out.
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
  cat(sprintf("  Resample folds:   %s\n", x$params$kfolds %||% "auto"))
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
#' @return A fitted `cpm` object from a single in-sample fit.
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
  fit_cpm_single(
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
  real <- behav

  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    fit_call <- fit(
      object,
      conmat = conmat[rows_train, , drop = FALSE],
      behav = behav[rows_train],
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

    pred[rows_test, ] <- predict_cpm_model(fit_call$model, conmat_test)
    real[rows_test] <- behav_test

    if (params$return_edges == "all") {
      edges[,, fold] <- fit_call$edges[,, 1]
    } else if (params$return_edges == "sum") {
      edges <- edges + fit_call$edges
    }
  }

  metrics <- compute_fold_metrics(real, pred, folds)
  predictions <- compute_fold_predictions(real, pred, folds)

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

fit_cpm_single <- function(
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

  pred <- init_pred(behav)

  if (is.null(covariates)) {
    conmat_train <- conmat[include_cases, , drop = FALSE]
    behav_train <- behav[include_cases]
  } else {
    covariates_train <- covariates[include_cases, , drop = FALSE]
    conmat_train <- regress_covariates(
      conmat[include_cases, , drop = FALSE],
      covariates_train
    )
    behav_train <- drop(regress_covariates(
      behav[include_cases],
      covariates_train
    ))
  }

  cur_edges <- select_edges(
    conmat_train,
    behav_train,
    params$thresh_method,
    params$thresh_level
  )
  model <- train_cpm_model(
    conmat_train,
    behav_train,
    cur_edges,
    params$bias_correct
  )
  pred[include_cases, ] <- predict_cpm_model(model, conmat_train)

  edges <- switch(
    params$return_edges,
    none = NULL,
    sum = cur_edges,
    all = {
      edge_array <- array(
        dim = c(dim(cur_edges), 1L),
        dimnames = list(NULL, corr_types, NULL)
      )
      edge_array[,, 1] <- cur_edges
      edge_array
    }
  )

  real <- behav
  real[include_cases] <- behav_train

  new_cpm(
    call = call,
    folds = list(include_cases),
    behav = real,
    pred = pred,
    edges = edges,
    model = model,
    spec = object,
    params = list(
      fit_mode = "single",
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      kfolds = 1L,
      bias_correct = params$bias_correct
    )
  )
}

train_cpm_model <- function(conmat, behav, edges, bias_correct) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  x <- matrix(
    1,
    nrow = dim(conmat)[1],
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x[, corr_type] <- Rfast::rowsums(
      conmat[, edges[, corr_type], drop = FALSE]
    )
  }

  models <- lapply(inc_edges, function(inc_edge) {
    cur_x <- if (inc_edge == "both") {
      x
    } else {
      x[, c("(Intercept)", inc_edge)]
    }
    stats::.lm.fit(cur_x, behav)$coefficients
  })
  names(models) <- inc_edges

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edges,
    models = models
  )
}

predict_cpm_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  x_new <- matrix(
    1,
    nrow = dim(conmat_new)[1],
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x_new[, corr_type] <- Rfast::rowsums(
      conmat_new[, model$edges[, corr_type], drop = FALSE]
    )
  }

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(inc_edges),
    dimnames = list(NULL, inc_edges)
  )
  for (inc_edge in inc_edges) {
    cur_x_new <- if (inc_edge == "both") {
      x_new
    } else {
      x_new[, c("(Intercept)", inc_edge)]
    }
    pred[, inc_edge] <- cur_x_new %*% model$models[[inc_edge]]
  }

  pred
}
