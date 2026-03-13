#' @export
print.cpm <- function(x, ...) {
  cat("CPM results:\n")
  cat("  Call: ")
  print(x$call)
  cat(sprintf("  Number of observations: %d\n", length(x$real)))
  cat(sprintf("    Complete cases: %d\n", sum(stats::complete.cases(x$pred))))
  if (!is.null(x$edges)) {
    cat(sprintf("  Number of edges: %d\n", dim(x$edges)[1]))
  } else {
    cat("  Number of edges: not stored\n")
  }
  covariates_param <- if (!is.null(x$params$covariates)) {
    x$params$covariates
  } else {
    NA
  }
  cat("  Parameters:\n")
  cat(sprintf("    Covariates:       %s\n", covariates_param))
  cat(sprintf("    Threshold method: %s\n", x$params$thresh_method))
  cat(sprintf("    Threshold level:  %.2f\n", x$params$thresh_level))
  cat(sprintf("    CV folds:         %d\n", x$params$kfolds))
  cat(sprintf("    Bias correction:  %s\n", x$params$bias_correct))
  invisible(x)
}

normalize_inputs <- function(conmat, behav, covariates) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.")
  }
  if (nrow(conmat) != length(behav)) {
    stop("Case numbers of `conmat` and `behav` must match.")
  }
  check_names(conmat, behav)

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop("Case numbers of `covariates` and `behav` must match.")
    }
    check_names(covariates, behav)
  }

  list(
    behav = behav,
    covariates = covariates
  )
}

resolve_include_cases <- function(conmat, behav, covariates, na_action) {
  switch(
    na_action,
    fail = {
      stopifnot(
        "Missing values found in `conmat`" = !anyNA(conmat),
        "Missing values found in `behav`" = !anyNA(behav),
        "Missing values found in `covariates`" = is.null(covariates) ||
          !anyNA(covariates)
      )
      seq_along(behav)
    },
    exclude = Reduce(
      intersect,
      list(
        which(stats::complete.cases(conmat)),
        which(stats::complete.cases(behav)),
        if (!is.null(covariates)) {
          which(stats::complete.cases(covariates))
        } else {
          seq_along(behav)
        }
      )
    )
  )
}

resolve_kfolds <- function(kfolds, include_cases) {
  if (is.null(kfolds)) {
    return(length(include_cases))
  }
  kfolds
}

init_edges <- function(return_edges, conmat, kfolds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(corr_types), kfolds),
      dimnames = list(NULL, corr_types, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(corr_types)),
      dimnames = list(NULL, corr_types)
    ),
    none = NULL
  )
}

init_pred <- function(behav) {
  matrix(
    nrow = length(behav),
    ncol = length(inc_edges),
    dimnames = list(names(behav), inc_edges)
  )
}

fit_predict_cv <- function(
  conmat,
  behav,
  covariates,
  include_cases,
  folds,
  thresh_method,
  thresh_level,
  bias_correct,
  return_edges,
  pred,
  edges
) {
  real <- behav
  kfolds <- length(folds)
  for (fold in seq_len(kfolds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(include_cases, rows_test)

    if (is.null(covariates)) {
      conmat_train <- conmat[rows_train, , drop = FALSE]
      conmat_test <- conmat[rows_test, , drop = FALSE]
      behav_train <- behav[rows_train]
      behav_test <- behav[rows_test]
    } else {
      regressed <- regress_covariates_foldwise(
        conmat,
        behav,
        covariates,
        rows_train,
        rows_test
      )
      conmat_train <- regressed$conmat_train
      conmat_test <- regressed$conmat_test
      behav_train <- regressed$behav_train
      behav_test <- regressed$behav_test
    }

    cur_edges <- select_edges(
      conmat_train,
      behav_train,
      thresh_method,
      thresh_level
    )
    cur_pred <- predict_cpm(
      conmat_train,
      behav_train,
      conmat_test,
      cur_edges,
      bias_correct
    )
    pred[rows_test, ] <- cur_pred
    real[rows_test] <- behav_test
    if (return_edges == "all") {
      edges[,, fold] <- cur_edges
    } else if (return_edges == "sum") {
      edges <- edges + cur_edges
    }
  }

  list(pred = pred, edges = edges, real = real)
}

regress_covariates_foldwise <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test
) {
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

  list(
    conmat_train = conmat_regressed$train,
    conmat_test = conmat_regressed$test,
    behav_train = behav_regressed$train,
    behav_test = drop(behav_regressed$test)
  )
}

regress_covariates_by_train <- function(
  resp_train,
  resp_test,
  cov_train,
  cov_test
) {
  x_train <- cbind(1, cov_train)
  model <- stats::.lm.fit(x_train, resp_train)
  x_test <- cbind(1, cov_test)
  list(
    train = model$residuals,
    test = resp_test - x_test %*% model$coefficients
  )
}

new_cpm <- function(call, folds, behav, pred, edges, params) {
  structure(
    list(
      folds = folds,
      real = behav,
      pred = pred,
      edges = edges,
      call = call,
      params = params
    ),
    class = "cpm"
  )
}
