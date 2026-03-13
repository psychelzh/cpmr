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
  cat(sprintf("    Stored splits:    %d\n", length(x$folds)))
  cat(sprintf("    Bias correction:  %s\n", x$params$bias_correct))
  invisible(x)
}

#' Collect selected edges from a CPM fit
#'
#' @param x A `cpm` object.
#' @param ... For future extension. Currently ignored.
#'
#' @return A matrix for `return_edges = "sum"`, a 3D array for
#'   `return_edges = "all"`, or `NULL` for `return_edges = "none"`.
#' @export
collect_edges.cpm <- function(x, ...) {
  x$edges
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

new_cpm <- function(call, folds, behav, pred, edges, model, spec, params) {
  structure(
    list(
      folds = folds,
      real = behav,
      pred = pred,
      edges = edges,
      model = model,
      spec = spec,
      call = call,
      params = params
    ),
    class = "cpm"
  )
}
