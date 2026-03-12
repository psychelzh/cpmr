#' Perform Connectome-based Predictive Modeling (CPM)
#'
#' The connectome-based predictive modeling (CPM) is a data-driven approach to
#' predict individual behavior from brain connectivity data. Originally proposed
#' by Shen et al. (2017), the CPM has been widely used in various studies. This
#' function implements the CPM algorithm and provides a convenient interface to
#' use it.
#'
#' @examples
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' cpm(conmat, behav)
#' # use different threshold method and level
#' cpm(conmat, behav, thresh_method = "sparsity", thresh_level = 0.05)
#' # use a 10-fold cross-validation
#' cpm(conmat, behav, kfolds = 10)
#' @param conmat A matrix of connectome data. Observations in row, edges in
#'   column (assumed that duplicated edges are removed).
#' @param behav A numeric vector contains behavior data. Length must equal to
#'   number of observations in `conmat`. Note `behav` could also be a row/column
#'   matrix, which will be converted to a vector using [drop()].
#' @param ... For future extension. Currently ignored.
#' @param covariates A matrix of covariates. Observations in row, variables in
#'   column. If `NULL`, no covariates are used. Note if a vector is provided, it
#'   will be converted to a column matrix.
#' @param confounds Deprecated alias of `covariates`.
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
#'   Note complete cases are intersection of `conmat`, `behav` and `covariates`
#'   if provided.
#' @return A list with the following components:
#'
#'   \item{folds}{The corresponding fold for each observation when used as test
#'     group in cross-validation.}
#'
#'   \item{real}{The real behavior data. This is the same as the input `behav`
#'     if `covariates` is `NULL`, otherwise it is the fold-wise residual of
#'     `behav` after regressing out `covariates` in cross-validation.}
#'
#'   \item{pred}{The predicted behavior data, with each column corresponding to
#'     a model, i.e., both edges, positive edges, negative edges, and the row
#'     names corresponding to the observation names (the same as those of
#'     `behav`).}
#'
#'   \item{edges}{The selected edges, if `return_edges` is not `"none"`. If
#'     `return_edges` is `"sum"`, it is a matrix with rows corresponding to
#'     edges and columns corresponding to correlation types (i.e., `"pos"` and
#'     `"neg"`). If `return_edges` is `"all"`, it is a 3D array with dimensions
#'     corresponding to edges, correlation types and folds.}
#'
#'   \item{call}{The matched call.}
#'
#'   \item{params}{A list of parameters used in the function, including:
#'
#'   * `covariates` indicating if covariates are used
#'
#'   * `thresh_method` indicating the threshold method
#'
#'   * `thresh_level` indicating the threshold level
#'
#'   * `kfolds` indicating the number of folds in cross-validation
#'
#'   * `bias_correct` indicating if bias correction is used}
#' @references
#'
#' Shen, X., Finn, E. S., Scheinost, D., Rosenberg, M. D., Chun, M. M.,
#' Papademetris, X., & Constable, R. T. (2017). Using connectome-based
#' predictive modeling to predict individual behavior from brain connectivity.
#' Nature Protocols, 12(3), 506–518. https://doi.org/10.1038/nprot.2016.178
#'
#' Rapuano, K. M., Rosenberg, M. D., Maza, M. T., Dennis, N. J., Dorji, M.,
#' Greene, A. S., Horien, C., Scheinost, D., Todd Constable, R., & Casey, B. J.
#' (2020). Behavioral and brain signatures of substance use vulnerability in
#' childhood. Developmental Cognitive Neuroscience, 46, 100878.
#' https://doi.org/10.1016/j.dcn.2020.100878
#' @export
cpm <- function(
  conmat,
  behav,
  ...,
  covariates = NULL,
  confounds = NULL,
  thresh_method = c("alpha", "sparsity"),
  thresh_level = 0.01,
  kfolds = NULL,
  bias_correct = TRUE,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude")
) {
  call <- match.call()
  thresh_method <- match.arg(thresh_method)
  return_edges <- match.arg(return_edges)
  na_action <- match.arg(na_action)

  covariates <- resolve_covariates(covariates, confounds)

  normalized <- normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(conmat, behav, covariates, na_action)

  kfolds <- resolve_kfolds(kfolds, include_cases)
  folds <- crossv_kfold(include_cases, kfolds)
  edges <- init_edges(return_edges, conmat, kfolds)
  pred <- init_pred(behav)
  cv_result <- fit_predict_cv(
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
  )

  new_cpm(
    call = call,
    folds = folds,
    behav = cv_result$real,
    pred = cv_result$pred,
    edges = cv_result$edges,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      kfolds = kfolds,
      bias_correct = bias_correct
    )
  )
}

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
  } else if (!is.null(x$params$confounds)) {
    warning(
      "`x$params$confounds` is deprecated; using it as `covariates` in print.cpm().",
      call. = FALSE
    )
    x$params$confounds
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

resolve_covariates <- function(covariates, confounds) {
  if (!is.null(covariates) && !is.null(confounds)) {
    stop("Please provide only one of `covariates` or `confounds`.")
  }
  if (!is.null(confounds)) {
    warning(
      "`confounds` is deprecated; please use `covariates` instead.",
      call. = FALSE
    )
    return(confounds)
  }
  covariates
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
