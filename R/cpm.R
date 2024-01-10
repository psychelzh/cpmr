#' Perform Connectome-based Predictive Modeling (CPM)
#'
#' The connectome-based predictive modeling (CPM) is a data-driven approach to
#' predict individual behavior from brain connectivity data. The CPM is based on
#' the linear regression model, where the brain connectivity data are used as
#' predictors and the behavior data are used as response.
#'
#' The CPM is implemented in the following steps:
#'
#' 1. Select edges based on the correlation between brain connectivity and
#'   behavior data.
#'
#' 2. Train a linear regression model using the selected edges.
#'
#' 3. Predict behavior data using the trained model.
#'
#' 4. Repeat steps 1-3 using different folds of cross-validation.
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
#'   number of observations in `conmat`.
#' @param ... For future extension. Currently ignored.
#' @param thresh_method,thresh_level The threshold method and level used in edge
#'   selection. If method is set to be `"alpha"`, the edge selection is based on
#'   the critical value of correlation coefficient. If method is set to be
#'   `"sparsity"`, the edge selection is based on the quantile of correlation
#'   coefficient, thus network sparsity is controlled.
#' @param kfolds Folds number of cross-validation. If `NULL`, it will be set to
#'   be equal to the number of observations, i.e., leave-one-subject-out.
#' @param bias_correct Logical value indicating if the connectome data should be
#'   bias-corrected. If `TRUE`, the connectome data will be centered and scaled
#'   to have unit variance based on the training data.
#' @return A list with the following components:
#'
#'   \item{real}{The real behavior data.}
#'
#'   \item{pred}{The predicted behavior data, with each column corresponding to
#'     a model, i.e., both edges, positive edges, negative edges.}
#'
#'   \item{edges}{The selected edges, a 3D array with dimensions of folds, edges
#'     and networks.}
#' @export
cpm <- function(conmat, behav, ...,
                thresh_method = c("alpha", "sparsity"),
                thresh_level = 0.01,
                kfolds = NULL,
                bias_correct = TRUE) {
  thresh_method <- match.arg(thresh_method)
  num_sub <- length(behav)
  # default to leave-one-subject-out
  if (is.null(kfolds)) kfolds <- num_sub
  folds <- crossv_kfold(num_sub, kfolds)
  # pre-allocation
  pred <- matrix(
    nrow = num_sub,
    ncol = length(includes),
    dimnames = list(NULL, includes)
  )
  edges <- array(
    dim = c(kfolds, dim(conmat)[2], length(networks)),
    dimnames = list(NULL, NULL, networks)
  )
  for (fold in seq_len(kfolds)) {
    rows_train <- folds != fold
    # train models
    conmat_train <- conmat[rows_train, , drop = FALSE]
    conmat_test <- conmat[!rows_train, , drop = FALSE]
    behav_train <- behav[rows_train]
    result <- .cpm(
      conmat_train, behav_train, conmat_test,
      thresh_method, thresh_level,
      bias_correct
    )
    pred[!rows_train, ] <- result$pred
    edges[fold, , ] <- result$edges
  }
  list(
    real = behav,
    pred = pred,
    edges = edges
  )
}

.cpm <- function(conmat_train, behav_train, conmat_test,
                 thresh_method, thresh_level,
                 bias_correct = TRUE) {
  if (bias_correct) {
    center <- colmeans(conmat_train)
    scale <- colVars(conmat_train, std = TRUE)
    conmat_train <- fscale(conmat_train, center, scale)
    conmat_test <- fscale(conmat_test, center, scale)
  }
  edges <- select_edges(
    conmat_train, behav_train,
    method = thresh_method,
    level = thresh_level
  )
  x_train <- allocate_design(dim(conmat_train)[1])
  x_test <- allocate_design(dim(conmat_test)[1])
  for (network in networks) {
    x_train[, network] <- rowsums(
      conmat_train[, edges[, network], drop = FALSE]
    )
    x_test[, network] <- rowsums(
      conmat_test[, edges[, network], drop = FALSE]
    )
  }
  pred <- matrix(
    nrow = dim(conmat_test)[1],
    ncol = length(includes),
    dimnames = list(NULL, includes)
  )
  for (include in includes) {
    if (include != "both") {
      cur_x_train <- x_train[, c("(Intercept)", include)]
      cur_x_test <- x_test[, c("(Intercept)", include)]
    } else {
      cur_x_train <- x_train
      cur_x_test <- x_test
    }
    cur_model <- stats::.lm.fit(cur_x_train, behav_train)
    pred[, include] <- cur_x_test %*% cur_model$coefficients
  }
  list(
    edges = edges,
    pred = pred
  )
}

# helper functions
select_edges <- function(conmat, behav, ...,
                               method = c("alpha", "sparsity"),
                               level = 0.01) {
  method <- match.arg(method)
  r_mat <- stats::cor(conmat, behav)
  r_crit <- switch(method,
    alpha = {
      thresh <- critical_r(nrow(conmat), level)
      c(-thresh, thresh)
    },
    sparsity = {
      thresh <- stats::quantile(r_mat, c(level, 1 - level))
      if (thresh[[1]] > 0 || thresh[[2]] < 0) {
        warning("Not enough positive or negative correlation values.")
      }
      thresh
    },
    stop("Invalid threshold method.")
  )
  matrix(
    c(r_mat > r_crit[2], r_mat < r_crit[1]),
    ncol = 2,
    dimnames = list(NULL, networks)
  )
}

cpm_predict <- function(model, conmat, mask, ...,
                        bias_correct = TRUE) {
  if (bias_correct) {
    conmat <- fscale(
      conmat,
      center = attr(model, "train_center"),
      scale = attr(model, "train_scale")
    )
  }
  networks <- attr(model, "networks")
  x <- matrix(1, nrow = nrow(conmat), ncol = length(networks) + 1)
  for (i in seq_along(networks)) {
    x[, i] <- rowsums(conmat[, mask[, networks[[i]]], drop = FALSE])
  }
  x %*% model$coefficients
}

allocate_design <- function(nrow) {
  matrix(
    1,
    nrow = nrow, ncol = length(networks) + 1,
    dimnames = list(NULL, c("(Intercept)", networks))
  )
}
