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
#' @return An S3 object of class `cpm`, which is a list containing the following
#'   components:
#'
#'   \item{real}{A numeric vector of observed behavior data.}
#'
#'   \item{pred}{A matrix of predicted behavior data. Rows are observations and
#'     columns are different models.}
#'
#'   \item{edges_prop}{A list of selection proportion of edges in each network,
#'     i.e., positive and negative networks.}
#'
#'   \item{cor}{A list of correlation test results between predicted behavior
#'     data and observed behavior data.}
#'
#'   The object also contains the following attributes:
#'
#'   \item{thresh_method}{The threshold method used in edge selection.}
#'
#'   \item{thresh_level}{The threshold level used in edge selection.}
#'
#'   \item{kfolds}{The folds number of cross-validation.}
#'
#'   \item{bias_correct}{Logical value indicating if the connectome data should
#'      be bias-corrected.}
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
  networks <- c("pos", "neg")
  includes <- c("both", "pos", "neg")
  behav_pred <- matrix(
    numeric(num_sub * length(includes)),
    nrow = num_sub,
    dimnames = list(NULL, includes)
  )
  masks <- array(
    logical(kfolds * ncol(conmat) * length(networks)),
    dim = c(kfolds, ncol(conmat), length(networks)),
    dimnames = list(NULL, NULL, networks)
  )
  for (fold in seq_len(kfolds)) {
    rows_train <- folds != fold
    # train models
    conmat_train <- conmat[rows_train, , drop = FALSE]
    conmat_test <- conmat[!rows_train, , drop = FALSE]
    behav_train <- behav[rows_train]
    cur_mask <- prepare_brain_mask(
      conmat_train, behav_train,
      thresh_method, thresh_level
    )
    for (include in includes) {
      cur_networks <- include
      if (cur_networks == "both") cur_networks <- networks
      model <- cpm_train(
        conmat_train, behav_train, cur_mask,
        bias_correct = bias_correct,
        networks = cur_networks
      )
      behav_pred[!rows_train, include] <- cpm_predict(
        model, conmat_test, cur_mask
      )
    }
    # save masks
    masks[fold, , ] <- cur_mask
  }
  structure(
    list(
      real = behav,
      pred = behav_pred,
      edges_prop = apply(masks, 2:3, mean),
      cor = apply(
        behav_pred, 2,
        function(pred) stats::cor.test(pred, behav)
      )
    ),
    thresh_method = thresh_method,
    thresh_level = thresh_level,
    kfolds = kfolds,
    bias_correct = bias_correct,
    class = "cpm"
  )
}

#' @export
print.cpm <- function(x, ...) {
  cat("CPM results with the following parameters:\n")
  cat("  Threshold method: ", attr(x, "thresh_method"), "\n", sep = "")
  cat("  Threshold level: ", attr(x, "thresh_level"), "\n", sep = "")
  cat("  Cross-validation folds: ", attr(x, "kfolds"), "\n", sep = "")
  cat("  Bias correction: ", attr(x, "bias_correct"), "\n", sep = "")
  cat("Correlation test results (N = ", length(x$real), "):\n", sep = "")
  for (include in names(x$cor)) {
    cat(
      "  Include ", include, " networks: ",
      round(x$cor[[include]]$estimate, 2),
      " (p = ", round(x$cor[[include]]$p.value, 3), ")\n",
      sep = ""
    )
  }
  invisible(x)
}

# helper functions
prepare_brain_mask <- function(conmat, behav, ...,
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
    dimnames = list(NULL, c("pos", "neg"))
  )
}

cpm_train <- function(conmat, behav, mask, ...,
                      bias_correct = TRUE,
                      networks = c("pos", "neg")) {
  networks <- match.arg(networks, several.ok = TRUE)
  if (bias_correct) conmat <- fscale(conmat)
  x <- matrix(1, nrow = nrow(conmat), ncol = length(networks) + 1)
  for (i in seq_along(networks)) {
    x[, i] <- rowsums(conmat[, mask[, networks[[i]]], drop = FALSE])
  }
  model <- stats::.lm.fit(x, behav)
  if (bias_correct) {
    model <- structure(
      model,
      train_center = attr(conmat, "scaled:center"),
      train_scale = attr(conmat, "scaled:scale")
    )
  }
  structure(
    model,
    networks = networks
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
