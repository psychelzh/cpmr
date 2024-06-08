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
#' @param confounds A matrix of confounding variables. Observations in row,
#'   variables in column. If `NULL`, no confounding variables are used.
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
#' @return A list with the following components:
#'
#'   \item{folds}{The corresponding fold for each observation when used as test
#'     group in cross-validation.}
#'
#'   \item{real}{The real behavior data. This is the same as the input `behav`
#'     if `confounds` is `NULL`, otherwise it is the residual of `behav` after
#'     regressing out `confounds`.}
#'
#'   \item{pred}{The predicted behavior data, with each column corresponding to
#'     a model, i.e., both edges, positive edges, negative edges, and the row
#'     names corresponding to the observation names (the same as those of
#'     `behav`).}
#'
#'   \item{edges}{The selected edges, if `return_edges` is not `"none"`. If
#'     `return_edges` is `"sum"`, it is a matrix with rows corresponding to
#'     edges and columns corresponding to networks. If `return_edges` is
#'     `"all"`, it is a 3D array with dimensions corresponding to folds, edges,
#'     and networks.}
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
cpm <- function(conmat, behav, ...,
                confounds = NULL,
                thresh_method = c("alpha", "sparsity"),
                thresh_level = 0.01,
                kfolds = NULL,
                bias_correct = TRUE,
                return_edges = c("none", "sum", "all")) {
  call <- match.call()
  thresh_method <- match.arg(thresh_method)
  return_edges <- match.arg(return_edges)
  # ensure `behav` is a vector
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.")
  }
  check_names(conmat, behav)
  if (!is.null(confounds)) {
    check_names(confounds, behav)
    conmat <- regress_counfounds(conmat, confounds)
    behav <- regress_counfounds(behav, confounds)
  }
  # default to leave-one-subject-out
  if (is.null(kfolds)) kfolds <- length(behav)
  folds <- crossv_kfold(length(behav), kfolds)
  # pre-allocation
  edges <- switch(return_edges,
    all = array(
      dim = c(kfolds, dim(conmat)[2], length(networks)),
      dimnames = list(NULL, NULL, networks)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(networks)),
      dimnames = list(NULL, networks)
    )
  )
  pred <- matrix(
    nrow = length(behav),
    ncol = length(includes),
    dimnames = list(names(behav), includes)
  )
  for (fold in seq_len(kfolds)) {
    rows_train <- folds != fold
    conmat_train <- conmat[rows_train, , drop = FALSE]
    behav_train <- behav[rows_train]
    cur_edges <- select_edges(
      conmat_train, behav_train,
      thresh_method, thresh_level
    )
    conmat_test <- conmat[!rows_train, , drop = FALSE]
    cur_pred <- predict_cpm(
      conmat_train, behav_train, conmat_test,
      cur_edges, bias_correct
    )
    pred[!rows_train, ] <- cur_pred
    if (return_edges == "all") {
      edges[fold, , ] <- cur_edges
    } else if (return_edges == "sum") {
      edges <- edges + cur_edges
    }
  }
  structure(
    list(
      folds = folds,
      real = behav,
      pred = pred,
      edges = edges,
      call = call
    ),
    class = "cpm"
  )
}

#' @export
print.cpm <- function(x, ...) {
  cv <- if (length(unique(x$folds)) == length(x$real)) {
    "leave-one-out"
  } else {
    sprintf("%d-fold", length(unique(x$folds)))
  }
  cat(sprintf("CPM results based on %s cross validation.\n", cv))
  invisible(x)
}

# helper functions
check_names <- function(data, behav) {
  if (!is.null(rownames(data)) && !is.null(names(behav))) {
    if (!identical(rownames(data), names(behav))) {
      stop(
        sprintf(
          "Case names of `%s` must match those of behavior data.",
          deparse1(substitute(data))
        )
      )
    }
  }
  invisible()
}

select_edges <- function(conmat, behav, method, level) {
  r_mat <- stats::cor(conmat, behav)
  r_crit <- switch(method,
    alpha = {
      thresh <- critical_r(nrow(conmat), level)
      c(-thresh, thresh)
    },
    sparsity = {
      k <- round(level * length(r_mat))
      thresh <- c(
        nth(r_mat, k),
        nth(r_mat, k, descending = TRUE)
      )
      if (thresh[[1]] > 0 || thresh[[2]] < 0) {
        warning("Not enough positive or negative correlation values.") # nocov
      }
      thresh
    },
    stop("Invalid threshold method.")
  )
  matrix(
    c(r_mat >= r_crit[2], r_mat <= r_crit[1]),
    ncol = 2,
    dimnames = list(NULL, networks)
  )
}

predict_cpm <- function(conmat, behav, conmat_new, edges, bias_correct) {
  if (bias_correct) {
    center <- colmeans(conmat)
    scale <- colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
    conmat_new <- fscale(conmat_new, center, scale)
  }
  allocate_predictors <- function(nrow) {
    matrix(
      1,
      nrow = nrow, ncol = length(networks) + 1,
      dimnames = list(NULL, c("(Intercept)", networks))
    )
  }
  x <- allocate_predictors(dim(conmat)[1])
  x_new <- allocate_predictors(dim(conmat_new)[1])
  for (network in networks) {
    x[, network] <- rowsums(
      conmat[, edges[, network], drop = FALSE]
    )
    x_new[, network] <- rowsums(
      conmat_new[, edges[, network], drop = FALSE]
    )
  }
  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(includes),
    dimnames = list(NULL, includes)
  )
  for (include in includes) {
    if (include == "both") {
      cur_x <- x
      cur_x_new <- x_new
    } else {
      cur_x <- x[, c("(Intercept)", include)]
      cur_x_new <- x_new[, c("(Intercept)", include)]
    }
    model <- stats::.lm.fit(cur_x, behav)
    pred[, include] <- cur_x_new %*% model$coefficients
  }
  pred
}

regress_counfounds <- function(resp, confounds) {
  stats::.lm.fit(cbind(1, confounds), resp)$residuals
}

critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

crossv_kfold <- function(n, k) {
  sample(cut(seq_len(n), breaks = k, labels = FALSE))
}

fscale <- function(x, center, scale) {
  eachrow(eachrow(x, center, "-"), scale, "/")
}
