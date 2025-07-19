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
  r_crit <- switch(
    method,
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
    dimnames = list(NULL, corr_types)
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
      nrow = nrow,
      ncol = length(corr_types) + 1,
      dimnames = list(NULL, c("(Intercept)", corr_types))
    )
  }
  x <- allocate_predictors(dim(conmat)[1])
  x_new <- allocate_predictors(dim(conmat_new)[1])
  for (corr_type in corr_types) {
    x[, corr_type] <- rowsums(
      conmat[, edges[, corr_type], drop = FALSE]
    )
    x_new[, corr_type] <- rowsums(
      conmat_new[, edges[, corr_type], drop = FALSE]
    )
  }
  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(inc_edges),
    dimnames = list(NULL, inc_edges)
  )
  for (inc_edge in inc_edges) {
    if (inc_edge == "both") {
      cur_x <- x
      cur_x_new <- x_new
    } else {
      cur_x <- x[, c("(Intercept)", inc_edge)]
      cur_x_new <- x_new[, c("(Intercept)", inc_edge)]
    }
    model <- stats::.lm.fit(cur_x, behav)
    pred[, inc_edge] <- cur_x_new %*% model$coefficients
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

crossv_kfold <- function(x, k) {
  split(sample(x), cut(seq_along(x), breaks = k, labels = FALSE))
}

fscale <- function(x, center, scale) {
  eachrow(eachrow(x, center, "-"), scale, "/")
}
