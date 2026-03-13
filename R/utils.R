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
        Rfast::nth(r_mat, k),
        Rfast::nth(r_mat, k, descending = TRUE)
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

regress_covariates <- function(resp, covariates) {
  stats::.lm.fit(cbind(1, covariates), resp)$residuals
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
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}
