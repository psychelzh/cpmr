critical_r <- function(n, alpha) {
  if (alpha <= 0) {
    return(Inf)
  }
  if (alpha >= 1) {
    return(0)
  }

  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

core_select_edges <- function(conmat, behav, method, level) {
  r_mat <- stats::cor(conmat, behav)
  r_crit <- switch(
    method,
    alpha = {
      thresh <- critical_r(nrow(conmat), level)
      c(-thresh, thresh)
    },
    sparsity = {
      k <- max(1L, min(length(r_mat), as.integer(round(level * length(r_mat)))))
      thresh <- c(
        Rfast::nth(r_mat, k),
        Rfast::nth(r_mat, k, descending = TRUE)
      )
      if (thresh[[1]] > 0 || thresh[[2]] < 0) {
        warning("Not enough positive or negative correlation values.")
      }
      thresh
    },
    stop("Invalid threshold method.")
  )

  matrix(
    c(r_mat >= r_crit[2], r_mat <= r_crit[1]),
    ncol = 2,
    dimnames = list(colnames(conmat), edge_signs)
  )
}
