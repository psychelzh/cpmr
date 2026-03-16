safe_cor <- function(x, y, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  valid <- stats::complete.cases(x, y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }

  x <- x[valid]
  y <- y[valid]
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y, method = method)
}

safe_rmse <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (!any(valid)) {
    return(NA_real_)
  }

  sqrt(mean((x[valid] - y[valid])^2))
}

safe_mae <- function(x, y) {
  valid <- stats::complete.cases(x, y)
  if (!any(valid)) {
    return(NA_real_)
  }

  mean(abs(x[valid] - y[valid]))
}

safe_mean <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

safe_std_error <- function(x) {
  valid <- x[stats::complete.cases(x)]
  if (length(valid) < 2L) {
    return(NA_real_)
  }

  stats::sd(valid) / sqrt(length(valid))
}
