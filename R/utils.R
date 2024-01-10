critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

crossv_kfold <- function(n, k) {
  sample(cut(seq_len(n), breaks = k, labels = FALSE))
}

fscale <- function(x, center = TRUE, scale = TRUE) {
  if (isTRUE(center)) center <- colmeans(x)
  if (isTRUE(scale)) scale <- colVars(x, std = TRUE)
  eachrow(eachrow(x, center, "-"), scale, "/")
}
