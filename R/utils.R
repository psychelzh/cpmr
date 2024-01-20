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
