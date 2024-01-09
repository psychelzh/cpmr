critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

crossv_kfold <- function(n, k) {
  sample(cut(seq_len(n), breaks = k, labels = FALSE))
}

fscale <- function(x, center = TRUE, scale = TRUE, add_attr = TRUE) {
  if (isTRUE(center)) center <- colmeans(x)
  if (isTRUE(scale)) scale <- colVars(x, std = TRUE)
  scaled <- eachrow(eachrow(x, center, "-"), scale, "/")
  if (add_attr) {
    attr(scaled, "scaled:center") <- center
    attr(scaled, "scaled:scale") <- scale
  }
  scaled
}
