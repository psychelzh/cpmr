simulate_cpm_problem <- function(n = 60, p = 30, seed = 1) {
  withr::local_seed(seed)

  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(x) <- paste0("edge_", seq_len(p))
  rownames(x) <- paste0("id_", seq_len(n))

  signal <- 1.2 * x[, 1] - 0.9 * x[, 2] + 0.6 * x[, 3]
  y <- signal + rnorm(n, sd = 0.4)
  names(y) <- rownames(x)

  list(
    x = x,
    y = y,
    data = data.frame(y = unname(y), x, check.names = FALSE)
  )
}
