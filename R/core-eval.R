core_compute_fold_metrics <- function(real, pred, folds, network) {
  fold_metrics <- lapply(seq_along(folds), function(i) {
    rows <- folds[[i]]
    data.frame(
      fold = i,
      n_assess = length(rows),
      both = core_safe_cor(real[rows], pred[rows, "both"]),
      pos = core_safe_cor(real[rows], pred[rows, "pos"]),
      neg = core_safe_cor(real[rows], pred[rows, "neg"]),
      estimate = core_safe_cor(real[rows], pred[rows, network])
    )
  })
  do.call(rbind, fold_metrics)
}

core_compute_fold_predictions <- function(real, pred, folds, network) {
  fold_id <- rep(NA_integer_, length(real))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  data.frame(
    row = seq_along(real),
    fold = fold_id,
    truth = real,
    .pred = pred[, network],
    both = pred[, "both"],
    pos = pred[, "pos"],
    neg = pred[, "neg"]
  )
}

core_safe_cor <- function(x, y, method = "pearson") {
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
