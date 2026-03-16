compute_fold_metrics <- function(predictions, folds) {
  fold_metrics <- lapply(seq_along(folds), function(i) {
    rows <- folds[[i]]
    data.frame(
      fold = i,
      n_assess = length(rows),
      both = safe_cor(predictions$real[rows], predictions$both[rows]),
      pos = safe_cor(predictions$real[rows], predictions$pos[rows]),
      neg = safe_cor(predictions$real[rows], predictions$neg[rows])
    )
  })
  do.call(rbind, fold_metrics)
}

summarize_resample_edges <- function(edges, return_edges, kfolds) {
  if (is.null(edges) || return_edges == "none") {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(edges / kfolds)
  }

  apply(edges, c(1, 2), mean)
}
