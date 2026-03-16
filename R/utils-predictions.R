compute_single_predictions <- function(real, pred) {
  new_predictions(real, pred)
}

compute_fold_predictions <- function(real, pred, folds) {
  fold_id <- rep(NA_integer_, length(real))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  new_predictions(real, pred, fold = fold_id)
}

new_predictions <- function(real, pred, fold = NULL) {
  predictions <- data.frame(
    row = seq_along(real),
    real = real,
    both = pred[, "both"],
    pos = pred[, "pos"],
    neg = pred[, "neg"],
    row.names = prediction_row_names(real)
  )

  if (is.null(fold)) {
    return(predictions)
  }

  predictions$fold <- fold
  predictions[, c("row", "fold", "real", prediction_types), drop = FALSE]
}

prediction_row_names <- function(real) {
  if (!is.null(names(real)) && !anyDuplicated(names(real))) {
    names(real)
  } else {
    NULL
  }
}
