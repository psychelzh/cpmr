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
  prediction_types <- colnames(pred)
  predictions <- data.frame(
    row = seq_along(real),
    real = real,
    pred,
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

prediction_columns <- function(predictions) {
  setdiff(names(predictions), c("row", "fold", "real"))
}
