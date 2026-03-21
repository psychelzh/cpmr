compute_single_predictions <- function(observed, pred) {
  new_predictions(observed, pred)
}

compute_fold_predictions <- function(observed, pred, folds) {
  fold_id <- rep(NA_integer_, length(observed))
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  new_predictions(observed, pred, fold = fold_id)
}

new_predictions <- function(observed, pred, fold = NULL) {
  prediction_streams <- colnames(pred)
  predictions <- data.frame(
    row = seq_along(observed),
    observed = observed,
    pred,
    row.names = prediction_row_names(observed)
  )

  if (is.null(fold)) {
    return(predictions)
  }

  predictions$fold <- fold
  predictions[, c("row", "fold", "observed", prediction_streams), drop = FALSE]
}

prediction_row_names <- function(observed) {
  if (!is.null(names(observed)) && !anyDuplicated(names(observed))) {
    names(observed)
  } else {
    NULL
  }
}

prediction_columns <- function(predictions) {
  setdiff(names(predictions), c("row", "fold", "observed"))
}
