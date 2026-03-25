assemble_single_predictions <- function(
  observed,
  include_cases,
  split_fit
) {
  pred <- matrix(
    NA_real_,
    nrow = length(observed),
    ncol = ncol(split_fit$predictions),
    dimnames = list(
      prediction_row_names(observed),
      colnames(split_fit$predictions)
    )
  )
  pred[include_cases, ] <- split_fit$predictions

  observed[include_cases] <- split_fit$observed
  prediction_frame(observed, pred)
}

assemble_fold_predictions <- function(observed, folds, split_results) {
  pred <- matrix(
    NA_real_,
    nrow = length(observed),
    ncol = ncol(split_results[[1]]$predictions),
    dimnames = list(
      prediction_row_names(observed),
      colnames(split_results[[1]]$predictions)
    )
  )
  fold_id <- rep(NA_integer_, length(observed))
  for (fold in seq_along(folds)) {
    rows_test <- folds[[fold]]
    pred[rows_test, ] <- split_results[[fold]]$predictions
    observed[rows_test] <- split_results[[fold]]$observed
    fold_id[rows_test] <- fold
  }

  prediction_frame(observed, pred, fold = fold_id)
}

prediction_frame <- function(observed, pred, fold = NULL) {
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
