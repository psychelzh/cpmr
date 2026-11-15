run_cpm_workflow <- function(
  object,
  conmat,
  behav,
  covariates = NULL,
  resamples = 10L,
  return_edges = c("sum", "none", "all"),
  na_action = c("fail", "exclude"),
  call = NULL
) {
  return_edges <- match.arg(return_edges)
  inputs <- prepare_cpm_inputs(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action
  )

  folds <- resolve_assessment_folds(
    resamples = resamples,
    include_cases = inputs$include_cases
  )
  n_folds <- length(folds)
  warn_small_assessment_folds(folds)

  warn_large_edge_storage(ncol(conmat), n_folds, return_edges)

  edges <- init_edge_storage(return_edges, conmat, n_folds)
  split_results <- vector("list", n_folds)

  for (fold in seq_len(n_folds)) {
    rows_test <- folds[[fold]]
    rows_train <- setdiff(inputs$include_cases, rows_test)

    split_result <- fit_split(
      conmat = conmat,
      behav = inputs$behav,
      covariates = inputs$covariates,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = object$selection,
      construction_spec = object$construction,
      model_spec = object$model
    )

    if (return_edges == "all") {
      edges[,, fold] <- split_result$edge_mask
    } else if (return_edges == "sum") {
      edges <- edges + split_result$edge_mask
    }
    split_results[[fold]] <- split_result[c("observed", "predictions")]
    rm(split_result)
  }

  predictions <- assemble_fold_predictions(
    observed = inputs$behav,
    folds = folds,
    split_results = split_results
  )

  structure(
    list(
      call = call,
      spec = object,
      settings = list(
        covariates = !is.null(inputs$covariates),
        na_action = inputs$na_action,
        return_edges = return_edges
      ),
      predictions = predictions,
      edges = edges,
      folds = folds
    ),
    class = "cpm"
  )
}

prepare_cpm_inputs <- function(
  conmat,
  behav,
  covariates,
  na_action
) {
  na_action <- match.arg(na_action, c("fail", "exclude"))
  inputs <- normalize_inputs(conmat, behav, covariates)
  include_cases <- complete_case_rows(
    conmat = conmat,
    behav = inputs$behav,
    covariates = inputs$covariates,
    na_action = na_action
  )

  if (length(include_cases) == 0L) {
    stop(
      "No complete-case observations available for CPM.",
      call. = FALSE
    )
  }

  list(
    behav = inputs$behav,
    covariates = inputs$covariates,
    include_cases = include_cases,
    na_action = na_action
  )
}

normalize_inputs <- function(conmat, behav, covariates = NULL) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.", call. = FALSE)
  }
  if (nrow(conmat) != length(behav)) {
    stop(
      "The number of observations in `conmat` and `behav` must match.",
      call. = FALSE
    )
  }
  if (!is.null(rownames(conmat)) && !is.null(names(behav))) {
    if (!identical(rownames(conmat), names(behav))) {
      stop(
        "Case names of `conmat` must match those of behavior data.",
        call. = FALSE
      )
    }
  }

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop(
        "The number of observations in `covariates` and `behav` must match.",
        call. = FALSE
      )
    }
    if (!is.null(rownames(covariates)) && !is.null(names(behav))) {
      if (!identical(rownames(covariates), names(behav))) {
        stop(
          "Case names of `covariates` must match those of behavior data.",
          call. = FALSE
        )
      }
    }
  }

  list(
    behav = behav,
    covariates = covariates
  )
}

complete_case_rows <- function(conmat, behav, covariates, na_action) {
  switch(
    na_action,
    fail = {
      if (anyNA(conmat)) {
        stop("Missing values found in `conmat`", call. = FALSE)
      }
      if (anyNA(behav)) {
        stop("Missing values found in `behav`", call. = FALSE)
      }
      if (!is.null(covariates) && anyNA(covariates)) {
        stop("Missing values found in `covariates`", call. = FALSE)
      }
      seq_along(behav)
    },
    exclude = {
      include_mask <- stats::complete.cases(conmat) & !is.na(behav)
      if (!is.null(covariates)) {
        include_mask <- include_mask & stats::complete.cases(covariates)
      }
      which(include_mask)
    }
  )
}

warn_large_edge_storage <- function(n_edges, n_folds, return_edges) {
  if (return_edges != "all") {
    return(invisible())
  }

  estimated_bytes <- as.double(n_edges) * length(edge_signs) * n_folds * 4
  threshold_bytes <- 10 * 1024^2
  if (estimated_bytes > threshold_bytes) {
    warning(
      sprintf(
        paste0(
          "Storing fold-wise edges (`return_edges = \"all\"`) may consume ",
          "large memory (~%.1f MB). Consider `return_edges = \"sum\"` or ",
          "`return_edges = \"none\"` when edge storage is optional."
        ),
        estimated_bytes / 1024^2
      )
    )
  }

  invisible()
}

warn_small_assessment_folds <- function(folds) {
  small_folds <- sum(lengths(folds) < 3L)
  if (small_folds == 0L) {
    return(invisible(FALSE))
  }

  warning(
    sprintf(
      paste0(
        "%d of %d assessment folds contain fewer than 3 observations. ",
        "Fold-wise correlations for these folds are unavailable; RMSE and ",
        "MAE should be prioritized, and pooled out-of-fold correlations ",
        "should be treated as descriptive."
      ),
      small_folds,
      length(folds)
    ),
    call. = FALSE
  )
  invisible(TRUE)
}

init_edge_storage <- function(return_edges, conmat, n_folds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), n_folds),
      dimnames = list(NULL, edge_signs, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_signs)),
      dimnames = list(NULL, edge_signs)
    ),
    none = NULL
  )
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

prediction_frame <- function(observed, pred, fold) {
  prediction_streams <- colnames(pred)
  predictions <- data.frame(
    row = seq_along(observed),
    observed = observed,
    fold = fold,
    pred,
    row.names = prediction_row_names(observed)
  )

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
