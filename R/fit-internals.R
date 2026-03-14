validate_kfolds <- function(kfolds) {
  if (
    !is.null(kfolds) &&
      (!is.numeric(kfolds) ||
        length(kfolds) != 1L ||
        is.na(kfolds) ||
        !is.finite(kfolds) ||
        kfolds < 2 ||
        kfolds %% 1 != 0)
  ) {
    stop(
      "`kfolds` must be NULL or a single integer greater than or equal to 2."
    )
  }

  if (is.null(kfolds)) {
    return(NULL)
  }

  as.integer(kfolds)
}

validate_resamples <- function(resamples, include_cases) {
  if (!is.list(resamples) || length(resamples) == 0L) {
    stop("`resamples` must be a non-empty list of assessment indices.")
  }
  if (length(resamples) < 2L) {
    stop("`resamples` must contain at least 2 assessment sets.")
  }

  normalized <- lapply(resamples, function(idx) {
    if (!is.numeric(idx) || anyNA(idx) || any(!is.finite(idx))) {
      stop("Each element in `resamples` must contain finite numeric indices.")
    }
    if (any(idx %% 1 != 0)) {
      stop("Each element in `resamples` must contain integer-valued indices.")
    }

    idx <- as.integer(idx)
    if (any(idx <= 0L)) {
      stop("Each element in `resamples` must contain positive indices.")
    }
    if (anyDuplicated(idx)) {
      stop("Each element in `resamples` must not contain duplicates.")
    }

    idx
  })

  all_assessment <- unlist(normalized, use.names = FALSE)
  include_cases <- sort(unique(include_cases))

  if (!all(all_assessment %in% include_cases)) {
    stop("All `resamples` indices must be contained in complete-case rows.")
  }
  if (length(all_assessment) != length(unique(all_assessment))) {
    stop("`resamples` indices must not overlap across folds.")
  }
  if (!identical(sort(all_assessment), include_cases)) {
    stop("`resamples` indices must cover all complete-case rows exactly once.")
  }

  normalized
}

maybe_warn_large_edge_storage <- function(n_edges, kfolds, return_edges) {
  if (return_edges != "all") {
    return(invisible())
  }

  estimated_bytes <- as.double(n_edges) * length(corr_types) * kfolds * 4
  threshold_bytes <- 10 * 1024^2
  if (estimated_bytes > threshold_bytes) {
    warning(
      sprintf(
        paste0(
          "Storing fold-wise edges (`return_edges = \"all\"`) may consume ",
          "large memory (~%.1f MB). Consider `return_edges = \"sum\"` or ",
          "`collect_edges(format = \"index\")` for sparse export."
        ),
        estimated_bytes / 1024^2
      )
    )
  }

  invisible()
}

edges_to_index <- function(edges, return_edges) {
  if (return_edges == "none" || is.null(edges)) {
    return(NULL)
  }

  if (return_edges == "sum") {
    return(list(
      pos = which(edges[, "pos"] > 0),
      neg = which(edges[, "neg"] > 0)
    ))
  }

  lapply(seq_len(dim(edges)[3]), function(fold) {
    list(
      fold = fold,
      pos = which(edges[, "pos", fold]),
      neg = which(edges[, "neg", fold])
    )
  })
}

# `fit()` and `fit_resamples()` share the same training-data contract:
# residualize covariates on training rows only, then pass the transformed
# training data into the common edge-selection/model-training primitives.
prepare_training_data <- function(conmat, behav, covariates, rows) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows, , drop = FALSE],
      behav = behav[rows],
      covariates = NULL
    ))
  }

  covariates_train <- covariates[rows, , drop = FALSE]
  list(
    conmat = regress_covariates(
      conmat[rows, , drop = FALSE],
      covariates_train
    ),
    behav = drop(regress_covariates(
      behav[rows],
      covariates_train
    )),
    covariates = covariates_train
  )
}

# Assessment rows must reuse nuisance-regression coefficients learned from the
# paired training rows only so resampling never leaks test information back.
prepare_assessment_data <- function(
  conmat,
  behav,
  covariates,
  rows_train,
  rows_test,
  covariates_train = NULL
) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows_test, , drop = FALSE],
      behav = behav[rows_test]
    ))
  }

  if (is.null(covariates_train)) {
    covariates_train <- covariates[rows_train, , drop = FALSE]
  }
  covariates_test <- covariates[rows_test, , drop = FALSE]

  conmat_regressed <- regress_covariates_by_train(
    conmat[rows_train, , drop = FALSE],
    conmat[rows_test, , drop = FALSE],
    covariates_train,
    covariates_test
  )
  behav_regressed <- regress_covariates_by_train(
    behav[rows_train],
    behav[rows_test],
    covariates_train,
    covariates_test
  )

  list(
    conmat = conmat_regressed$test,
    behav = drop(behav_regressed$test)
  )
}

fit_cpm_single <- function(
  call,
  object,
  conmat,
  behav,
  covariates,
  return_edges,
  na_action
) {
  if (
    !is.logical(return_edges) ||
      length(return_edges) != 1L ||
      is.na(return_edges)
  ) {
    stop("`return_edges` must be either TRUE or FALSE in `fit_cpm_single()`.")
  }

  params <- object$params

  normalized <- normalize_inputs(conmat, behav, covariates)
  behav <- normalized$behav
  covariates <- normalized$covariates

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action
  )

  pred <- init_pred(behav)
  training <- prepare_training_data(
    conmat,
    behav,
    covariates,
    include_cases
  )

  edges <- select_edges(
    training$conmat,
    training$behav,
    params$thresh_method,
    params$thresh_level
  )
  model <- train_cpm_model(
    training$conmat,
    training$behav,
    edges,
    params$bias_correct
  )
  pred[include_cases, ] <- predict_cpm_model(model, training$conmat)

  real <- behav
  real[include_cases] <- training$behav

  new_cpm(
    call = call,
    behav = real,
    pred = pred,
    edges = if (return_edges) edges,
    model = model,
    spec = object,
    params = list(
      covariates = !is.null(covariates),
      thresh_method = params$thresh_method,
      thresh_level = params$thresh_level,
      return_edges = return_edges,
      na_action = na_action,
      bias_correct = params$bias_correct
    )
  )
}

train_cpm_model <- function(conmat, behav, edges, bias_correct) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  x <- matrix(
    1,
    nrow = dim(conmat)[1],
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x[, corr_type] <- Rfast::rowsums(
      conmat[, edges[, corr_type], drop = FALSE]
    )
  }

  models <- lapply(inc_edges, function(inc_edge) {
    cur_x <- if (inc_edge == "both") {
      x
    } else {
      x[, c("(Intercept)", inc_edge)]
    }
    stats::.lm.fit(cur_x, behav)$coefficients
  })
  names(models) <- inc_edges

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edges,
    models = models
  )
}

predict_cpm_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  x_new <- matrix(
    1,
    nrow = dim(conmat_new)[1],
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x_new[, corr_type] <- Rfast::rowsums(
      conmat_new[, model$edges[, corr_type], drop = FALSE]
    )
  }

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(inc_edges),
    dimnames = list(NULL, inc_edges)
  )
  for (inc_edge in inc_edges) {
    cur_x_new <- if (inc_edge == "both") {
      x_new
    } else {
      x_new[, c("(Intercept)", inc_edge)]
    }
    pred[, inc_edge] <- cur_x_new %*% model$models[[inc_edge]]
  }

  pred
}
