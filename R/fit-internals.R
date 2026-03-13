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

fit_cpm_single <- function(
  call,
  object,
  conmat,
  behav,
  covariates,
  return_edges,
  na_action
) {
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

  if (is.null(covariates)) {
    conmat_train <- conmat[include_cases, , drop = FALSE]
    behav_train <- behav[include_cases]
  } else {
    covariates_train <- covariates[include_cases, , drop = FALSE]
    conmat_train <- regress_covariates(
      conmat[include_cases, , drop = FALSE],
      covariates_train
    )
    behav_train <- drop(regress_covariates(
      behav[include_cases],
      covariates_train
    ))
  }

  cur_edges <- select_edges(
    conmat_train,
    behav_train,
    params$thresh_method,
    params$thresh_level
  )
  model <- train_cpm_model(
    conmat_train,
    behav_train,
    cur_edges,
    params$bias_correct
  )
  pred[include_cases, ] <- predict_cpm_model(model, conmat_train)

  edges <- switch(
    return_edges,
    none = NULL,
    sum = cur_edges,
    all = {
      edge_array <- array(
        dim = c(dim(cur_edges), 1L),
        dimnames = list(NULL, corr_types, NULL)
      )
      edge_array[,, 1] <- cur_edges
      edge_array
    }
  )

  real <- behav
  real[include_cases] <- behav_train

  new_cpm(
    call = call,
    folds = list(include_cases),
    behav = real,
    pred = pred,
    edges = edges,
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
