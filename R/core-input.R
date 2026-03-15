edge_signs <- c("pos", "neg")
prediction_networks <- c("both", edge_signs)

core_repair_predictor_names <- function(conmat) {
  predictor_names <- colnames(conmat)
  if (is.null(predictor_names)) {
    predictor_names <- rep("", ncol(conmat))
  }

  missing_names <- is.na(predictor_names) | !nzchar(predictor_names)
  if (any(missing_names)) {
    predictor_names[missing_names] <- paste0("edge_", seq_len(ncol(conmat)))[
      missing_names
    ]
  }
  if (anyDuplicated(predictor_names)) {
    predictor_names <- make.unique(predictor_names, sep = "_")
  }

  colnames(conmat) <- predictor_names
  conmat
}

check_names <- function(data, outcome) {
  if (!is.null(rownames(data)) && !is.null(names(outcome))) {
    if (!identical(rownames(data), names(outcome))) {
      stop(
        sprintf(
          "Case names of `%s` must match those of outcome data.",
          deparse1(substitute(data))
        )
      )
    }
  }

  invisible()
}

core_validate_thresh_method <- function(method) {
  match.arg(method, c("alpha", "sparsity"))
}

core_validate_thresh_level <- function(level) {
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level) ||
      level < 0 ||
      level > 1
  ) {
    stop("`thresh_level` must be a single number between 0 and 1.")
  }

  as.numeric(level)
}

core_validate_bias_correct <- function(bias_correct) {
  if (
    !is.logical(bias_correct) ||
      length(bias_correct) != 1L ||
      is.na(bias_correct)
  ) {
    stop("`bias_correct` must be either TRUE or FALSE.")
  }

  bias_correct
}

core_validate_network <- function(network) {
  match.arg(network, prediction_networks)
}

core_warn_extreme_thresh_level <- function(method, level) {
  if (!level %in% c(0, 1)) {
    return(invisible())
  }

  warning(
    sprintf(
      paste0(
        "`thresh_level = %s` is a boundary setting for ",
        "`thresh_method = \"%s\"` and is usually not meaningful in practice."
      ),
      format(level, trim = TRUE),
      method
    ),
    call. = FALSE
  )

  invisible()
}

core_normalize_inputs <- function(conmat, behav, covariates = NULL) {
  conmat <- as.matrix(conmat)
  if (!is.numeric(conmat)) {
    stop("Predictor data must be numeric.")
  }
  conmat <- core_repair_predictor_names(conmat)

  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Outcome data must be a numeric vector.")
  }
  if (nrow(conmat) != length(behav)) {
    stop("Case numbers of `conmat` and `behav` must match.")
  }
  check_names(conmat, behav)

  if (!is.null(covariates)) {
    covariates <- as.matrix(covariates)
    if (!is.numeric(covariates)) {
      stop("Covariates must be numeric.")
    }
    if (nrow(covariates) != length(behav)) {
      stop("Case numbers of `covariates` and `behav` must match.")
    }
    check_names(covariates, behav)
  }

  list(
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )
}

core_resolve_include_cases <- function(conmat, behav, covariates, na_action) {
  switch(
    na_action,
    fail = {
      stopifnot(
        "Missing values found in `conmat`" = !anyNA(conmat),
        "Missing values found in `behav`" = !anyNA(behav),
        "Missing values found in `covariates`" = is.null(covariates) ||
          !anyNA(covariates)
      )
      seq_along(behav)
    },
    exclude = Reduce(
      intersect,
      list(
        which(stats::complete.cases(conmat)),
        which(stats::complete.cases(behav)),
        if (!is.null(covariates)) {
          which(stats::complete.cases(covariates))
        } else {
          seq_along(behav)
        }
      )
    )
  )
}

core_resolve_kfolds <- function(kfolds, include_cases) {
  if (is.null(kfolds)) {
    return(length(include_cases))
  }

  kfolds
}

core_init_edges <- function(return_edges, conmat, kfolds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), kfolds),
      dimnames = list(colnames(conmat), edge_signs, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_signs)),
      dimnames = list(colnames(conmat), edge_signs)
    ),
    none = NULL
  )
}

core_init_pred <- function(behav) {
  matrix(
    nrow = length(behav),
    ncol = length(prediction_networks),
    dimnames = list(names(behav), prediction_networks)
  )
}

core_prepare_prediction_matrix <- function(new_data, predictor_names = NULL) {
  if (is.data.frame(new_data)) {
    if (!all(vapply(new_data, is.numeric, logical(1)))) {
      stop("`new_data` must contain only numeric predictors.")
    }
    new_data <- data.matrix(new_data)
  } else {
    new_data <- as.matrix(new_data)
  }

  if (!is.numeric(new_data)) {
    stop("`new_data` must contain only numeric predictors.")
  }

  if (!is.null(predictor_names)) {
    if (!is.null(colnames(new_data))) {
      if (!all(predictor_names %in% colnames(new_data))) {
        stop(
          "`new_data` must contain the same predictor columns used at fit time."
        )
      }
      new_data <- new_data[, predictor_names, drop = FALSE]
    } else if (ncol(new_data) != length(predictor_names)) {
      stop(
        "`new_data` must have the same number of predictor columns used at fit time."
      )
    }
  }

  unname(new_data)
}
