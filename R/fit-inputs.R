check_names <- function(data, behav) {
  if (!is.null(rownames(data)) && !is.null(names(behav))) {
    if (!identical(rownames(data), names(behav))) {
      stop(
        sprintf(
          "Case names of `%s` must match those of behavior data.",
          deparse1(substitute(data))
        )
      )
    }
  }
  invisible()
}

normalize_inputs <- function(conmat, behav, covariates = NULL) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.")
  }
  if (nrow(conmat) != length(behav)) {
    stop("Case numbers of `conmat` and `behav` must match.")
  }
  check_names(conmat, behav)

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop("Case numbers of `covariates` and `behav` must match.")
    }
    check_names(covariates, behav)
  }

  list(
    behav = behav,
    covariates = covariates
  )
}

resolve_include_cases <- function(conmat, behav, covariates, na_action) {
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

resolve_kfolds <- function(kfolds, include_cases) {
  if (is.null(kfolds)) {
    return(length(include_cases))
  }
  kfolds
}

init_edges <- function(return_edges, conmat, kfolds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_types), kfolds),
      dimnames = list(NULL, edge_types, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_types)),
      dimnames = list(NULL, edge_types)
    ),
    none = NULL
  )
}

init_pred <- function(behav) {
  matrix(
    nrow = length(behav),
    ncol = length(prediction_types),
    dimnames = list(names(behav), prediction_types)
  )
}
