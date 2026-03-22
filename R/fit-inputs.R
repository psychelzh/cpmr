resolve_data_context <- function(
  conmat,
  behav,
  covariates,
  na_action
) {
  na_action <- match.arg(na_action, c("fail", "exclude"))

  context <- normalize_inputs(conmat, behav, covariates)

  include_cases <- resolve_include_cases(
    conmat,
    context$behav,
    context$covariates,
    na_action
  )

  list(
    behav = context$behav,
    covariates = context$covariates,
    include_cases = include_cases,
    na_action = na_action
  )
}

normalize_inputs <- function(conmat, behav, covariates = NULL) {
  behav <- drop(behav)
  if (!is.vector(behav) || !is.numeric(behav)) {
    stop("Behavior data must be a numeric vector.")
  }
  if (nrow(conmat) != length(behav)) {
    stop("The number of observations in `conmat` and `behav` must match.")
  }
  check_names(conmat, behav)

  if (!is.null(covariates)) {
    if (is.vector(covariates)) {
      covariates <- as.matrix(covariates)
    }
    if (nrow(covariates) != length(behav)) {
      stop("The number of observations in `covariates` and `behav` must match.")
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
