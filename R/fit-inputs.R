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

regress_covariates_by_train <- function(
  resp_train,
  cov_train,
  resp_test = NULL,
  cov_test = NULL
) {
  x_train <- cbind(1, cov_train)
  model <- stats::.lm.fit(x_train, resp_train)

  if (is.null(resp_test)) {
    return(list(train = model$residuals))
  }

  x_test <- cbind(1, cov_test)
  list(
    train = model$residuals,
    test = resp_test - x_test %*% model$coefficients
  )
}
