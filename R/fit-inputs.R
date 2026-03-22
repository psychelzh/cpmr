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

prepare_training_data <- function(conmat, behav, covariates, rows_train) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows_train, , drop = FALSE],
      behav = behav[rows_train],
      covariates = NULL
    ))
  }

  covariates_train <- covariates[rows_train, , drop = FALSE]
  list(
    conmat = regress_covariates(
      conmat[rows_train, , drop = FALSE],
      covariates_train
    ),
    behav = drop(regress_covariates(
      behav[rows_train],
      covariates_train
    )),
    covariates = covariates_train
  )
}

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

regress_covariates <- function(resp, covariates) {
  stats::.lm.fit(cbind(1, covariates), resp)$residuals
}

regress_covariates_by_train <- function(
  resp_train,
  resp_test,
  cov_train,
  cov_test
) {
  x_train <- cbind(1, cov_train)
  model <- stats::.lm.fit(x_train, resp_train)
  x_test <- cbind(1, cov_test)
  list(
    train = model$residuals,
    test = resp_test - x_test %*% model$coefficients
  )
}
