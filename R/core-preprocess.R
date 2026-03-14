core_regress_covariates <- function(resp, covariates) {
  stats::.lm.fit(cbind(1, covariates), resp)$residuals
}

core_regress_covariates_by_train <- function(
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

core_prepare_training_data <- function(conmat, behav, covariates, rows_train) {
  if (is.null(covariates)) {
    return(list(
      conmat = conmat[rows_train, , drop = FALSE],
      behav = behav[rows_train],
      covariates = NULL
    ))
  }

  covariates_train <- covariates[rows_train, , drop = FALSE]
  list(
    conmat = core_regress_covariates(
      conmat[rows_train, , drop = FALSE],
      covariates_train
    ),
    behav = drop(core_regress_covariates(
      behav[rows_train],
      covariates_train
    )),
    covariates = covariates_train
  )
}

core_prepare_assessment_data <- function(
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

  conmat_regressed <- core_regress_covariates_by_train(
    conmat[rows_train, , drop = FALSE],
    conmat[rows_test, , drop = FALSE],
    covariates_train,
    covariates_test
  )
  behav_regressed <- core_regress_covariates_by_train(
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
