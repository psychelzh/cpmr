test_that("normalize_inputs validates and normalizes data", {
  conmat <- matrix(rnorm(20), ncol = 2)
  behav <- matrix(rnorm(10), ncol = 1)
  covariates <- rnorm(10)

  normalized <- normalize_inputs(conmat, behav, covariates)

  expect_true(is.vector(normalized$behav))
  expect_equal(length(normalized$behav), nrow(conmat))
  expect_true(is.matrix(normalized$covariates))
  expect_equal(nrow(normalized$covariates), nrow(conmat))
})

test_that("complete_case_rows returns intersection in exclude mode", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  behav[2] <- NA
  covariates[3, 1] <- NA

  include_cases <- complete_case_rows(
    conmat,
    behav,
    covariates,
    na_action = "exclude"
  )

  expect_identical(include_cases, 4:10)
})

test_that("complete_case_rows fails clearly on missing required inputs", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  expect_error(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    "Missing values found in `conmat`",
    fixed = TRUE
  )

  conmat[1, 1] <- 0
  behav[1] <- NA
  expect_error(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    "Missing values found in `behav`",
    fixed = TRUE
  )

  behav[1] <- 0
  covariates[1, 1] <- NA
  expect_error(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    "Missing values found in `covariates`",
    fixed = TRUE
  )
})

test_that("complete_case_rows returns all rows in fail mode without missing data", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  expect_identical(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    seq_along(behav)
  )
})

test_that("normalize_inputs checks case-name alignment directly", {
  conmat <- matrix(1:10, nrow = 2, dimnames = list(c("a", "b"), NULL))
  behav <- stats::setNames(1:2, c("a", "c"))
  covariates <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), NULL))

  expect_error(
    normalize_inputs(conmat, behav, covariates),
    "Case names of `conmat` must match",
    fixed = TRUE
  )

  rownames(conmat) <- names(behav)
  rownames(covariates) <- c("a", "b")
  expect_error(
    normalize_inputs(conmat, behav, covariates),
    "Case names of `covariates` must match",
    fixed = TRUE
  )

  expect_silent(normalize_inputs(conmat, unname(behav), covariates))
})

test_that("assemble_fold_predictions annotates rows with fold ids", {
  observed <- c(1, 2, 3, 4)
  split_results <- list(
    list(
      observed = c(2, 4),
      predictions = cbind(
        joint = c(2.1, 4.1),
        positive = c(2.2, 4.2),
        negative = c(1.9, 3.9)
      )
    ),
    list(
      observed = c(1, 3),
      predictions = cbind(
        joint = c(1.1, 3.1),
        positive = c(1.2, 3.2),
        negative = c(0.9, 2.9)
      )
    )
  )
  folds <- list(c(2L, 4L), c(1L, 3L))

  collected <- assemble_fold_predictions(observed, folds, split_results)

  expect_named(
    collected,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(collected$row, 1:4)
  expect_equal(collected$fold, c(2L, 1L, 2L, 1L))
  expect_equal(collected$observed, observed)
})

test_that("init_edge_storage allocates expected structures", {
  conmat <- matrix(rnorm(40), ncol = 4)

  edges_sum <- init_edge_storage("sum", conmat, n_folds = 5)
  expect_equal(dim(edges_sum), c(ncol(conmat), 2))
  expect_identical(colnames(edges_sum), c("positive", "negative"))

  edges_all <- init_edge_storage("all", conmat, n_folds = 5)
  expect_equal(dim(edges_all), c(ncol(conmat), 2, 5))

  expect_null(init_edge_storage("none", conmat, n_folds = 5))
})

test_that("fold-wise edge masks agree with individual fits and summed storage", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(12 * 20), nrow = 12)
  behav <- conmat[, 1] + rnorm(12, sd = 0.1)
  folds <- list(c(1L, 4L, 7L), c(2L, 5L, 8L), c(3L, 6L, 9L), 10:12)
  settings <- spec()

  all <- cpm(
    conmat,
    behav,
    spec = settings,
    resamples = folds,
    return_edges = "all"
  )
  summed <- cpm(
    conmat,
    behav,
    spec = settings,
    resamples = folds,
    return_edges = "sum"
  )
  none <- cpm(
    conmat,
    behav,
    spec = settings,
    resamples = folds,
    return_edges = "none"
  )

  expect_type(all$edges, "logical")
  expect_identical(dim(all$edges), c(ncol(conmat), 2L, length(folds)))
  for (fold in seq_along(folds)) {
    split <- fit_split(
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      rows_train = setdiff(seq_along(behav), folds[[fold]]),
      rows_test = folds[[fold]],
      selection_spec = settings$selection,
      construction_spec = settings$construction,
      model_spec = settings$model
    )
    expect_identical(all$edges[,, fold], split$edge_mask)
  }
  expect_true(any(all$edges))
  expect_equal(summed$edges, apply(all$edges, c(1, 2), sum))
  expect_null(none$edges)
  expect_identical(all$predictions, summed$predictions)
  expect_identical(all$predictions, none$predictions)
})

test_that("run_cpm_workflow matches cpm() outputs", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- spec(
    selection = cpm_selection_cor(
      criterion = "proportion",
      level = 0.2
    )
  )
  call <- quote(cpm(conmat = conmat, behav = behav, spec = spec))

  withr::local_seed(999)
  folds <- make_kfold_assessment_folds(seq_along(behav), 5)

  withr::local_seed(999)
  internal_result <- run_cpm_workflow(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    resamples = folds,
    return_edges = "sum",
    na_action = "fail",
    call = call
  )
  api_result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    resamples = folds,
    return_edges = "sum",
    na_action = "fail"
  )

  expect_identical(internal_result$folds, api_result$folds)
  expect_identical(internal_result$call, call)
  expect_equal(internal_result$edges, api_result$edges)
  expect_equal(internal_result$predictions, api_result$predictions)
})

test_that("run_cpm_workflow errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- spec()

  expect_error(
    run_cpm_workflow(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      resamples = NULL,
      return_edges = "sum",
      na_action = "exclude"
    ),
    "No complete-case observations available for CPM.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    run_cpm_workflow(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      resamples = NULL,
      return_edges = "sum",
      na_action = "exclude"
    ),
    "CPM requires at least 2 complete-case observations to define assessment folds.",
    fixed = TRUE
  )
})

test_that("warn_large_edge_storage signals large fold-wise storage", {
  expect_warning(
    warn_large_edge_storage(
      n_edges = 200000,
      n_folds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = FALSE
  )

  expect_invisible(warn_large_edge_storage(10, 5, "sum"))
})
