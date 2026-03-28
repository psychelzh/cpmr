test_that("run_single_fit returns a single-fold cpm object", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- spec(
    selection = cpm_selection_cor(
      criterion = "p_value",
      level = 0.05
    )
  )
  call <- quote(cpm(conmat = conmat, behav = behav, spec = spec))

  internal_result <- run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    na_action = "fail",
    call = call
  )
  expect_s3_class(internal_result, "cpm")
  expect_identical(as.character(internal_result$call[[1]]), "cpm")
  expect_length(internal_result$folds, 1)
  expect_identical(internal_result$folds[[1]], seq_along(behav))
  expect_equal(dim(internal_result$edges), c(ncol(conmat), 2))
  expect_equal(nrow(internal_result$predictions), length(behav))
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

test_that("run_resample_fit matches cpm() outputs", {
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
  internal_result <- run_resample_fit(
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

test_that("run_single_fit errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- spec()
  call <- quote(cpm(conmat = conmat, behav = behav, spec = spec))

  expect_error(
    run_single_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      na_action = "exclude",
      call = call
    ),
    "No complete-case observations available for fitting.",
    fixed = TRUE
  )

  behav[1:2] <- rnorm(2)
  expect_error(
    run_single_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      na_action = "exclude",
      call = call
    ),
    "CPM fitting requires at least 3 training observations.",
    fixed = TRUE
  )
})

test_that("run_resample_fit errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- spec()
  expect_error(
    run_resample_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      resamples = NULL,
      return_edges = "sum",
      na_action = "exclude"
    ),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    run_resample_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      resamples = NULL,
      return_edges = "sum",
      na_action = "exclude"
    ),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )
})
