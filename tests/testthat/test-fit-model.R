test_that("critical_r matches the t-statistic conversion", {
  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)

  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )
})

test_that("select_edges returns a logical pos/neg mask", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  edges <- select_edges(conmat, behav, "alpha", 0.1)

  expect_equal(dim(edges), c(ncol(conmat), 2))
  expect_type(edges, "logical")
  expect_identical(colnames(edges), c("pos", "neg"))
})

test_that("select_edges warns when sparsity selection drops one edge sign", {
  behav <- 1:10
  conmat <- cbind(behav, behav * 2, behav * 3, behav * 4)

  expect_warning(
    select_edges(conmat, behav, "sparsity", 0.25),
    "The requested sparsity level did not retain both positive and negative edges.",
    fixed = TRUE
  )
})

test_that("select_edges validates threshold method", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  expect_error(
    select_edges(conmat, behav, "bogus", 0.1),
    "`method` must be either \"alpha\" or \"sparsity\".",
    fixed = TRUE
  )
})

test_that("fscale centers and scales columns", {
  x <- matrix(as.numeric(1:6), nrow = 3)
  center <- c(2, 5)
  scale <- c(1, 2)

  expect_equal(
    fscale(x, center, scale),
    sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  )
})

test_that("train_model and predict_model compose correctly", {
  withr::local_seed(1)
  n <- 12
  p <- 6
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:8
  rows_test <- 9:12

  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )

  edges <- select_edges(training$conmat, training$behav, "alpha", 0.1)
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = edges,
    bias_correct = TRUE
  )

  expect_equal(dim(edges), c(p, 2))
  expect_named(model$models, c("both", "pos", "neg"))
  expect_equal(
    dim(predict_model(model, assessment$conmat)),
    c(length(rows_test), 3)
  )
})
