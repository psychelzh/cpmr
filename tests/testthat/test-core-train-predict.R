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
