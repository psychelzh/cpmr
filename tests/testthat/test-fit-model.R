test_that("critical_r matches the t-statistic conversion", {
  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)

  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )
})

test_that("select_edges returns a logical positive/negative mask", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  edges <- select_edges(
    conmat = conmat,
    behav = behav,
    association_method = "pearson",
    threshold_method = "alpha",
    threshold_level = 0.1
  )

  expect_equal(dim(edges), c(ncol(conmat), 2))
  expect_type(edges, "logical")
  expect_identical(colnames(edges), c("positive", "negative"))
})

test_that("select_edges warns when sparsity selection drops one edge sign", {
  behav <- 1:10
  conmat <- cbind(behav, behav * 2, behav * 3, behav * 4)

  expect_warning(
    select_edges(
      conmat = conmat,
      behav = behav,
      association_method = "pearson",
      threshold_method = "sparsity",
      threshold_level = 0.25
    ),
    "The requested sparsity level did not retain both positive and negative edges.",
    fixed = TRUE
  )
})

test_that("select_sparsity_thresholds applies per-sign proportions", {
  associations <- c(seq_len(20), -seq_len(80))

  cutoffs <- select_sparsity_thresholds(associations, proportion = 0.1)
  mask <- threshold_edges_by_cutoffs(associations, cutoffs)

  expect_equal(unname(cutoffs[["positive"]]), 19)
  expect_equal(unname(cutoffs[["negative"]]), 73)
  expect_equal(colSums(mask), c(positive = 2, negative = 8))
})

test_that("sparsity thresholding retains tied edges at the cutoff", {
  associations <- c(rep(1, 4), rep(-1, 4))

  cutoffs <- select_sparsity_thresholds(associations, proportion = 0.25)
  mask <- threshold_edges_by_cutoffs(associations, cutoffs)

  expect_equal(cutoffs, c(positive = 1, negative = 1))
  expect_equal(colSums(mask), c(positive = 4, negative = 4))
})

test_that("sign_sparsity_cutoff handles empty scores and zero proportions", {
  expect_identical(sign_sparsity_cutoff(numeric(), proportion = 0.1), Inf)
  expect_identical(sign_sparsity_cutoff(c(3, 2, 1), proportion = 0), Inf)
})

test_that("select_edges validates threshold method", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  expect_error(
    select_edges(
      conmat = conmat,
      behav = behav,
      association_method = "pearson",
      threshold_method = "bogus",
      threshold_level = 0.1
    ),
    "`threshold_method` must be one of \"alpha\", \"sparsity\", or \"effect_size\".",
    fixed = TRUE
  )
})

test_that("select_edges supports spearman and effect_size screening", {
  behav <- c(1, 2, 3, 4, 5, 6)
  conmat <- cbind(
    behav^3,
    c(1, 4, 9, 16, 25, 36),
    c(6, 5, 4, 3, 2, 1),
    c(3, 1, 4, 1, 5, 9)
  )

  spearman_edges <- select_edges(
    conmat = conmat,
    behav = behav,
    association_method = "spearman",
    threshold_method = "effect_size",
    threshold_level = 0.8
  )

  expect_true(all(spearman_edges[1:2, "positive"]))
  expect_true(isTRUE(spearman_edges[3, "negative"]))
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

  edge_screen <- screen_edges(
    conmat = training$conmat,
    behav = training$behav,
    association_method = "pearson",
    threshold_method = "alpha",
    threshold_level = 0.1
  )
  model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_screen = edge_screen,
    bias_correct = TRUE,
    feature_space = "separate",
    model_spec = cpm_model_lm()
  )

  expect_equal(dim(edge_screen$mask), c(p, 2))
  expect_named(model$outcome_models, c("joint", "positive", "negative"))
  expect_equal(
    dim(predict_model(model, assessment$conmat)),
    c(length(rows_test), 3)
  )
})

test_that("net feature space with lm model produces a single stream", {
  withr::local_seed(2)
  conmat <- matrix(rnorm(40), nrow = 8, ncol = 5)
  behav <- rnorm(8)
  edge_screen <- list(
    mask = matrix(
      c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    weights = matrix(
      c(1, 0, 0, 1, 1, 0, 0, 1, 0, 1),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    edge_weighting = "binary",
    weighting_scale = 0.05,
    thresholds = c(positive = 0.1, negative = 0.1)
  )

  model <- train_model(
    conmat = conmat,
    behav = behav,
    edge_screen = edge_screen,
    bias_correct = FALSE,
    feature_space = "net",
    model_spec = cpm_model_lm()
  )
  pred <- predict_model(model, conmat)

  expect_named(model$outcome_models, "net")
  expect_identical(colnames(pred), "net")
  expect_equal(dim(pred), c(nrow(conmat), 1))
})

test_that("joint stream handles aliased empty-sign features", {
  network_strengths <- cbind(
    positive = rep(0, 5),
    negative = 1:5
  )
  behav <- 1:5

  fitted <- fit_prediction_model(
    network_strengths = network_strengths,
    behav = behav,
    feature_space = "separate",
    prediction_stream = "joint",
    model_spec = cpm_model_lm()
  )

  expect_true(is.na(fitted$coefficients[["positive"]]))
  expect_identical(fitted$prediction_coefficients[["positive"]], 0)
  expect_equal(
    predict_prediction_model(
      fitted_model = fitted,
      network_strengths = network_strengths,
      feature_space = "separate",
      prediction_stream = "joint"
    ),
    behav
  )
})

test_that("sigmoid edge weighting yields smooth edge weights", {
  behav <- c(1, 2, 3, 4, 5, 6, 7, 8)
  conmat <- cbind(
    behav,
    behav + c(0, 0, 0, 1, 1, 1, 2, 2),
    rev(behav),
    c(1, 2, 1, 2, 1, 2, 1, 2)
  )

  edge_screen <- screen_edges(
    conmat = conmat,
    behav = behav,
    association_method = "pearson",
    threshold_method = "effect_size",
    threshold_level = 0.4,
    edge_weighting = "sigmoid",
    weighting_scale = 0.05
  )

  expect_true(is.double(edge_screen$weights))
  expect_true(any(edge_screen$weights > 0 & edge_screen$weights < 1))
  expect_true(all(edge_screen$weights[edge_screen$mask] >= 0.5))
  expect_true(all(edge_screen$weights[!edge_screen$mask] <= 0.5))
})

test_that("compute_edge_weights validates weighting mode", {
  expect_error(
    compute_edge_weights(
      associations = c(0.2, -0.3),
      cutoffs = c(positive = 0.1, negative = 0.1),
      mask = matrix(
        c(TRUE, FALSE, FALSE, TRUE),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      edge_weighting = "bogus",
      weighting_scale = 0.05
    ),
    "`edge_weighting` must be either \"binary\" or \"sigmoid\".",
    fixed = TRUE
  )
})

test_that("smooth_threshold_weights returns zeros for infinite cutoffs", {
  expect_equal(
    smooth_threshold_weights(c(0.1, 0.2, NA_real_), cutoff = Inf, scale = 0.05),
    c(0, 0, 0)
  )
})

test_that("prediction helpers validate unsupported modes", {
  network_strengths <- cbind(
    positive = c(1, 2),
    negative = c(3, 4)
  )

  expect_error(
    prediction_features(
      network_strengths = network_strengths,
      feature_space = "separate",
      prediction_stream = "bogus"
    ),
    paste0(
      "`prediction_stream` must be one of ",
      "\"joint\", \"positive\", or \"negative\" for ",
      "`feature_space = \"separate\"`."
    ),
    fixed = TRUE
  )
  expect_error(
    prediction_features(
      network_strengths = network_strengths,
      feature_space = "bogus",
      prediction_stream = "joint"
    ),
    "`feature_space` must be either \"separate\" or \"net\".",
    fixed = TRUE
  )
  expect_error(
    fit_outcome_model(
      features = matrix(1:2, ncol = 1),
      behav = 1:2,
      model_spec = structure(list(type = "bogus"), class = "cpm_model_spec")
    ),
    "`model` must be a supported CPM outcome model.",
    fixed = TRUE
  )
  expect_error(
    prediction_streams_for_feature_space("bogus"),
    "`feature_space` must be either \"separate\" or \"net\".",
    fixed = TRUE
  )
})

test_that("predict_outcome_model validates unsupported model types", {
  expect_error(
    predict_outcome_model(
      fitted_model = list(type = "bogus"),
      features = matrix(1:2, ncol = 1)
    ),
    "`model` must be a supported CPM outcome model.",
    fixed = TRUE
  )
})
