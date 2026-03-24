test_that("critical_r matches the t-statistic conversion", {
  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)

  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )
})

test_that("critical_r validates alpha bounds", {
  expect_identical(critical_r(20, 0), Inf)
  expect_error(
    critical_r(20, -0.1),
    "`alpha` must be a single number between 0 and 1.",
    fixed = TRUE
  )
})

test_that("select_edge_mask returns a logical positive/negative mask", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  edges <- select_edge_mask(
    conmat = conmat,
    behav = behav,
    criterion = "p_value",
    level = 0.1
  )

  expect_equal(dim(edges), c(ncol(conmat), 2))
  expect_type(edges, "logical")
  expect_identical(colnames(edges), c("positive", "negative"))
})

test_that("p-value selection at zero retains no edges", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  expect_warning(
    edges <- select_edge_mask(
      conmat = conmat,
      behav = behav,
      criterion = "p_value",
      level = 0
    ),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_false(any(edges))
})

test_that("select_edge_mask warns when proportion selection drops one edge sign", {
  behav <- 1:10
  conmat <- cbind(behav, behav * 2, behav * 3, behav * 4)

  expect_warning(
    select_edge_mask(
      conmat = conmat,
      behav = behav,
      criterion = "proportion",
      level = 0.25
    ),
    "The requested sparsity level did not retain both positive and negative edges.",
    fixed = TRUE
  )
})

test_that("select_sparsity_thresholds applies per-sign proportions", {
  associations <- c(seq_len(20), -seq_len(80))

  cutoffs <- select_sparsity_thresholds(associations, proportion = 0.1)
  mask <- edge_mask_from_cutoffs(associations, cutoffs)

  expect_equal(unname(cutoffs[["positive"]]), 19)
  expect_equal(unname(cutoffs[["negative"]]), 73)
  expect_equal(colSums(mask), c(positive = 2, negative = 8))
})

test_that("sparsity thresholding retains tied edges at the cutoff", {
  associations <- c(rep(1, 4), rep(-1, 4))

  cutoffs <- select_sparsity_thresholds(associations, proportion = 0.25)
  mask <- edge_mask_from_cutoffs(associations, cutoffs)

  expect_equal(cutoffs, c(positive = 1, negative = 1))
  expect_equal(colSums(mask), c(positive = 4, negative = 4))
})

test_that("sign_sparsity_cutoff handles empty scores and zero proportions", {
  expect_identical(sign_sparsity_cutoff(numeric(), proportion = 0.1), Inf)
  expect_identical(sign_sparsity_cutoff(c(3, 2, 1), proportion = 0), Inf)
})

test_that("select_edge_mask validates selection criterion", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  expect_error(
    select_edge_mask(
      conmat = conmat,
      behav = behav,
      criterion = "bogus",
      level = 0.1
    ),
    "'arg' should be one of",
    fixed = FALSE
  )
})

test_that("select_edge_mask supports spearman and absolute screening", {
  behav <- c(1, 2, 3, 4, 5, 6)
  conmat <- cbind(
    behav^3,
    c(1, 4, 9, 16, 25, 36),
    c(6, 5, 4, 3, 2, 1),
    c(3, 1, 4, 1, 5, 9)
  )

  spearman_edges <- select_edge_mask(
    conmat = conmat,
    behav = behav,
    method = "spearman",
    criterion = "absolute",
    level = 0.8
  )

  expect_true(all(spearman_edges[1:2, "positive"]))
  expect_true(isTRUE(spearman_edges[3, "negative"]))
})

test_that("run_edge_selection accepts staged selection specs directly", {
  behav <- c(1, 2, 3, 4, 5, 6)
  conmat <- cbind(
    behav^2,
    rev(behav),
    c(1, 1, 2, 2, 3, 3)
  )

  from_spec <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.8
    )
  )
  from_scalars <- select_edge_mask(
    conmat = conmat,
    behav = behav,
    method = "spearman",
    criterion = "absolute",
    level = 0.8
  )

  expect_equal(from_spec$mask, from_scalars)
})

test_that("fit_split_model and predict_split_model compose correctly", {
  withr::local_seed(1)
  n <- 12
  p <- 6
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:8
  rows_test <- 9:12

  covariates_train <- covariates[rows_train, , drop = FALSE]
  conmat_resid <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates_train,
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  behav_resid <- regress_covariates_by_train(
    resp_train = behav[rows_train],
    resp_test = behav[rows_test],
    cov_train = covariates_train,
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  training_conmat <- conmat_resid$train
  training_behav <- drop(behav_resid$train)
  assessment_conmat <- conmat_resid$test

  edge_selection <- run_edge_selection(
    conmat = training_conmat,
    behav = training_behav,
    selection_spec = cpm_selection_cor(
      criterion = "p_value",
      level = 0.1
    )
  )
  model <- fit_split_model(
    conmat = training_conmat,
    behav = training_behav,
    edge_selection = edge_selection,
    construction_spec = cpm_spec(
      construction = cpm_construction_summary(
        sign_mode = "separate",
        weight_scale = 0,
        standardize_edges = TRUE
      )
    )$construction,
    model_spec = cpm_model_lm()
  )

  expect_equal(dim(edge_selection$mask), c(p, 2))
  expect_named(model$outcome_models, c("joint", "positive", "negative"))
  expect_equal(
    dim(predict_split_model(model, assessment_conmat)),
    c(length(rows_test), 3)
  )
  expect_equal(
    predict_split_model(model),
    predict_split_model(model, training_conmat)
  )
})

test_that("net sign mode with lm model produces a single stream", {
  withr::local_seed(2)
  conmat <- matrix(rnorm(40), nrow = 8, ncol = 5)
  behav <- rnorm(8)
  edge_selection <- list(
    associations = c(0.2, -0.2, NA_real_, 0.3, 0.4),
    mask = matrix(
      c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    thresholds = c(positive = 0.1, negative = 0.1)
  )

  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = cpm_spec(
      construction = cpm_construction_summary(
        sign_mode = "net",
        weight_scale = 0,
        standardize_edges = FALSE
      )
    )$construction,
    model_spec = cpm_model_lm()
  )
  pred <- predict_split_model(model, conmat)

  expect_named(model$outcome_models, "net")
  expect_identical(colnames(pred), "net")
  expect_equal(dim(pred), c(nrow(conmat), 1))
})

test_that("fit_split_model handles aliased empty-sign features", {
  conmat <- cbind(
    rep(0, 5),
    1:5
  )
  behav <- 1:5
  edge_selection <- list(
    associations = c(0.3, -0.3),
    mask = matrix(
      c(TRUE, FALSE, FALSE, TRUE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    thresholds = c(positive = 0.1, negative = 0.1)
  )
  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = cpm_spec()$construction,
    model_spec = cpm_model_lm()
  )
  fitted <- model$outcome_models$joint
  pred <- predict_split_model(model, conmat)

  expect_true(is.na(fitted$coefficients[["positive_summary"]]))
  expect_identical(fitted$prediction_coefficients[["positive_summary"]], 0)
  expect_equal(drop(pred[, "joint"]), behav)
})

test_that("sigmoid edge weighting yields smooth edge weights", {
  behav <- c(1, 2, 3, 4, 5, 6, 7, 8)
  conmat <- cbind(
    behav,
    behav + c(0, 0, 0, 1, 1, 1, 2, 2),
    rev(behav),
    c(1, 2, 1, 2, 1, 2, 1, 2)
  )

  edge_selection <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = cpm_selection_cor(
      criterion = "absolute",
      level = 0.4
    )
  )
  edge_weights <- edge_weights_summary(
    associations = edge_selection$associations,
    cutoffs = edge_selection$thresholds,
    mask = edge_selection$mask,
    weight_scale = 0.05
  )

  expect_true(is.double(edge_weights))
  expect_true(any(edge_weights > 0 & edge_weights < 1))
  expect_true(all(edge_weights[edge_selection$mask] >= 0.5))
  expect_true(all(edge_weights[!edge_selection$mask] <= 0.5))
})

test_that("edge_weights_summary returns zeros for infinite cutoffs", {
  expect_equal(
    edge_weights_summary(
      associations = c(0.1, 0.2, NA_real_),
      cutoffs = c(positive = Inf, negative = Inf),
      mask = matrix(
        FALSE,
        nrow = 3,
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      weight_scale = 0.05
    ),
    matrix(
      0,
      nrow = 3,
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    )
  )
})

test_that("edge_weights_summary returns binary weights when scale is zero", {
  mask <- matrix(
    c(TRUE, FALSE, FALSE, TRUE),
    ncol = 2,
    dimnames = list(NULL, c("positive", "negative"))
  )

  expect_equal(
    edge_weights_summary(
      associations = c(0.2, -0.3),
      cutoffs = c(positive = 0.1, negative = 0.1),
      mask = mask,
      weight_scale = 0
    ),
    matrix(
      c(1, 0, 0, 1),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    )
  )
})

test_that("feature_matrix_summary skips empty edge signs in weighted mode", {
  conmat <- cbind(c(1, 2), c(3, 4))
  edge_weights <- cbind(
    positive = c(0.5, 0),
    negative = c(0, 0)
  )

  expect_equal(
    feature_matrix_summary(
      conmat = conmat,
      edge_mask = matrix(FALSE, nrow = 2, ncol = 2),
      edge_weights = edge_weights
    ),
    cbind(
      positive_summary = c(0.5, 1),
      negative_summary = c(0, 0)
    )
  )
})

test_that("prediction helpers validate unsupported modes", {
  construction_state <- list(
    construction = cpm_spec()$construction,
    center = NULL,
    scale = NULL,
    edge_mask = NULL,
    edge_weights = NULL,
    summaries = cbind(
      positive_summary = c(1, 2),
      negative_summary = c(3, 4)
    )
  )

  expect_error(
    run_edge_selection(
      conmat = matrix(1:6, nrow = 3),
      behav = 1:3,
      selection_spec = structure(
        list(type = "bogus"),
        class = "cpm_selection_spec"
      )
    ),
    "`selection` must be a supported CPM selection spec.",
    fixed = TRUE
  )
  expect_error(
    build_construction_state(
      conmat = matrix(1:6, nrow = 3),
      edge_selection = list(),
      construction_spec = structure(
        list(type = "bogus"),
        class = "cpm_construction_spec"
      )
    ),
    "`construction` must be a supported CPM construction spec.",
    fixed = TRUE
  )
  expect_error(
    construction_features(
      utils::modifyList(
        construction_state,
        list(
          construction = structure(
            list(type = "bogus"),
            class = "cpm_construction_spec"
          )
        )
      )
    ),
    "`construction` must be a supported CPM construction spec.",
    fixed = TRUE
  )
  expect_error(
    construction_features(
      utils::modifyList(
        construction_state,
        list(
          construction = structure(
            list(type = "summary", sign_mode = "bogus"),
            class = "cpm_construction_spec"
          )
        )
      )
    ),
    "`construction$sign_mode` must be a supported summary construction mode.",
    fixed = TRUE
  )

  construction_state_net <- utils::modifyList(
    construction_state,
    list(
      construction = cpm_spec(
        construction = cpm_construction_summary(
          sign_mode = "net",
          weight_scale = 0,
          standardize_edges = FALSE
        )
      )$construction
    )
  )

  feature_sets <- construction_features(construction_state)
  expect_named(feature_sets, c("joint", "positive", "negative"))
  expect_equal(feature_sets$joint, construction_state$summaries)
  expect_equal(
    construction_features(construction_state_net)$net,
    matrix(c(-2, -2), ncol = 1, dimnames = list(NULL, "net_summary"))
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
})

test_that("construction_features returns cached summary feature sets", {
  construction_state <- list(
    construction = cpm_spec(
      construction = cpm_construction_summary(sign_mode = "separate")
    )$construction,
    center = NULL,
    scale = NULL,
    edge_mask = matrix(
      c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    edge_weights = NULL,
    summaries = cbind(
      positive_summary = c(1, 2),
      negative_summary = c(3, 4)
    )
  )

  expect_equal(
    construction_features(construction_state)$joint,
    construction_state$summaries
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
