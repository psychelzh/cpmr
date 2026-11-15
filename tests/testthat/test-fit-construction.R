test_that("construction rejects unsupported types and sign modes", {
  construction_state <- list(
    construction = spec()$construction,
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
            list(type = "strength", sign_mode = "bogus"),
            class = "cpm_construction_spec"
          )
        )
      )
    ),
    paste0(
      "`construction$sign_mode` must be a supported CPM strength ",
      "construction mode."
    ),
    fixed = TRUE
  )

  construction_state_net <- utils::modifyList(
    construction_state,
    list(
      construction = spec(
        construction = cpm_construction_strength(
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
})

test_that("sigmoid edge weighting stores smooth edge weights in split models", {
  withr::local_seed(202)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  s <- spec(
    selection = cpm_selection_cor(
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_strength(
      weight_scale = 0.03
    )
  )

  edge_selection <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = s$selection
  )
  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = s$construction,
    model_spec = s$model
  )

  expect_true(is.double(model$edge_weights))
  expect_equal(dim(model$edge_weights), c(ncol(conmat), 2))
  expect_true(any(
    model$edge_weights > 0 & model$edge_weights < 1
  ))
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

test_that("selected-edge standardization matches full-matrix standardization", {
  withr::local_seed(123)
  train <- matrix(rnorm(80), nrow = 10)
  assessment <- matrix(rnorm(24), nrow = 3)
  mask <- cbind(
    positive = c(TRUE, FALSE, TRUE, rep(FALSE, 5)),
    negative = c(FALSE, TRUE, rep(FALSE, 6))
  )
  center <- Rfast::colmeans(train)
  scale <- Rfast::colVars(train, std = TRUE)
  weights <- cbind(
    positive = c(0.8, 0, 0.2, rep(0, 5)),
    negative = c(0, 0.6, rep(0, 6))
  )

  for (data in list(train, assessment, assessment[1, , drop = FALSE])) {
    for (edge_weights in list(NULL, weights)) {
      expected <- feature_matrix_summary(
        fscale(data, center, scale),
        mask,
        edge_weights
      )
      expect_equal(
        feature_matrix_summary(data, mask, edge_weights, center, scale),
        expected
      )
    }
  }
})

test_that("construction_features returns cached summary feature sets", {
  construction_state <- list(
    construction = spec(
      construction = cpm_construction_strength(sign_mode = "separate")
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
