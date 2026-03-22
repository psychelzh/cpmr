test_that("cpm_spec stores staged model parameters", {
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "proportion",
      level = 0.05
    ),
    construction = cpm_construction_summary(
      polarity = "net",
      weight_scale = 0.02,
      standardize_edges = FALSE
    ),
    model = cpm_model_lm()
  )

  expect_s3_class(spec, "cpm_spec")
  expect_s3_class(spec$selection, "cpm_selection_spec")
  expect_s3_class(spec$construction, "cpm_construction_spec")
  expect_s3_class(spec$model, "cpm_model_spec")
  expect_identical(spec$selection$type, "cor")
  expect_identical(spec$selection$method, "spearman")
  expect_identical(spec$selection$criterion, "proportion")
  expect_identical(spec$selection$level, 0.05)
  expect_identical(spec$construction$type, "summary")
  expect_identical(spec$construction$polarity, "net")
  expect_identical(spec$construction$prediction_streams, "net")
  expect_identical(spec$construction$weight_scale, 0.02)
  expect_false(spec$construction$standardize_edges)
  expect_identical(spec$model$type, "lm")
})

test_that("new_cpm_spec builds cpm_spec objects", {
  selection <- list(
    type = "cor",
    method = "pearson",
    criterion = "p_value",
    level = 0.05
  )
  construction <- list(
    type = "summary",
    polarity = "separate",
    weight_scale = 0,
    standardize_edges = TRUE
  )
  model <- list(type = "lm")

  spec <- new_cpm_spec(
    selection = selection,
    construction = construction,
    model = model
  )

  expect_s3_class(spec, "cpm_spec")
  expect_identical(spec$selection, selection)
  expect_identical(spec$construction, construction)
  expect_identical(spec$model, model)
})

test_that("fit.cpm_spec returns a cpm object with correct call", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()
  result <- fit(spec, conmat = conmat, behav = behav)
  expect_s3_class(result, "cpm")
  expect_identical(as.character(result$call[[1]]), "fit")
  expect_s3_class(result$spec, "cpm_spec")
  expect_identical(result$spec, spec)
})

test_that("cpm_spec defaults to classic CPM edge handling", {
  spec <- cpm_spec()

  expect_identical(spec$selection$type, "cor")
  expect_s3_class(spec$selection, "cpm_selection_spec")
  expect_identical(spec$selection$method, "pearson")
  expect_identical(spec$selection$criterion, "p_value")
  expect_identical(spec$selection$level, 0.01)
  expect_identical(spec$construction$type, "summary")
  expect_s3_class(spec$construction, "cpm_construction_spec")
  expect_identical(spec$construction$polarity, "separate")
  expect_identical(
    spec$construction$prediction_streams,
    c("joint", "positive", "negative")
  )
  expect_identical(spec$construction$weight_scale, 0)
  expect_false(spec$construction$standardize_edges)
  expect_s3_class(spec$model, "cpm_model_spec")
})

test_that("helper constructors validate scalar parameter values", {
  expect_error(
    cpm_selection_cor(level = -0.1),
    "`level` must be in (0, 1] when `criterion = \"p_value\"`.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(level = c(0.1, 0.2)),
    "`level` must be a single finite number.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "absolute", level = -0.1),
    "`level` must be between 0 and 1 when `criterion = \"absolute\"`.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "proportion", level = 1.1),
    "`level` must be between 0 and 1 when `criterion = \"proportion\"`.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(level = 0),
    "`level` must be in (0, 1] when `criterion = \"p_value\"`.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(level = 1),
    paste(
      "`level = 1` with `criterion = \"p_value\"` effectively",
      "disables p-value filtering."
    ),
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 0),
    paste(
      "`level = 0` with `criterion = \"absolute\"` effectively",
      "disables absolute-correlation filtering."
    ),
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 1),
    paste(
      "`level = 1` with `criterion = \"absolute\"` retains only",
      "perfect absolute-correlation edges."
    ),
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 0),
    paste(
      "`level = 0` with `criterion = \"proportion\"` retains no",
      "edges by design."
    ),
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 1),
    paste(
      "`level = 1` with `criterion = \"proportion\"` retains all",
      "eligible edges."
    ),
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(standardize_edges = NA),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(weight_scale = -0.1),
    "`weight_scale` must be a single non-negative number.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(standardize_edges = c(TRUE, FALSE)),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_spec(selection = list()),
    "`selection` must be created by `cpm_selection_cor()`.",
    fixed = TRUE
  )
  expect_error(
    cpm_spec(construction = list()),
    paste(
      "`construction` must be created by",
      "`cpm_construction_summary()`."
    ),
    fixed = TRUE
  )
  expect_error(
    cpm_spec(model = list()),
    "`model` must be created by `cpm_model_lm()`.",
    fixed = TRUE
  )
})

test_that("cpm_spec validates helper schema instead of class alone", {
  bad_selection <- structure(
    list(type = "cor"),
    class = "cpm_selection_spec"
  )
  expect_error(
    cpm_spec(selection = bad_selection),
    "`selection$method` must be one of \"pearson\", \"spearman\".",
    fixed = TRUE
  )

  bad_construction <- structure(
    list(type = "summary", polarity = "sideways", weight_scale = 0),
    class = "cpm_construction_spec"
  )
  expect_error(
    cpm_spec(construction = bad_construction),
    "`construction$polarity` must be one of \"separate\", \"net\".",
    fixed = TRUE
  )

  bad_model <- structure(
    list(type = "glm"),
    class = "cpm_model_spec"
  )
  expect_error(
    cpm_spec(model = bad_model),
    "`model$type` must be one of \"lm\".",
    fixed = TRUE
  )
})

test_that("validate_cpm_component checks message-builder inputs", {
  valid_selection <- cpm_selection_cor()

  expect_error(
    validate_cpm_component(
      valid_selection,
      component = c("selection", "construction"),
      constructor = "cpm_selection_cor"
    ),
    "`component` must be a single string.",
    fixed = TRUE
  )
  expect_error(
    validate_cpm_component(
      valid_selection,
      component = "selection",
      constructor = NA_character_
    ),
    "`constructor` must be a single string.",
    fixed = TRUE
  )
})

test_that("helper constructors round-trip through params", {
  expect_s3_class(cpm_model_lm(), "cpm_model_spec")
  expect_identical(cpm_model_lm()$type, "lm")
  expect_identical(format_model_type("custom"), "custom")
})

test_that("validate_construction_spec derives prediction streams", {
  separate <- validate_construction_spec(cpm_construction_summary())
  net <- validate_construction_spec(
    cpm_construction_summary(polarity = "net")
  )

  expect_identical(
    separate$prediction_streams,
    c("joint", "positive", "negative")
  )
  expect_identical(net$prediction_streams, "net")
})

test_that("print.cpm_spec shows readable staged settings", {
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_summary(
      polarity = "net",
      weight_scale = 0.03,
      standardize_edges = TRUE
    )
  )

  expect_output(print(spec), "CPM specification")
  expect_output(print(spec), "Selection")
  expect_output(print(spec), "Method:\\s+spearman")
  expect_output(print(spec), "Criterion:\\s+absolute")
  expect_output(print(spec), "Construction")
  expect_output(print(spec), "Polarity:\\s+net")
  expect_output(print(spec), "Outcome model:\\s+linear regression")
})

test_that("net construction yields a single prediction stream", {
  withr::local_seed(101)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(
    construction = cpm_construction_summary(polarity = "net")
  )

  result <- fit(spec, conmat = conmat, behav = behav)

  expect_named(result$predictions, c("row", "observed", "net"))
  expect_named(result$model$outcome_models, "net")
})

test_that("sigmoid edge weighting stores smooth edge weights in the model", {
  withr::local_seed(202)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_summary(
      weight_scale = 0.03
    )
  )

  result <- fit(spec, conmat = conmat, behav = behav)

  expect_true(is.double(result$model$edge_weights))
  expect_equal(dim(result$model$edge_weights), c(ncol(conmat), 2))
  expect_true(any(
    result$model$edge_weights > 0 & result$model$edge_weights < 1
  ))
})

test_that("fit_resamples returns predictions and folds", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(spec, conmat = conmat, behav = behav, resamples = 5)

  expect_s3_class(res, "cpm_resamples")
  expect_identical(as.character(res$call[[1]]), "fit_resamples")
  expect_s3_class(res$spec, "cpm_spec")
  expect_identical(length(res$folds), 5L)
  expect_equal(nrow(res$predictions), 10)
  expect_named(
    res$predictions,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_null(res$edges)
  expect_s3_class(summary(res), "cpm_resamples_summary")
})

test_that("fit_resamples accepts custom resample indices", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  resamples <- list(1:3, 4:6, 7:10)
  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    na_action = "exclude",
    resamples = resamples
  )

  expect_identical(length(res$folds), length(resamples))
  expect_identical(res$folds, resamples)
})

test_that("fit_resamples validates fold-count resamples", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = 1),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = 2.5),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = 11),
    "`resamples` as a fold count must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
})

test_that("fit_resamples validates custom resamples", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1:6, 6:10)
    ),
    "must not overlap",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = list(1:5)),
    "at least 2 assessment sets",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(1.5, 2.5), 3:10)
    ),
    "integer-valued indices",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(0, 1), 2:10)
    ),
    "positive indices",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(1, 1), 2:10)
    ),
    "must not contain duplicates",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1:4, 5:8)
    ),
    "must cover all complete-case rows exactly once",
    fixed = TRUE
  )
})

test_that("fit_resamples errors clearly for insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, na_action = "exclude"),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, na_action = "exclude"),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )
})

test_that("fit_resamples errors when any fold leaves fewer than 3 training rows", {
  conmat <- matrix(rnorm(40), ncol = 10)
  behav <- rnorm(4)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = 2),
    "Each resample must leave at least 3 complete-case training observations.",
    fixed = TRUE
  )

  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1L, 2:4)
    ),
    "Each resample must leave at least 3 complete-case training observations.",
    fixed = TRUE
  )
})

test_that("fit_resamples can store summed edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    resamples = 5,
    return_edges = "sum"
  )

  edges <- res$edges
  expect_type(edges, "double")
  expect_equal(dim(edges), c(12, 2))
})

test_that("fit_resamples can store fold-wise edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    resamples = 5,
    return_edges = "all"
  )

  edges <- res$edges
  expect_type(edges, "logical")
  expect_equal(dim(edges), c(12, 2, 5))
})

test_that("fit_resamples handles covariates in assessment pipeline", {
  withr::local_seed(42)
  n <- 24
  p <- 18
  cov <- rnorm(n)
  conmat <- matrix(rnorm(n * p) + rep(cov, p), nrow = n, ncol = p)
  behav <- cov * 1.5 + rnorm(n)
  covariates <- matrix(cov, ncol = 1)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = 6,
    return_edges = "sum"
  )

  pred <- res$predictions
  expect_true(isTRUE(all(stats::complete.cases(pred$joint))))
  expect_true(isTRUE(res$settings$covariates))
  expect_equal(length(res$folds), 6L)
})

test_that("fit_resamples fold path matches fit() on the same training subset", {
  withr::local_seed(7)
  n <- 15
  p <- 12
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      criterion = "p_value",
      level = 0.1
    )
  )
  resamples <- list(1:5, 6:10, 11:15)
  rows_test <- resamples[[1]]
  rows_train <- setdiff(seq_len(n), rows_test)

  single_fit <- fit(
    spec,
    conmat = conmat[rows_train, , drop = FALSE],
    behav = behav[rows_train],
    covariates = covariates[rows_train, , drop = FALSE],
    na_action = "fail"
  )
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
  fold_edges <- select_edge_mask(
    conmat = training$conmat,
    behav = training$behav,
    method = spec$selection$method,
    criterion = spec$selection$criterion,
    level = spec$selection$level
  )
  fold_model <- fit_split_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_selection = run_edge_selection(
      conmat = training$conmat,
      behav = training$behav,
      selection_spec = spec$selection
    ),
    construction_spec = spec$construction,
    model_spec = spec$model
  )
  resampled <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "fail"
  )
  fold_pred <- predict_split_model(fold_model, assessment$conmat)
  collected <- resampled$predictions

  expect_equal(single_fit$edges, fold_edges)
  expect_equal(single_fit$model$edges, fold_model$edges)
  expect_equal(single_fit$model$outcome_models, fold_model$outcome_models)
  expect_equal(single_fit$model$center, fold_model$center)
  expect_equal(single_fit$model$scale, fold_model$scale)
  expect_equal(
    predict_split_model(single_fit$model, assessment$conmat),
    fold_pred
  )
  expect_equal(
    as.matrix(collected[rows_test, c("joint", "positive", "negative")]),
    fold_pred,
    ignore_attr = TRUE
  )
  expect_equal(collected$observed[rows_test], assessment$behav)
})

test_that("fit_resamples excludes incomplete rows consistently with covariates", {
  withr::local_seed(11)
  n <- 15
  p <- 8
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- cpm_spec()

  behav[2] <- NA_real_
  conmat[5, 3] <- NA_real_
  covariates[8, 1] <- NA_real_

  include_cases <- setdiff(seq_len(n), c(2L, 5L, 8L))
  resamples <- list(include_cases[1:4], include_cases[5:8], include_cases[9:12])
  resampled <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "exclude"
  )
  collected <- resampled$predictions

  expect_identical(resampled$folds, resamples)
  expect_equal(sort(collected$row[!is.na(collected$fold)]), include_cases)
  expect_true(isTRUE(all(stats::complete.cases(collected$joint[
    include_cases
  ]))))
  expect_true(isTRUE(all(is.na(collected$joint[-include_cases]))))
  expect_true(isTRUE(all(is.na(collected$fold[-include_cases]))))
})
