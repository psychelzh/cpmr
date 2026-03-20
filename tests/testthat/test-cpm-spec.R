test_that("cpm_spec stores staged model parameters", {
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "proportion",
      level = 0.05
    ),
    construction = cpm_construction_strength(
      polarity = "net",
      weighting = cpm_weighting("sigmoid", scale = 0.02),
      standardize_edges = FALSE
    ),
    model = cpm_model_lm()
  )

  expect_s3_class(spec, "cpm_spec")
  expect_s3_class(spec$helpers$selection, "cpm_selection_spec")
  expect_s3_class(spec$helpers$construction, "cpm_construction_spec")
  expect_s3_class(spec$helpers$model, "cpm_model_spec")
  expect_identical(spec$params$selection$type, "cor")
  expect_identical(spec$params$selection$method, "spearman")
  expect_identical(spec$params$selection$criterion, "proportion")
  expect_identical(spec$params$selection$level, 0.05)
  expect_identical(spec$params$construction$type, "strength")
  expect_identical(spec$params$construction$polarity, "net")
  expect_identical(spec$params$construction$weighting$method, "sigmoid")
  expect_identical(spec$params$construction$weighting$scale, 0.02)
  expect_false(spec$params$construction$standardize_edges)
  expect_identical(spec$params$model$type, "lm")
})

test_that("new_cpm_spec builds cpm_spec objects", {
  params <- list(
    selection = list(
      type = "cor",
      method = "pearson",
      criterion = "p_value",
      level = 0.05
    ),
    construction = list(
      type = "strength",
      polarity = "separate",
      weighting = list(method = "binary", scale = 0.05),
      standardize_edges = TRUE
    ),
    model = list(type = "lm")
  )

  spec <- new_cpm_spec(params = params)

  expect_s3_class(spec, "cpm_spec")
  expect_identical(spec$params, params)
  expect_s3_class(spec$helpers$selection, "cpm_selection_spec")
  expect_s3_class(spec$helpers$construction, "cpm_construction_spec")
  expect_s3_class(spec$helpers$model, "cpm_model_spec")
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

  expect_identical(spec$params$selection$type, "cor")
  expect_identical(spec$params$selection$method, "pearson")
  expect_identical(spec$params$selection$criterion, "p_value")
  expect_identical(spec$params$selection$level, 0.01)
  expect_identical(spec$params$construction$type, "strength")
  expect_identical(spec$params$construction$polarity, "separate")
  expect_identical(spec$params$construction$weighting$method, "binary")
  expect_false(spec$params$construction$standardize_edges)
})

test_that("helper constructors validate scalar parameter values", {
  expect_error(
    cpm_selection_cor(level = -0.1),
    "`level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(level = c(0.1, 0.2)),
    "`level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_strength(standardize_edges = NA),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_weighting(scale = 0),
    "`scale` must be a single positive number.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_strength(standardize_edges = c(TRUE, FALSE)),
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
      "`cpm_construction_strength()`."
    ),
    fixed = TRUE
  )
  expect_error(
    cpm_spec(model = list()),
    "`model` must be created by `cpm_model_lm()`.",
    fixed = TRUE
  )
})

test_that("helper constructors round-trip through params", {
  expect_s3_class(cpm_model_lm(), "cpm_model_spec")
  expect_identical(cpm_model_lm()$type, "lm")
  expect_identical(format_model_type("custom"), "custom")
  expect_error(
    cpm_model_from_params("bogus"),
    "`model` must be a supported CPM outcome model.",
    fixed = TRUE
  )
  expect_error(
    selection_from_params(
      "bogus",
      method = "pearson",
      criterion = "p_value",
      level = 0.1
    ),
    "`selection` must be a supported CPM selection type.",
    fixed = TRUE
  )
  expect_error(
    construction_from_params(
      "bogus",
      polarity = "separate",
      edge_weighting = "binary",
      weighting_scale = 0.05,
      standardize_edges = FALSE
    ),
    "`construction` must be a supported CPM construction type.",
    fixed = TRUE
  )
})

test_that("print methods show readable staged settings", {
  spec <- cpm_spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_strength(
      polarity = "net",
      weighting = cpm_weighting("sigmoid", scale = 0.03),
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

  selection <- cpm_selection_cor(
    method = "spearman",
    criterion = "absolute",
    level = 0.1
  )
  expect_output(print(selection), "CPM selection \\(correlation\\)")
  expect_output(print(selection), "Criterion:\\s+absolute")

  construction <- cpm_construction_strength(
    polarity = "net",
    weighting = cpm_weighting("sigmoid", scale = 0.03),
    standardize_edges = TRUE
  )
  expect_output(print(construction), "CPM construction \\(network strength\\)")
  expect_output(print(construction), "Polarity:\\s+net")
  expect_output(print(construction), "Edge standardization:\\s+z-score")
})

test_that("net construction yields a single prediction stream", {
  withr::local_seed(101)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(
    construction = cpm_construction_strength(polarity = "net")
  )

  result <- fit(spec, conmat = conmat, behav = behav)

  expect_named(result$predictions, c("row", "real", "net"))
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
    construction = cpm_construction_strength(
      weighting = cpm_weighting("sigmoid", scale = 0.03)
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

  res <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)

  expect_s3_class(res, "cpm_resamples")
  expect_identical(as.character(res$call[[1]]), "fit_resamples")
  expect_s3_class(res$spec, "cpm_spec")
  expect_identical(length(res$folds), 5L)
  expect_equal(nrow(res$predictions), 10)
  expect_named(
    res$predictions,
    c("row", "fold", "real", "joint", "positive", "negative")
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

test_that("fit_resamples validates kfolds", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 1),
    "`kfolds` must be NULL or a single integer greater than or equal to 2.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 2.5),
    "`kfolds` must be NULL or a single integer greater than or equal to 2.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 11),
    "`kfolds` must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      kfolds = 5,
      resamples = list(1:5, 6:10)
    ),
    "Specify either `resamples` or `kfolds`, not both.",
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
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 2),
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
    kfolds = 5,
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
    kfolds = 5,
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
    kfolds = 6,
    return_edges = "sum"
  )

  pred <- res$predictions
  expect_true(isTRUE(all(stats::complete.cases(pred$joint))))
  expect_true(isTRUE(res$params$covariates))
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
  fold_edges <- select_edges(
    conmat = training$conmat,
    behav = training$behav,
    selection_method = spec$params$selection$method,
    selection_criterion = spec$params$selection$criterion,
    selection_level = spec$params$selection$level
  )
  fold_model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edge_screen = screen_edges(
      conmat = training$conmat,
      behav = training$behav,
      selection_method = spec$params$selection$method,
      selection_criterion = spec$params$selection$criterion,
      selection_level = spec$params$selection$level,
      edge_weighting = spec$params$construction$weighting$method,
      weighting_scale = spec$params$construction$weighting$scale
    ),
    standardize_edges = spec$params$construction$standardize_edges,
    construction_polarity = spec$params$construction$polarity,
    model_spec = spec$helpers$model
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
  fold_pred <- predict_model(fold_model, assessment$conmat)
  collected <- resampled$predictions

  expect_equal(single_fit$edges, fold_edges)
  expect_equal(single_fit$model$edges, fold_model$edges)
  expect_equal(single_fit$model$outcome_models, fold_model$outcome_models)
  expect_equal(single_fit$model$center, fold_model$center)
  expect_equal(single_fit$model$scale, fold_model$scale)
  expect_equal(predict_model(single_fit$model, assessment$conmat), fold_pred)
  expect_equal(
    as.matrix(collected[rows_test, c("joint", "positive", "negative")]),
    fold_pred,
    ignore_attr = TRUE
  )
  expect_equal(collected$real[rows_test], assessment$behav)
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
