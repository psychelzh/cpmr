test_that("cpm_reg creates a registered parsnip model specification", {
  spec <- cpm_reg(
    thresh_method = "sparsity",
    thresh_level = 0.2,
    bias_correct = FALSE,
    network = "neg"
  )

  expect_s3_class(spec, "cpm_reg")
  expect_s3_class(spec, "model_spec")
  expect_identical(spec$engine, "cpmr")
  expect_identical(rlang::eval_tidy(spec$args$network), "neg")
  expect_no_error(parsnip::translate(spec))
})

test_that("parsnip fit and predict work with cpm_reg", {
  problem <- simulate_cpm_problem(n = 60, p = 24, seed = 21)
  train <- problem$data[1:45, ]
  test <- problem$data[46:60, ]

  spec <- cpm_reg(
    thresh_method = "sparsity",
    thresh_level = 0.2,
    network = "both"
  )

  fit_obj <- fit(spec, y ~ ., data = train)
  pred_num <- predict(fit_obj, test)
  pred_raw <- predict(fit_obj, test, type = "raw")

  expect_s3_class(fit_obj, "model_fit")
  expect_named(pred_num, ".pred")
  expect_equal(nrow(pred_num), nrow(test))
  expect_s3_class(pred_raw, "tbl_df")
  expect_named(pred_raw, c("both", "pos", "neg"))
  expect_equal(collect_edges(fit_obj), fit_obj$fit$edges)
})

test_that("custom dials helpers return parameter objects", {
  expect_s3_class(cpm_network(), "param")
  expect_s3_class(cpm_thresh_method(), "param")
  expect_s3_class(cpm_bias_correct(), "param")
})
