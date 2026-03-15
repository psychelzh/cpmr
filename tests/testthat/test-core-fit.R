test_that("core_fit_xy returns a cpm_fit object with stored edges", {
  problem <- simulate_cpm_problem(n = 40, p = 20, seed = 11)

  fit <- core_fit_xy(
    conmat = problem$x,
    behav = problem$y,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    bias_correct = TRUE,
    network = "both"
  )

  expect_s3_class(fit, "cpm_fit")
  expect_identical(fit$network, "both")
  expect_equal(dim(fit$edges), c(20, 2))
  expect_named(fit$params, c("thresh_method", "thresh_level", "bias_correct"))
})

test_that("core_fit_xy requires at least three complete observations", {
  expect_error(
    core_fit_xy(matrix(1:4, nrow = 2), c(1, 2)),
    "At least 3 complete observations are required to fit CPM.",
    fixed = TRUE
  )
})

test_that("predict.cpm_fit returns numeric and raw predictions", {
  problem <- simulate_cpm_problem(n = 50, p = 25, seed = 12)
  train_rows <- 1:35
  test_rows <- 36:50

  fit <- core_fit_xy(
    conmat = problem$x[train_rows, , drop = FALSE],
    behav = problem$y[train_rows],
    thresh_method = "sparsity",
    thresh_level = 0.2,
    network = "pos"
  )

  pred_num <- predict(fit, problem$x[test_rows, , drop = FALSE])
  pred_raw <- predict(fit, problem$x[test_rows, , drop = FALSE], type = "raw")

  expect_type(pred_num, "double")
  expect_length(pred_num, length(test_rows))
  expect_s3_class(pred_raw, "tbl_df")
  expect_named(pred_raw, c("both", "pos", "neg"))
  expect_equal(pred_num, pred_raw$pos)
})

test_that("print.cpm_fit reports a concise fit summary", {
  problem <- simulate_cpm_problem(n = 24, p = 8, seed = 51)

  fit <- core_fit_xy(
    conmat = problem$x[1:18, , drop = FALSE],
    behav = problem$y[1:18],
    thresh_method = "alpha",
    thresh_level = 0.1,
    network = "both"
  )

  printed <- paste(capture.output(print(fit)), collapse = "\n")
  expect_match(printed, "CPM engine fit")
  expect_match(printed, "Threshold method: alpha")
})
