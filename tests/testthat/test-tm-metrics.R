test_that("CPM yardstick metrics cover data-frame and vector paths", {
  metrics_data <- tibble::tibble(
    truth = c(1, 2, 3, 4),
    estimate = c(1, 2, 4, 5)
  )

  cor_res <- cpm_cor(metrics_data, truth, estimate)
  spearman_res <- cpm_spearman(metrics_data, truth, estimate)

  expect_named(cor_res, c(".metric", ".estimator", ".estimate"))
  expect_named(spearman_res, c(".metric", ".estimator", ".estimate"))
  expect_equal(cpm_spearman_vec(1:4, c(2, 3, 4, 5)), 1)
  expect_equal(cpm_cor_vec(c(1, NA), c(1, 2), na_rm = FALSE), NA_real_)
  expect_equal(
    cpm_spearman_vec(c(1, NA), c(1, 2), na_rm = FALSE),
    NA_real_
  )
})
