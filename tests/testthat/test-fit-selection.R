test_that("critical_r matches the t-statistic conversion", {
  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)

  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )
})

test_that("edge selection rejects unsupported selection types", {
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
})

test_that("edge selection rejects unsupported criteria", {
  selection <- cpm_selection_cor()
  selection$criterion <- "bogus"

  expect_error(
    run_edge_selection(matrix(1:6, nrow = 3), 1:3, selection),
    '`criterion` must be one of "p_value", "absolute", or "proportion".',
    fixed = TRUE
  )
})

test_that("covariate-adjusted screening uses effective residual degrees of freedom", {
  withr::local_seed(44)
  covariates <- matrix(rnorm(200), nrow = 20)
  basis <- qr.Q(qr(cbind(1, covariates, matrix(rnorm(40), nrow = 20))))
  behav <- basis[, 12]
  edge <- 0.5 * behav + sqrt(0.75) * basis[, 13]
  selection <- cpm_selection_cor(level = 0.05)

  unadjusted <- run_edge_selection(matrix(edge, ncol = 1), behav, selection)
  adjusted <- run_edge_selection(
    matrix(edge, ncol = 1),
    behav,
    selection,
    covariate_rank = 10
  )
  expect_true(unadjusted$mask[1, "positive"])
  expect_false(adjusted$mask[1, "positive"])
  expect_equal(
    adjusted$thresholds[["positive"]],
    sqrt(qt(0.975, 8)^2 / (qt(0.975, 8)^2 + 8))
  )

  covariates <- cbind(covariates, covariates[, 1])
  fit <- fit_split(
    conmat = matrix(c(edge, 0), ncol = 1),
    behav = c(behav, 0),
    covariates = rbind(covariates, rep(0, ncol(covariates))),
    rows_train = 1:20,
    rows_test = 21,
    selection_spec = selection,
    construction_spec = cpm_construction_strength(),
    model_spec = cpm_model_lm()
  )
  expect_false(fit$edge_mask[1, "positive"])
  expect_error(
    critical_r(5, 0.05, covariate_rank = 3),
    "positive residual degrees of freedom",
    fixed = TRUE
  )
  expect_error(
    cpm(
      matrix(rnorm(24), nrow = 6),
      rnorm(6),
      covariates = matrix(rnorm(12), nrow = 6),
      resamples = list(1:3, 4:6)
    ),
    "positive residual degrees of freedom",
    fixed = TRUE
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

test_that("edge selection returns a logical positive/negative mask", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  edges <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = cpm_selection_cor(criterion = "p_value", level = 0.1)
  )$mask

  expect_equal(dim(edges), c(ncol(conmat), 2))
  expect_type(edges, "logical")
  expect_identical(colnames(edges), c("positive", "negative"))
})

test_that("p-value selection at zero retains no edges", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  expect_warning(
    edges <- run_edge_selection(
      conmat = conmat,
      behav = behav,
      selection_spec = cpm_selection_cor(criterion = "p_value", level = 0)
    )$mask,
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_false(any(edges))
})

test_that("edge selection warns when proportion selection drops one edge sign", {
  behav <- 1:10
  conmat <- cbind(behav, behav * 2, behav * 3, behav * 4)

  expect_warning(
    run_edge_selection(
      conmat = conmat,
      behav = behav,
      selection_spec = cpm_selection_cor(criterion = "proportion", level = 0.25)
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

test_that("edge selection supports spearman and absolute screening", {
  behav <- c(1, 2, 3, 4, 5, 6)
  conmat <- cbind(
    behav^3,
    c(1, 4, 9, 16, 25, 36),
    c(6, 5, 4, 3, 2, 1),
    c(3, 1, 4, 1, 5, 9)
  )

  spearman_edges <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.8
    )
  )$mask

  expect_true(all(spearman_edges[1:2, "positive"]))
  expect_true(isTRUE(spearman_edges[3, "negative"]))
})
