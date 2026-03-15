test_that("edge selection helpers cover alpha, warnings, and invalid methods", {
  alpha_edges <- core_select_edges(
    conmat = matrix(rnorm(30), nrow = 10),
    behav = rnorm(10),
    method = "alpha",
    level = 0.2
  )

  expect_equal(dim(alpha_edges), c(3, 2))
  expect_gt(critical_r(10, 0.05), 0)
  expect_identical(critical_r(10, 0), Inf)
  expect_identical(critical_r(10, 1), 0)
  expect_warning(
    core_select_edges(
      conmat = cbind(1:10, 2:11, 3:12),
      behav = 1:10,
      method = "sparsity",
      level = 0.5
    ),
    "Not enough positive or negative correlation values.",
    fixed = TRUE
  )
  expect_error(
    core_select_edges(
      conmat = matrix(rnorm(30), nrow = 10),
      behav = rnorm(10),
      method = "bogus",
      level = 0.2
    ),
    "Invalid threshold method.",
    fixed = TRUE
  )

  deterministic_behav <- 1:10
  deterministic_conmat <- cbind(
    pos = deterministic_behav,
    neg = -deterministic_behav,
    mixed = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)
  )
  expect_no_warning(
    core_select_edges(
      conmat = deterministic_conmat,
      behav = deterministic_behav,
      method = "sparsity",
      level = 0
    )
  )
  expect_true(
    !any(core_select_edges(
      conmat = matrix(rnorm(30), nrow = 10),
      behav = rnorm(10),
      method = "alpha",
      level = 0
    ))
  )
})
