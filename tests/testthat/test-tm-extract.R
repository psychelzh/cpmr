test_that("extract helpers reject unsupported inputs and validate payloads", {
  problem <- simulate_cpm_problem(n = 30, p = 10, seed = 71)
  fit <- core_fit_xy(
    conmat = problem$x,
    behav = problem$y,
    thresh_method = "sparsity",
    thresh_level = 0.2
  )

  expect_identical(collect_edges(extract_cpm_edges(fit)), fit$edges)
  expect_error(
    collect_edges(1),
    "supports fitted CPM model objects",
    fixed = FALSE
  )
  expect_error(
    extract_cpm_edges(structure(list(fit = 1), class = "model_fit")),
    "only supports fitted `model_fit` objects",
    fixed = FALSE
  )
  expect_error(
    extract_cpm_edges(1),
    "requires a fitted `cpm_fit`, `model_fit`, or fitted `workflow` object",
    fixed = FALSE
  )

  good_edges <- matrix(
    c(TRUE, FALSE, FALSE, TRUE),
    ncol = 2,
    dimnames = list(c("edge_1", "edge_2"), edge_signs)
  )

  expect_error(
    new_cpm_edge_extract(numeric(), good_edges),
    "`predictors` must be a non-empty character vector.",
    fixed = TRUE
  )
  expect_error(
    new_cpm_edge_extract(c("edge_1", "edge_2"), matrix(1, nrow = 1)),
    "`edges` must be a logical matrix.",
    fixed = TRUE
  )
  expect_error(
    new_cpm_edge_extract(
      c("edge_1", "edge_2"),
      matrix(TRUE, nrow = 2, ncol = 2, dimnames = list(NULL, c("a", "b")))
    ),
    "`edges` must have columns named `pos` and `neg`.",
    fixed = TRUE
  )
  expect_error(
    new_cpm_edge_extract(c("edge_1"), good_edges),
    "`predictors` and `edges` must describe the same number of rows.",
    fixed = TRUE
  )
})

test_that("resample edge helpers cover empty and malformed extract states", {
  testthat::skip_if_not_installed("tune")

  empty_tune <- structure(
    tibble::tibble(
      id = character(),
      .extracts = list(),
      .config = character()
    ),
    class = c("tune_results", "tbl_df", "tbl", "data.frame")
  )
  wrong_tune <- structure(
    tibble::tibble(
      id = "Fold1",
      .extracts = list(1),
      .config = "pre0_mod0_post0"
    ),
    class = c("tune_results", "tbl_df", "tbl", "data.frame")
  )

  expect_error(
    collect_edges(empty_tune),
    "control_resamples\\(extract = extract_cpm_edges\\)",
    fixed = FALSE
  )
  expect_error(
    collect_edges(wrong_tune),
    "control_resamples\\(extract = extract_cpm_edges\\)",
    fixed = FALSE
  )
  expect_error(
    validate_selected_only(NA),
    "`selected_only` must be `TRUE` or `FALSE`.",
    fixed = TRUE
  )

  empty_extracts <- tibble::tibble(
    id = character(),
    .extracts = list()
  )
  expect_equal(
    nrow(expand_cpm_edge_extracts(
      extracts = empty_extracts,
      meta_cols = character(),
      resample_id_cols = "id",
      type = "all",
      selected_only = TRUE
    )),
    0
  )

  none_selected <- tibble::tibble(
    id = "Fold1",
    .extracts = list(new_cpm_edge_extract(
      predictors = c("edge_1", "edge_2"),
      edges = matrix(
        FALSE,
        nrow = 2,
        ncol = 2,
        dimnames = list(c("edge_1", "edge_2"), edge_signs)
      )
    ))
  )
  expanded_none <- expand_cpm_edge_extracts(
    extracts = none_selected,
    meta_cols = character(),
    resample_id_cols = "id",
    type = "all",
    selected_only = TRUE
  )
  expect_named(expanded_none, c("id", "predictor", "pos", "neg"))
  expect_equal(nrow(expanded_none), 0)

  expanded_empty <- tibble::tibble(
    id = character(),
    predictor = character(),
    pos = logical(),
    neg = logical()
  )
  summarized_empty <- summarize_cpm_edge_extracts(
    expanded = expanded_empty,
    meta_cols = character(),
    resample_id_cols = "id",
    type = "sum",
    selected_only = TRUE
  )
  expect_named(summarized_empty, c("predictor", "pos", "neg", "n_folds"))
  expect_equal(nrow(summarized_empty), 0)

  summarized_without_ids <- summarize_cpm_edge_extracts(
    expanded = tibble::tibble(
      predictor = c("edge_1", "edge_1"),
      pos = c(TRUE, FALSE),
      neg = c(FALSE, TRUE)
    ),
    meta_cols = character(),
    resample_id_cols = character(),
    type = "sum",
    selected_only = FALSE
  )
  expect_equal(summarized_without_ids$n_folds, 2)
})
