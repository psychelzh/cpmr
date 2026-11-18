test_that("spec stores staged model parameters", {
  s <- spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "proportion",
      level = 0.05
    ),
    construction = cpm_construction_strength(
      sign_mode = "net",
      weight_scale = 0.02,
      standardize_edges = FALSE
    ),
    model = cpm_model_lm()
  )

  expect_s3_class(s, "cpm_spec")
  expect_s3_class(s$selection, "cpm_selection_spec")
  expect_s3_class(s$construction, "cpm_construction_spec")
  expect_s3_class(s$model, "cpm_model_spec")
  expect_identical(s$selection$type, "cor")
  expect_identical(s$selection$method, "spearman")
  expect_identical(s$selection$criterion, "proportion")
  expect_identical(s$selection$level, 0.05)
  expect_identical(s$construction$type, "strength")
  expect_identical(s$construction$sign_mode, "net")
  expect_identical(s$construction$weight_scale, 0.02)
  expect_false(s$construction$standardize_edges)
  expect_identical(s$model$type, "lm")
})

test_that("spec defaults to classic CPM edge handling", {
  spec <- spec()

  expect_identical(spec$selection$type, "cor")
  expect_s3_class(spec$selection, "cpm_selection_spec")
  expect_identical(spec$selection$method, "pearson")
  expect_identical(spec$selection$criterion, "p_value")
  expect_identical(spec$selection$level, 0.01)
  expect_identical(spec$construction$type, "strength")
  expect_s3_class(spec$construction, "cpm_construction_spec")
  expect_identical(spec$construction$sign_mode, "separate")
  expect_identical(spec$construction$weight_scale, 0)
  expect_false(spec$construction$standardize_edges)
  expect_s3_class(spec$model, "cpm_model_spec")
})

test_that("helper constructors validate scalar parameter values", {
  expect_error(
    cpm_selection_cor(criterion = "bogus"),
    "'arg' should be one of",
    fixed = FALSE
  )
  expect_error(
    cpm_selection_cor(level = -0.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(level = c(0.1, 0.2)),
    "`level` must be a single finite number.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "absolute", level = -0.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "proportion", level = 1.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_strength(standardize_edges = NA),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_strength(weight_scale = -0.1),
    "`weight_scale` must be a single non-negative number.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_strength(standardize_edges = c(TRUE, FALSE)),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    spec(selection = list()),
    "`selection` must be a `cpm_selection_spec` object.",
    fixed = TRUE
  )
  expect_error(
    spec(construction = list()),
    "`construction` must be a `cpm_construction_spec` object.",
    fixed = TRUE
  )
  expect_error(
    spec(model = list()),
    "`model` must be a `cpm_model_spec` object.",
    fixed = TRUE
  )
})

test_that("spec checks stage classes without unpacking stage internals", {
  custom_selection <- structure(
    list(type = "custom_selection"),
    class = "cpm_selection_spec"
  )
  custom_construction <- structure(
    list(type = "custom_construction"),
    class = "cpm_construction_spec"
  )
  custom_model <- structure(
    list(type = "custom_model"),
    class = "cpm_model_spec"
  )

  spec <- spec(
    selection = custom_selection,
    construction = custom_construction,
    model = custom_model
  )

  expect_identical(spec$selection, custom_selection)
  expect_identical(spec$construction, custom_construction)
  expect_identical(spec$model, custom_model)
})

test_that("helper constructors round-trip through params", {
  expect_s3_class(cpm_model_lm(), "cpm_model_spec")
  expect_identical(cpm_model_lm()$type, "lm")
})

test_that("print.cpm_spec shows readable staged settings", {
  spec <- spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_strength(
      sign_mode = "net",
      weight_scale = 0.03,
      standardize_edges = TRUE
    )
  )

  expect_output(print(spec), "CPM specification")
  expect_output(print(spec), "Selection")
  expect_output(print(spec), "Method:\\s+spearman")
  expect_output(print(spec), "Criterion:\\s+absolute")
  expect_output(print(spec), "Construction")
  expect_output(print(spec), "Sign mode:\\s+net")
  expect_output(print(spec), "Outcome model:\\s+linear regression")
})

test_that("print.cpm_spec shows default construction labels compactly", {
  expect_output(print(spec()), "Weight scale:\\s+none")
  expect_output(print(spec()), "Edge standardization:\\s+none")
})

test_that("print.cpm_spec displays unrecognized model names", {
  x <- spec()
  x$model$type <- "custom"

  expect_output(print(x), "Outcome model:\\s+custom")
})
