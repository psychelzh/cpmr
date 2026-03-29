example_tidy_result <- function() {
  withr::local_seed(123)
  conmat <- matrix(rnorm(200), nrow = 20)
  behav <- rnorm(20)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 4)
}

test_that("Basic case works for `tidy()`", {
  result <- example_tidy_result()

  pooled <- tidy(result, component = "metrics", level = "pooled")
  edges <- tidy(result, component = "edges")

  expect_named(pooled, c("prediction", "estimate", "method"))
  expect_true(all(c("joint", "positive", "negative") %in% pooled$prediction))
  expect_named(
    edges,
    c(
      "positive",
      "negative"
    )
  )
})

test_that("Support pass arguments of `summary()`", {
  result <- example_tidy_result()

  expect_equal(
    unique(
      tidy(
        result,
        component = "metrics",
        level = "pooled",
        method = "spearman"
      )$method
    ),
    "spearman"
  )
  expect_named(
    tidy(result, component = "edges"),
    c("positive", "negative")
  )
})

test_that("tidy supports pooled and foldwise metric tables", {
  result <- example_tidy_result()

  foldwise <- tidy(result, component = "metrics")
  pooled <- tidy(result, component = "metrics", level = "pooled")

  expect_true(all(
    c("fold", "n_assess", "prediction", "estimate", "method") %in%
      names(foldwise)
  ))
  expect_true(all(c("prediction", "estimate", "method") %in% names(pooled)))
  expect_true(all(pooled$method == "pearson"))
})

test_that("tidy edges errors clearly when edge storage is disabled", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec(),
    resamples = 5,
    return_edges = "none"
  )

  expect_error(
    tidy(result, component = "edges"),
    paste0(
      "Edge output is unavailable because this `cpm` object was fit ",
      "with `return_edges = \"none\"`. Refit with `return_edges = ",
      "\"sum\"` or `\"all\"` to tidy edges."
    ),
    fixed = TRUE
  )
})
