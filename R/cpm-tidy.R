#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy. Use
#'   `"metrics"` for metric tables derived from [summary.cpm()], or
#'   `"edges"` for stored edge output, or `"tuning"` for nested threshold
#'   candidate scores.
#' @param level Metric table to return when `component = "metrics"`.
#'   `"pooled"` returns RMSE, MAE, and out-of-fold correlations; `"foldwise"`
#'   returns one correlation row per fold and prediction stream. The pooled
#'   table's `descriptive_only` column flags correlations computed when
#'   assessment folds contain fewer than three observations. When
#'   `component = "tuning"`, the result contains candidate-level scores and
#'   the prediction stream and level selected within each outer fold.
#' @return A [tibble][tibble::tibble-package] with component-specific columns.
#' @export
tidy.cpm <- function(
  x,
  ...,
  component = c("metrics", "edges", "tuning"),
  level = c("foldwise", "pooled")
) {
  component <- match.arg(component)
  level <- match.arg(level)
  sum_x <- summary(x, ...)

  switch(
    component,
    metrics = tibble::as_tibble(sum_x$tables[[level]]),
    edges = {
      if (is.null(sum_x$edges)) {
        stop(
          paste0(
            "Edge output is unavailable because this `cpm` object was fit ",
            "with `return_edges = \"none\"`. Refit with `return_edges = ",
            "\"sum\"` or `\"all\"` to tidy edges."
          ),
          call. = FALSE
        )
      }
      tibble::as_tibble(apply(sum_x$edges, 2, list))
    },
    tuning = {
      if (is.null(sum_x$tuning)) {
        stop(
          "Tuning output is unavailable because this `cpm` object was fit without `selection_levels`.",
          call. = FALSE
        )
      }
      tibble::as_tibble(sum_x$tuning)
    }
  )
}
