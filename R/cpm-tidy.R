#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy. Use
#'   `"metrics"` for correlation tables derived from [summary.cpm()], or
#'   `"edges"` for stored edge output.
#' @param level Correlation table to return when `component = "metrics"`.
#'   `"pooled"` returns pooled out-of-fold correlations, and `"foldwise"`
#'   returns one row per fold and prediction stream.
#' @return A [tibble][tibble::tibble-package] with component-specific columns.
#' @export
tidy.cpm <- function(
  x,
  ...,
  component = c("metrics", "edges"),
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
    }
  )
}
