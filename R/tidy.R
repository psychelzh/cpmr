#' Tidy a `cpm` object
#'
#' @param x A `cpm` object.
#' @param ... Additional arguments passed to `summary()`.
#' @param component A character vector indicating the component to tidy.
#' @return A tibble with the following columns:
#'
#'   For `component = "performance"`:
#'
#'   \item{method}{The method used to calculate the correlation between the real
#'   and predicted values.}
#'
#'   \item{pos}{The correlation between the real and predicted values for
#'   positive edges.}
#'
#'   \item{neg}{The correlation between the real and predicted values for
#'   negative edges.}
#'
#'   For `component = "edges"`:
#'
#'   \item{level}{The proportional threshold for edge selection.}
#'
#'   \item{pos}{A logical vector indicating whether each edge is selected based
#'   on the edge_level (positive).}
#'
#'   \item{neg}{A logical vector indicating whether each edge is selected based
#'   on the edge_level (negative).}
#' @export
tidy.cpm <- function(x, ..., component = c("performance", "edges")) {
  component <- match.arg(component)
  sum_x <- summary(x, ...)
  switch(component,
    performance = tibble::tibble(
      method = sum_x$params$method,
      tibble::as_tibble(sum_x$performance)
    ),
    edges = {
      if (is.null(sum_x$edges)) {
        warning(
          "No edges stored in the object.\n",
          "* You probably called `cpm()` with `return_edges = \"none\"`."
        )
        return(tibble::tibble())
      }
      tibble::tibble(
        level = sum_x$params$edge_level,
        tibble::as_tibble(apply(sum_x$edges, 2, list))
      )
    }
  )
}
