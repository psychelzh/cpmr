check_names <- function(data, outcome) {
  if (!is.null(rownames(data)) && !is.null(names(outcome))) {
    if (!identical(rownames(data), names(outcome))) {
      stop(
        sprintf(
          "Case names of `%s` must match those of outcome data.",
          deparse1(substitute(data))
        )
      )
    }
  }

  invisible()
}
