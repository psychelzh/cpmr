#' @export
print.cpm <- function(x, ...) {
  cv <- if (length(unique(x$folds)) == length(x$real)) {
    "leave-one-out"
  } else {
    sprintf("%d-fold", length(unique(x$folds)))
  }
  cat(sprintf("CPM results based on %s cross validation.\n", cv))
  invisible(x)
}
