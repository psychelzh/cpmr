# Shared constants for staged CPM fitting internals.

edge_signs <- c("positive", "negative")
summary_column_names <- stats::setNames(
  paste0(edge_signs, "_summary"),
  edge_signs
)
summary_columns <- unname(summary_column_names)
