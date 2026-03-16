print_performance_block <- function(values, header, std_error = NULL) {
  cat(header)
  for (prediction_type in prediction_types) {
    line <- sprintf(
      "    %s: %s",
      prediction_label(prediction_type),
      format_cor(values[[prediction_type]])
    )
    if (!is.null(std_error)) {
      line <- sprintf(
        "%s (SE %s)",
        line,
        format_cor(std_error[[prediction_type]])
      )
    }
    cat(line, "\n", sep = "")
  }
}

print_edge_rate_block <- function(edges, header = "  Selected edges:\n") {
  if (is.null(edges)) {
    return(invisible(NULL))
  }

  cat(header)
  for (edge_type in edge_types) {
    cat(sprintf(
      "    %s: %s\n",
      prediction_label(edge_type),
      format_rate(safe_mean(edges[, edge_type]))
    ))
  }

  invisible(NULL)
}

format_cor <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
}

prediction_labels <- c(
  both = "Combined",
  pos = "Positive",
  neg = "Negative"
)

prediction_label <- function(prediction_type) {
  unname(prediction_labels[[prediction_type]])
}
