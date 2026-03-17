print_performance_block <- function(values, header, std_error = NULL) {
  cat(header)
  for (prediction_type in prediction_types) {
    line <- sprintf(
      "    %s: %s",
      prediction_label(prediction_type),
      format_cor(values[[prediction_type]])
    )
    if (!is.null(std_error) && !is.na(std_error[[prediction_type]])) {
      line <- sprintf(
        "%s (SE %s)",
        line,
        format_cor(std_error[[prediction_type]])
      )
    }
    cat(line, "\n", sep = "")
  }
}

print_error_block <- function(errors, header = "  Prediction error:\n") {
  cat(header)
  for (error_type in rownames(errors)) {
    cat(sprintf("    %s:\n", toupper(error_type)))
    for (prediction_type in prediction_types) {
      cat(sprintf(
        "      %s: %s\n",
        prediction_label(prediction_type),
        format_value(errors[error_type, prediction_type])
      ))
    }
  }

  invisible(NULL)
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

format_value <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", x))
}

format_cor <- function(x) {
  format_value(x)
}

format_rate <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.2f%%", x * 100))
}

format_threshold_level <- function(x) {
  ifelse(is.na(x), "NA", trimws(formatC(x, format = "fg", digits = 3)))
}

format_yes_no <- function(x) {
  ifelse(is.na(x), "NA", ifelse(isTRUE(x), "yes", "no"))
}

format_covariates <- function(x) {
  ifelse(is.na(x), "NA", ifelse(isTRUE(x), "included", "none"))
}

edge_storage_labels <- c(
  none = "not stored",
  sum = "summed across folds",
  all = "stored for each fold"
)

edge_storage_label <- function(x) {
  unname(edge_storage_labels[[x]])
}

prediction_labels <- c(
  both = "Combined",
  pos = "Positive",
  neg = "Negative"
)

prediction_label <- function(prediction_type) {
  unname(prediction_labels[[prediction_type]])
}
