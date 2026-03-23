print_performance_block <- function(values, header, std_error = NULL) {
  cat(header)
  for (prediction_stream in names(values)) {
    line <- sprintf(
      "    %s: %s",
      prediction_label(prediction_stream),
      format_cor(values[[prediction_stream]])
    )
    if (!is.null(std_error) && !is.na(std_error[[prediction_stream]])) {
      line <- sprintf(
        "%s (SE %s)",
        line,
        format_cor(std_error[[prediction_stream]])
      )
    }
    cat(line, "\n", sep = "")
  }
}

print_error_block <- function(errors, header = "  Prediction error:\n") {
  cat(header)
  for (error_type in rownames(errors)) {
    cat(sprintf("    %s:\n", toupper(error_type)))
    for (prediction_stream in colnames(errors)) {
      cat(sprintf(
        "      %s: %s\n",
        prediction_label(prediction_stream),
        format_value(errors[error_type, prediction_stream])
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
  for (edge_type in edge_signs) {
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
  trimws(formatC(x, format = "fg", digits = 3))
}

format_edge_standardization <- function(x) {
  if (isTRUE(x)) "z-score" else "none"
}

format_covariates <- function(x) {
  if (isTRUE(x)) "included" else "none"
}

format_method_name <- function(x) {
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

format_model_type <- function(model_type) {
  switch(
    model_type,
    lm = "linear regression",
    model_type
  )
}

format_weighting_label <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", "sigmoid")
}

format_weight_scale <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", format_threshold_level(weight_scale))
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
  joint = "Joint",
  net = "Net",
  positive = "Positive",
  negative = "Negative"
)

prediction_label <- function(prediction_stream) {
  unname(prediction_labels[[prediction_stream]])
}

print_setting_line <- function(label, value, indent = "    ", width = 22L) {
  cat(sprintf(
    "%s%-*s %s\n",
    indent,
    width,
    paste0(label, ":"),
    value
  ))
}

print_selection_settings <- function(
  selection,
  indent = "    ",
  method_label = "Method",
  criterion_label = "Criterion",
  level_label = "Level"
) {
  print_setting_line(method_label, selection$method, indent = indent)
  print_setting_line(criterion_label, selection$criterion, indent = indent)
  print_setting_line(
    level_label,
    format_threshold_level(selection$level),
    indent = indent
  )

  invisible(NULL)
}

print_construction_settings <- function(
  construction,
  indent = "    ",
  sign_mode_label = "Sign mode"
) {
  print_setting_line(
    sign_mode_label,
    construction$sign_mode,
    indent = indent
  )
  print_setting_line(
    "Edge weighting",
    format_weighting_label(construction$weight_scale),
    indent = indent
  )
  print_setting_line(
    "Weight scale",
    format_weight_scale(construction$weight_scale),
    indent = indent
  )
  print_setting_line(
    "Edge standardization",
    format_edge_standardization(construction$standardize_edges),
    indent = indent
  )

  invisible(NULL)
}

print_model_settings <- function(
  model,
  indent = "    ",
  model_label = "Outcome model"
) {
  print_setting_line(
    model_label,
    format_model_type(model$type),
    indent = indent
  )

  invisible(NULL)
}

print_staged_settings <- function(
  selection,
  construction,
  model,
  indent = "    ",
  headers = NULL,
  selection_labels = list(
    method = "Method",
    criterion = "Criterion",
    level = "Level"
  ),
  construction_labels = list(
    sign_mode = "Sign mode"
  ),
  model_label = "Outcome model"
) {
  if (!is.null(headers$selection)) {
    cat(headers$selection)
  }
  print_selection_settings(
    selection,
    indent = indent,
    method_label = selection_labels$method,
    criterion_label = selection_labels$criterion,
    level_label = selection_labels$level
  )
  if (!is.null(headers$construction)) {
    cat(headers$construction)
  }
  print_construction_settings(
    construction,
    indent = indent,
    sign_mode_label = construction_labels$sign_mode
  )
  if (!is.null(headers$model)) {
    cat(headers$model)
  }
  print_model_settings(
    model,
    indent = indent,
    model_label = model_label
  )

  invisible(NULL)
}
