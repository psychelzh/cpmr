build_summary_construction_model <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  center <- NULL
  scale <- NULL
  if (construction_spec$standardize_edges) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- sweep(sweep(conmat, 2, center, "-"), 2, scale, "/")
  }

  edge_weights <- summary_edge_weights(
    associations = edge_selection$associations,
    cutoffs = edge_selection$thresholds,
    mask = edge_selection$mask,
    weight_scale = construction_spec$weight_scale
  )

  list(
    type = "summary",
    construction = construction_spec,
    center = center,
    scale = scale,
    edge_weights = edge_weights,
    prediction_streams = construction_spec$prediction_streams,
    constructed_features = summary_feature_matrix(conmat, edge_weights)
  )
}

summary_stream_spec <- function(polarity, prediction_stream = NULL) {
  stream_specs <- switch(
    polarity,
    separate = list(
      joint = list(type = "columns", columns = summary_columns),
      positive = list(type = "columns", columns = "positive_summary"),
      negative = list(type = "columns", columns = "negative_summary")
    ),
    net = list(
      net = list(
        type = "difference",
        positive = "positive_summary",
        negative = "negative_summary",
        output = "net_summary"
      )
    ),
    stop("`polarity` must be either \"separate\" or \"net\".", call. = FALSE)
  )

  if (is.null(prediction_stream)) {
    return(stream_specs)
  }

  stream_spec <- stream_specs[[prediction_stream]]
  if (is.null(stream_spec)) {
    valid <- paste(sprintf("\"%s\"", names(stream_specs)), collapse = ", ")
    stop(
      sprintf(
        "`prediction_stream` must be one of %s for `polarity = \"%s\"`.",
        valid,
        polarity
      ),
      call. = FALSE
    )
  }

  stream_spec
}

summary_stream_features <- function(
  construction_model,
  prediction_stream,
  conmat = NULL
) {
  summary_features <- construction_model$constructed_features
  if (!is.null(conmat)) {
    if (construction_model$construction$standardize_edges) {
      conmat <- sweep(
        sweep(conmat, 2, construction_model$center, "-"),
        2,
        construction_model$scale,
        "/"
      )
    }

    summary_features <- summary_feature_matrix(
      conmat,
      construction_model$edge_weights
    )
  }

  stream_spec <- summary_stream_spec(
    polarity = construction_model$construction$polarity,
    prediction_stream = prediction_stream
  )

  switch(
    stream_spec$type,
    columns = summary_features[, stream_spec$columns, drop = FALSE],
    difference = matrix(
      summary_features[, stream_spec$positive] -
        summary_features[, stream_spec$negative],
      ncol = 1,
      dimnames = list(NULL, stream_spec$output)
    )
  )
}

summary_edge_weights <- function(
  associations,
  cutoffs,
  mask,
  weight_scale = 0
) {
  weight_scale <- validate_weight_scale(weight_scale)

  if (weight_scale == 0) {
    return(matrix(
      as.numeric(mask),
      ncol = length(edge_signs),
      dimnames = list(NULL, edge_signs)
    ))
  }

  sigmoid_weights <- function(scores, cutoff) {
    if (!is.finite(cutoff)) {
      return(rep(0, length(scores)))
    }

    weights <- rep(0, length(scores))
    valid <- !is.na(scores) & scores > 0
    weights[valid] <- stats::plogis((scores[valid] - cutoff) / weight_scale)
    weights
  }

  cbind(
    positive = sigmoid_weights(associations, cutoffs[["positive"]]),
    negative = sigmoid_weights(-associations, cutoffs[["negative"]])
  )
}

summary_feature_matrix <- function(conmat, edge_weights) {
  summaries <- matrix(
    0,
    nrow = nrow(conmat),
    ncol = length(summary_columns),
    dimnames = list(NULL, summary_columns)
  )

  for (edge_sign in edge_signs) {
    weights <- edge_weights[, edge_sign]
    idx <- which(weights != 0)
    if (!length(idx)) {
      next
    }

    summaries[, summary_column_names[[edge_sign]]] <- drop(
      conmat[, idx, drop = FALSE] %*% weights[idx]
    )
  }

  summaries
}
