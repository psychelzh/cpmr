edge_signs <- c("positive", "negative")
summary_column_names <- stats::setNames(
  paste0(edge_signs, "_summary"),
  edge_signs
)
summary_columns <- unname(summary_column_names)

build_construction_model <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = build_construction_model_summary(
      conmat = conmat,
      edge_selection = edge_selection,
      construction_spec = construction_spec
    )
  )
}

construction_stream_features <- function(
  construction_model,
  prediction_stream,
  conmat_new = NULL
) {
  switch(
    construction_model$type,
    summary = stream_features_summary(
      construction_model = construction_model,
      prediction_stream = prediction_stream,
      conmat = conmat_new
    )
  )
}

build_construction_model_summary <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  center <- NULL
  scale <- NULL
  if (construction_spec$standardize_edges) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  edge_weights <- edge_weights_summary(
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
    constructed_features = feature_matrix_summary(conmat, edge_weights)
  )
}

stream_features_summary <- function(
  construction_model,
  prediction_stream,
  conmat = NULL
) {
  summary_features <- construction_model$constructed_features
  if (!is.null(conmat)) {
    if (construction_model$construction$standardize_edges) {
      conmat <- fscale(
        conmat,
        construction_model$center,
        construction_model$scale
      )
    }

    summary_features <- feature_matrix_summary(
      conmat,
      construction_model$edge_weights
    )
  }

  prediction_stream <- validate_choice(
    prediction_stream,
    construction_model$construction$prediction_streams,
    arg = "`prediction_stream`"
  )

  if (construction_model$construction$polarity == "net") {
    return(matrix(
      summary_features[, "positive_summary"] -
        summary_features[, "negative_summary"],
      ncol = 1,
      dimnames = list(NULL, "net_summary")
    ))
  }

  if (prediction_stream == "joint") {
    return(summary_features)
  }

  summary_features[, summary_column_names[[prediction_stream]], drop = FALSE]
}

edge_weights_summary <- function(
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

feature_matrix_summary <- function(conmat, edge_weights) {
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

fscale <- function(conmat, center, scale) {
  Rfast::eachrow(Rfast::eachrow(conmat, center, "-"), scale, "/")
}
