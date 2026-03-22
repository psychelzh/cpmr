edge_signs <- c("positive", "negative")
summary_column_names <- stats::setNames(
  paste0(edge_signs, "_summary"),
  edge_signs
)
summary_columns <- unname(summary_column_names)

build_construction_state <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  if (is.null(construction_spec$prediction_streams)) {
    construction_spec <- validate_construction_spec(construction_spec)
  }

  switch(
    construction_spec$type,
    summary = build_construction_state_summary(
      conmat = conmat,
      edge_selection = edge_selection,
      construction_spec = construction_spec
    )
  )
}

construction_features <- function(
  construction_state,
  conmat_new = NULL
) {
  switch(
    construction_state$type,
    summary = features_summary(
      construction_state = construction_state,
      conmat = conmat_new
    )
  )
}

construction_stream_features <- function(
  construction_state,
  prediction_stream,
  conmat_new = NULL,
  features = NULL
) {
  if (is.null(features)) {
    features <- construction_features(
      construction_state = construction_state,
      conmat_new = conmat_new
    )
  }

  switch(
    construction_state$type,
    summary = stream_features_summary(
      construction_state = construction_state,
      prediction_stream = prediction_stream,
      features = features
    )
  )
}

build_construction_state_summary <- function(
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
  construction_state,
  prediction_stream,
  conmat = NULL,
  features = NULL
) {
  if (is.null(features)) {
    features <- features_summary(
      construction_state = construction_state,
      conmat = conmat
    )
  }

  prediction_stream <- validate_choice(
    prediction_stream,
    construction_state$construction$prediction_streams,
    arg = "`prediction_stream`"
  )

  if (construction_state$construction$polarity == "net") {
    return(matrix(
      features[, "positive_summary"] -
        features[, "negative_summary"],
      ncol = 1,
      dimnames = list(NULL, "net_summary")
    ))
  }

  if (prediction_stream == "joint") {
    return(features)
  }

  features[, summary_column_names[[prediction_stream]], drop = FALSE]
}

features_summary <- function(construction_state, conmat = NULL) {
  if (is.null(conmat)) {
    return(construction_state$constructed_features)
  }

  if (construction_state$construction$standardize_edges) {
    conmat <- fscale(
      conmat,
      construction_state$center,
      construction_state$scale
    )
  }

  feature_matrix_summary(
    conmat,
    construction_state$edge_weights
  )
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

    active_weights <- weights[idx]
    summaries[, summary_column_names[[edge_sign]]] <- if (
      all(active_weights == 1)
    ) {
      Rfast::rowsums(conmat[, idx, drop = FALSE])
    } else {
      drop(conmat[, idx, drop = FALSE] %*% active_weights)
    }
  }

  summaries
}

fscale <- function(conmat, center, scale) {
  Rfast::eachrow(Rfast::eachrow(conmat, center, "-"), scale, "/")
}
