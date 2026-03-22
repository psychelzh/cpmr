build_construction_model <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = build_summary_construction_model(
      conmat = conmat,
      edge_selection = edge_selection,
      construction_spec = construction_spec
    )
  )
}

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
    conmat <- fscale(conmat, center, scale)
  }

  edge_weights <- compute_edge_weights(
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
    prediction_streams = summary_prediction_streams(construction_spec$polarity),
    constructed_features = compute_network_summaries(conmat, edge_weights)
  )
}

construction_stream_features <- function(
  construction_model,
  prediction_stream,
  conmat_new = NULL
) {
  switch(
    construction_model$type,
    summary = summary_construction_stream_features(
      construction_model = construction_model,
      prediction_stream = prediction_stream,
      conmat_new = conmat_new
    )
  )
}

summary_construction_stream_features <- function(
  construction_model,
  prediction_stream,
  conmat_new = NULL
) {
  network_summaries <- if (is.null(conmat_new)) {
    construction_model$constructed_features
  } else {
    summary_construction_features(
      construction_model = construction_model,
      conmat_new = conmat_new
    )
  }

  summary_stream_features(
    network_summaries = network_summaries,
    polarity = construction_model$construction$polarity,
    prediction_stream = prediction_stream
  )
}

summary_construction_features <- function(construction_model, conmat_new) {
  if (construction_model$construction$standardize_edges) {
    conmat_new <- fscale(
      conmat_new,
      construction_model$center,
      construction_model$scale
    )
  }

  compute_network_summaries(conmat_new, construction_model$edge_weights)
}

summary_stream_features <- function(
  network_summaries,
  polarity,
  prediction_stream
) {
  switch(
    polarity,
    separate = summary_separate_stream_features(
      network_summaries = network_summaries,
      prediction_stream = prediction_stream
    ),
    net = matrix(
      network_summaries[, "positive_summary"] -
        network_summaries[, "negative_summary"],
      ncol = 1,
      dimnames = list(NULL, "net_summary")
    ),
    stop(
      paste(
        "`polarity` must be either \"separate\" or \"net\"."
      )
    )
  )
}

summary_separate_stream_features <- function(
  network_summaries,
  prediction_stream
) {
  feature_sets <- list(
    joint = summary_columns,
    positive = "positive_summary",
    negative = "negative_summary"
  )
  columns <- feature_sets[[prediction_stream]]

  if (is.null(columns)) {
    stop(
      paste0(
        "`prediction_stream` must be one of ",
        "\"joint\", \"positive\", or \"negative\" for ",
        "`polarity = \"separate\"`."
      ),
      call. = FALSE
    )
  }

  network_summaries[, columns, drop = FALSE]
}

summary_prediction_streams <- function(polarity) {
  switch(
    polarity,
    separate = c("joint", edge_signs),
    net = "net",
    stop(
      paste(
        "`polarity` must be either \"separate\" or \"net\"."
      )
    )
  )
}

construction_prediction_streams <- function(construction_spec) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = summary_prediction_streams(construction_spec$polarity)
  )
}

compute_edge_weights <- function(
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

  cbind(
    positive = sigmoid_edge_weights(
      scores = associations,
      cutoff = cutoffs[["positive"]],
      scale = weight_scale
    ),
    negative = sigmoid_edge_weights(
      scores = -associations,
      cutoff = cutoffs[["negative"]],
      scale = weight_scale
    )
  )
}

sigmoid_edge_weights <- function(scores, cutoff, scale) {
  if (!is.finite(cutoff)) {
    return(rep(0, length(scores)))
  }

  weights <- rep(0, length(scores))
  valid <- !is.na(scores) & scores > 0
  weights[valid] <- stats::plogis((scores[valid] - cutoff) / scale)
  weights
}

compute_network_summaries <- function(conmat, edge_weights) {
  summaries <- matrix(
    0,
    nrow = nrow(conmat),
    ncol = length(summary_columns),
    dimnames = list(NULL, summary_columns)
  )

  for (edge_sign in edge_signs) {
    summaries[, summary_column_names[[edge_sign]]] <- weighted_row_sums_or_zero(
      conmat = conmat,
      weights = edge_weights[, edge_sign]
    )
  }

  summaries
}

weighted_row_sums_or_zero <- function(conmat, weights) {
  idx <- which(weights != 0)
  if (!length(idx)) {
    return(rep(0, nrow(conmat)))
  }

  drop(conmat[, idx, drop = FALSE] %*% weights[idx])
}

fscale <- function(x, center, scale) {
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}
