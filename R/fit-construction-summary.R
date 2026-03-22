summary_construction_model <- function(
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
    prediction_streams = summary_prediction_streams(construction_spec$polarity),
    constructed_features = summary_feature_matrix(conmat, edge_weights)
  )
}

summary_construction_features <- function(
  construction_model,
  conmat = NULL
) {
  if (is.null(conmat)) {
    return(construction_model$constructed_features)
  }

  if (construction_model$construction$standardize_edges) {
    conmat <- sweep(
      sweep(conmat, 2, construction_model$center, "-"),
      2,
      construction_model$scale,
      "/"
    )
  }

  summary_feature_matrix(conmat, construction_model$edge_weights)
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

summary_stream_features <- function(
  summary_features,
  polarity,
  prediction_stream
) {
  switch(
    polarity,
    separate = {
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

      summary_features[, columns, drop = FALSE]
    },
    net = {
      if (!identical(prediction_stream, "net")) {
        stop(
          "`prediction_stream` must be \"net\" for `polarity = \"net\"`.",
          call. = FALSE
        )
      }

      matrix(
        summary_features[, "positive_summary"] -
          summary_features[, "negative_summary"],
        ncol = 1,
        dimnames = list(NULL, "net_summary")
      )
    },
    stop(
      paste(
        "`polarity` must be either \"separate\" or \"net\"."
      )
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
