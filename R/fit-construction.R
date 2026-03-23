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
    summary = feature_sets_summary(
      construction_state = construction_state,
      conmat = conmat_new
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

  edge_mask <- edge_selection$mask
  edge_weights <- if (construction_spec$weight_scale == 0) {
    NULL
  } else {
    edge_weights_summary_impl(
      associations = edge_selection$associations,
      cutoffs = edge_selection$thresholds,
      mask = edge_mask,
      weight_scale = construction_spec$weight_scale
    )
  }

  list(
    type = "summary",
    construction = construction_spec,
    center = center,
    scale = scale,
    edge_mask = edge_mask,
    edge_weights = edge_weights,
    constructed_features = summary_matrix_from_state(
      list(
        construction = construction_spec,
        center = center,
        scale = scale,
        edge_mask = edge_mask,
        edge_weights = edge_weights
      ),
      conmat = conmat
    )
  )
}

feature_sets_summary <- function(construction_state, conmat = NULL) {
  summaries <- summary_matrix_from_state(construction_state, conmat)

  if (construction_state$construction$sign_mode == "net") {
    return(list(
      net = matrix(
        summaries[, "positive_summary"] -
          summaries[, "negative_summary"],
        ncol = 1,
        dimnames = list(NULL, "net_summary")
      )
    ))
  }

  list(
    joint = summaries,
    positive = summaries[, "positive_summary", drop = FALSE],
    negative = summaries[, "negative_summary", drop = FALSE]
  )
}

summary_matrix_from_state <- function(construction_state, conmat = NULL) {
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
    conmat = conmat,
    edge_mask = construction_state$edge_mask,
    edge_weights = construction_state$edge_weights,
    weight_scale = construction_state$construction$weight_scale
  )
}

edge_weights_summary <- function(
  associations,
  cutoffs,
  mask,
  weight_scale = 0
) {
  weight_scale <- normalize_weight_scale(weight_scale)

  edge_weights_summary_impl(
    associations = associations,
    cutoffs = cutoffs,
    mask = mask,
    weight_scale = weight_scale
  )
}

edge_weights_summary_impl <- function(
  associations,
  cutoffs,
  mask,
  weight_scale
) {
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

feature_matrix_summary <- function(
  conmat,
  edge_mask,
  edge_weights = NULL,
  weight_scale
) {
  if (weight_scale == 0) {
    return(feature_matrix_summary_binary(conmat, edge_mask))
  }

  feature_matrix_summary_weighted(conmat, edge_weights)
}

feature_matrix_summary_binary <- function(conmat, edge_mask) {
  summaries <- matrix(
    0,
    nrow = nrow(conmat),
    ncol = length(summary_columns),
    dimnames = list(NULL, summary_columns)
  )

  for (edge_sign in edge_signs) {
    idx <- which(edge_mask[, edge_sign])
    if (!length(idx)) {
      next
    }

    summaries[, summary_column_names[[edge_sign]]] <- Rfast::rowsums(
      conmat[, idx, drop = FALSE]
    )
  }

  summaries
}

feature_matrix_summary_weighted <- function(conmat, edge_weights) {
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
