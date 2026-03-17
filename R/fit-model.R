screen_edges <- function(
  conmat,
  behav,
  association_method,
  threshold_method,
  threshold_level,
  edge_weighting = c("binary", "sigmoid"),
  weighting_scale = 0.05
) {
  edge_weighting <- match.arg(edge_weighting)
  associations <- drop(stats::cor(
    conmat,
    behav,
    method = association_method
  ))
  associations[!is.finite(associations)] <- NA_real_

  thresholds <- switch(
    threshold_method,
    alpha = setNames(
      rep(critical_r(nrow(conmat), threshold_level), length(edge_types)),
      edge_types
    ),
    effect_size = setNames(
      rep(threshold_level, length(edge_types)),
      edge_types
    ),
    sparsity = select_sparsity_thresholds(
      associations = associations,
      proportion = threshold_level
    ),
    stop(
      paste(
        "`threshold_method` must be one of",
        "\"alpha\", \"sparsity\", or \"effect_size\"."
      )
    )
  )

  mask <- threshold_edges_by_cutoffs(
    associations = associations,
    cutoffs = thresholds
  )

  list(
    associations = associations,
    thresholds = thresholds,
    mask = mask,
    weights = compute_edge_weights(
      associations = associations,
      cutoffs = thresholds,
      mask = mask,
      edge_weighting = edge_weighting,
      weighting_scale = weighting_scale
    ),
    edge_weighting = edge_weighting,
    weighting_scale = weighting_scale
  )
}

select_edges <- function(
  conmat,
  behav,
  association_method,
  threshold_method,
  threshold_level
) {
  screen_edges(
    conmat = conmat,
    behav = behav,
    association_method = association_method,
    threshold_method = threshold_method,
    threshold_level = threshold_level
  )$mask
}

train_model <- function(
  conmat,
  behav,
  edge_screen,
  bias_correct,
  network_summary,
  prediction_head
) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  network_strengths <- compute_network_strengths(conmat, edge_screen$weights)
  prediction_types <- prediction_types_for_summary(network_summary)

  models <- lapply(prediction_types, function(prediction_type) {
    fit_prediction_head(
      network_strengths = network_strengths,
      behav = behav,
      network_summary = network_summary,
      prediction_type = prediction_type,
      prediction_head = prediction_head
    )
  })
  names(models) <- prediction_types

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edge_screen$mask,
    edge_weights = edge_screen$weights,
    edge_weighting = edge_screen$edge_weighting,
    weighting_scale = edge_screen$weighting_scale,
    edge_thresholds = edge_screen$thresholds,
    network_summary = network_summary,
    prediction_head = prediction_head,
    models = models
  )
}

predict_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  network_strengths <- compute_network_strengths(conmat_new, model$edge_weights)
  prediction_types <- names(model$models)

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_types),
    dimnames = list(NULL, prediction_types)
  )
  for (prediction_type in prediction_types) {
    pred[, prediction_type] <- predict_prediction_head(
      fitted_head = model$models[[prediction_type]],
      network_strengths = network_strengths,
      network_summary = model$network_summary,
      prediction_type = prediction_type,
      prediction_head = model$prediction_head
    )
  }

  pred
}

threshold_edges_by_cutoffs <- function(associations, cutoffs) {
  matrix(
    c(
      !is.na(associations) & associations >= cutoffs[["positive"]],
      !is.na(associations) & (-associations) >= cutoffs[["negative"]]
    ),
    ncol = 2,
    dimnames = list(NULL, edge_types)
  )
}

select_sparsity_thresholds <- function(associations, proportion) {
  k <- min(round(proportion * length(associations)), length(associations))
  positive_cutoff <- Inf
  negative_cutoff <- Inf

  if (k > 0L) {
    pos_idx <- which(!is.na(associations) & associations > 0)
    neg_idx <- which(!is.na(associations) & associations < 0)

    if (length(pos_idx)) {
      pos_order <- order(associations[pos_idx], decreasing = TRUE)
      positive_cutoff <- associations[pos_idx[pos_order][[min(k, length(pos_idx))]]]
    }
    if (length(neg_idx)) {
      neg_strength <- -associations[neg_idx]
      neg_order <- order(neg_strength, decreasing = TRUE)
      negative_cutoff <- neg_strength[neg_order[[min(k, length(neg_idx))]]]
    }
  }

  if (k > 0L && (is.infinite(positive_cutoff) || is.infinite(negative_cutoff))) {
    warning(
      paste(
        "The requested sparsity level did not retain both positive and",
        "negative edges."
      )
    ) # nocov
  }

  setNames(c(positive_cutoff, negative_cutoff), edge_types)
}

compute_edge_weights <- function(
  associations,
  cutoffs,
  mask,
  edge_weighting,
  weighting_scale
) {
  switch(
    edge_weighting,
    binary = matrix(
      as.numeric(mask),
      ncol = length(edge_types),
      dimnames = list(NULL, edge_types)
    ),
    sigmoid = cbind(
      positive = smooth_threshold_weights(
        scores = associations,
        cutoff = cutoffs[["positive"]],
        scale = weighting_scale
      ),
      negative = smooth_threshold_weights(
        scores = -associations,
        cutoff = cutoffs[["negative"]],
        scale = weighting_scale
      )
    ),
    stop("`edge_weighting` must be either \"binary\" or \"sigmoid\".")
  )
}

smooth_threshold_weights <- function(scores, cutoff, scale) {
  if (!is.finite(cutoff)) {
    return(rep(0, length(scores)))
  }

  weights <- rep(0, length(scores))
  valid <- !is.na(scores) & scores > 0
  weights[valid] <- stats::plogis((scores[valid] - cutoff) / scale)
  weights
}

compute_network_strengths <- function(conmat, edge_weights) {
  strengths <- matrix(
    0,
    nrow = nrow(conmat),
    ncol = length(edge_types),
    dimnames = list(NULL, edge_types)
  )

  for (edge_type in edge_types) {
    strengths[, edge_type] <- weighted_row_sums_or_zero(
      conmat = conmat,
      weights = edge_weights[, edge_type]
    )
  }

  strengths
}

weighted_row_sums_or_zero <- function(conmat, weights) {
  idx <- which(weights != 0)
  if (!length(idx)) {
    return(rep(0, nrow(conmat)))
  }

  drop(conmat[, idx, drop = FALSE] %*% weights[idx])
}

fit_prediction_head <- function(
  network_strengths,
  behav,
  network_summary,
  prediction_type,
  prediction_head
) {
  features <- prediction_features(
    network_strengths = network_strengths,
    network_summary = network_summary,
    prediction_type = prediction_type
  )
  design <- prediction_design_matrix(
    features = features,
    prediction_head = prediction_head
  )

  if (
    identical(prediction_head, "linear_no_intercept") &&
      all(design == 0)
  ) {
    coefficients <- stats::setNames(
      rep(0, ncol(design)),
      colnames(design)
    )
  } else {
    coefficients <- stats::.lm.fit(design, behav)$coefficients
    coefficients <- stats::setNames(coefficients, colnames(design))
  }

  list(coefficients = coefficients)
}

predict_prediction_head <- function(
  fitted_head,
  network_strengths,
  network_summary,
  prediction_type,
  prediction_head
) {
  features <- prediction_features(
    network_strengths = network_strengths,
    network_summary = network_summary,
    prediction_type = prediction_type
  )
  design <- prediction_design_matrix(
    features = features,
    prediction_head = prediction_head
  )

  drop(design %*% fitted_head$coefficients)
}

prediction_features <- function(
  network_strengths,
  network_summary,
  prediction_type
) {
  switch(
    network_summary,
    separate = switch(
      prediction_type,
      combined = network_strengths[, edge_types, drop = FALSE],
      positive = network_strengths[, "positive", drop = FALSE],
      negative = network_strengths[, "negative", drop = FALSE],
      stop(
        paste0(
          "`prediction_type` must be one of ",
          "\"combined\", \"positive\", or \"negative\" for ",
          "`network_summary = \"separate\"`."
        )
      )
    ),
    difference = matrix(
      network_strengths[, "positive"] - network_strengths[, "negative"],
      ncol = 1,
      dimnames = list(NULL, "difference")
    ),
    stop(
      paste(
        "`network_summary` must be either \"separate\" or \"difference\"."
      )
    )
  )
}

prediction_design_matrix <- function(features, prediction_head) {
  features <- as.matrix(features)

  switch(
    prediction_head,
    linear = cbind("(Intercept)" = 1, features),
    linear_no_intercept = features,
    stop(
      paste(
        "`prediction_head` must be either \"linear\" or",
        "\"linear_no_intercept\"."
      )
    )
  )
}

prediction_types_for_summary <- function(network_summary) {
  switch(
    network_summary,
    separate = c("combined", edge_types),
    difference = "difference",
    stop(
      paste(
        "`network_summary` must be either \"separate\" or \"difference\"."
      )
    )
  )
}

edge_types <- c("positive", "negative")

critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

fscale <- function(x, center, scale) {
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}
