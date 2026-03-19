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
    alpha = stats::setNames(
      rep(critical_r(nrow(conmat), threshold_level), length(edge_types)),
      edge_types
    ),
    effect_size = stats::setNames(
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
  feature_space,
  model_spec
) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  network_strengths <- compute_network_strengths(conmat, edge_screen$weights)
  prediction_streams <- prediction_streams_for_feature_space(feature_space)

  outcome_models <- lapply(prediction_streams, function(prediction_stream) {
    fit_prediction_model(
      network_strengths = network_strengths,
      behav = behav,
      feature_space = feature_space,
      prediction_stream = prediction_stream,
      model_spec = model_spec
    )
  })
  names(outcome_models) <- prediction_streams

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edge_screen$mask,
    edge_weights = edge_screen$weights,
    edge_weighting = edge_screen$edge_weighting,
    weighting_scale = edge_screen$weighting_scale,
    edge_thresholds = edge_screen$thresholds,
    feature_space = feature_space,
    model_spec = model_spec,
    prediction_streams = prediction_streams,
    outcome_models = outcome_models
  )
}

predict_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  network_strengths <- compute_network_strengths(conmat_new, model$edge_weights)
  prediction_streams <- model$prediction_streams

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_streams),
    dimnames = list(NULL, prediction_streams)
  )
  for (prediction_stream in prediction_streams) {
    pred[, prediction_stream] <- predict_prediction_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      network_strengths = network_strengths,
      feature_space = model$feature_space,
      prediction_stream = prediction_stream
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
  positive_cutoff <- Inf
  negative_cutoff <- Inf

  pos_idx <- which(!is.na(associations) & associations > 0)
  neg_idx <- which(!is.na(associations) & associations < 0)

  if (length(pos_idx)) {
    positive_cutoff <- sign_sparsity_cutoff(
      associations[pos_idx],
      proportion = proportion
    )
  }
  if (length(neg_idx)) {
    negative_cutoff <- sign_sparsity_cutoff(
      -associations[neg_idx],
      proportion = proportion
    )
  }

  if (
    proportion > 0L &&
      (is.infinite(positive_cutoff) || is.infinite(negative_cutoff))
  ) {
    warning(
      paste(
        "The requested sparsity level did not retain both positive and",
        "negative edges."
      )
    ) # nocov
  }

  stats::setNames(c(positive_cutoff, negative_cutoff), edge_types)
}

sign_sparsity_cutoff <- function(scores, proportion) {
  if (!length(scores) || proportion <= 0) {
    return(Inf)
  }

  k <- min(ceiling(proportion * length(scores)), length(scores))
  scores[order(scores, decreasing = TRUE)[[k]]]
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

fit_prediction_model <- function(
  network_strengths,
  behav,
  feature_space,
  prediction_stream,
  model_spec
) {
  features <- prediction_features(
    network_strengths = network_strengths,
    feature_space = feature_space,
    prediction_stream = prediction_stream
  )

  fit_outcome_model(
    features = features,
    behav = behav,
    model_spec = model_spec
  )
}

predict_prediction_model <- function(
  fitted_model,
  network_strengths,
  feature_space,
  prediction_stream
) {
  features <- prediction_features(
    network_strengths = network_strengths,
    feature_space = feature_space,
    prediction_stream = prediction_stream
  )

  predict_outcome_model(
    fitted_model = fitted_model,
    features = features
  )
}

prediction_features <- function(
  network_strengths,
  feature_space,
  prediction_stream
) {
  switch(
    feature_space,
    separate = separate_prediction_features(
      network_strengths = network_strengths,
      prediction_stream = prediction_stream
    ),
    net = matrix(
      network_strengths[, "positive"] - network_strengths[, "negative"],
      ncol = 1,
      dimnames = list(NULL, "net_strength")
    ),
    stop(
      paste(
        "`feature_space` must be either \"separate\" or \"net\"."
      )
    )
  )
}

separate_prediction_features <- function(network_strengths, prediction_stream) {
  feature_sets <- list(
    joint = edge_types,
    positive = "positive",
    negative = "negative"
  )
  columns <- feature_sets[[prediction_stream]]

  if (is.null(columns)) {
    stop(
      paste0(
        "`prediction_stream` must be one of ",
        "\"joint\", \"positive\", or \"negative\" for ",
        "`feature_space = \"separate\"`."
      ),
      call. = FALSE
    )
  }

  network_strengths[, columns, drop = FALSE]
}

fit_outcome_model <- function(features, behav, model_spec) {
  switch(
    model_spec$type,
    lm = fit_lm_outcome_model(features, behav),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

predict_outcome_model <- function(fitted_model, features) {
  switch(
    fitted_model$type,
    lm = predict_lm_outcome_model(fitted_model, features),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

lm_design_matrix <- function(features) {
  features <- as.matrix(features)
  cbind("(Intercept)" = 1, features)
}

fit_lm_outcome_model <- function(features, behav) {
  design <- lm_design_matrix(features)
  fit <- stats::lm.fit(design, behav)
  coefficients <- fit$coefficients
  prediction_coefficients <- coefficients
  prediction_coefficients[is.na(prediction_coefficients)] <- 0

  list(
    type = "lm",
    feature_names = colnames(features),
    coefficients = coefficients,
    prediction_coefficients = prediction_coefficients,
    rank = fit$rank
  )
}

predict_lm_outcome_model <- function(fitted_model, features) {
  design <- lm_design_matrix(features)
  drop(design %*% fitted_model$prediction_coefficients)
}

prediction_streams_for_feature_space <- function(feature_space) {
  switch(
    feature_space,
    separate = c("joint", edge_types),
    net = "net",
    stop(
      paste(
        "`feature_space` must be either \"separate\" or \"net\"."
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
