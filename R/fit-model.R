run_edge_selection <- function(
  conmat,
  behav,
  selection_method = c("pearson", "spearman"),
  selection_criterion = c("p_value", "absolute", "proportion"),
  selection_level = 0.01
) {
  selection_method <- match.arg(selection_method)
  selection_criterion <- match.arg(selection_criterion)
  selection_level <- validate_selection_level(
    selection_level,
    criterion = selection_criterion,
    arg = "`selection_level`"
  )
  associations <- drop(stats::cor(
    conmat,
    behav,
    method = selection_method
  ))
  associations[!is.finite(associations)] <- NA_real_

  thresholds <- switch(
    selection_criterion,
    p_value = stats::setNames(
      rep(critical_r(nrow(conmat), selection_level), length(edge_signs)),
      edge_signs
    ),
    absolute = stats::setNames(
      rep(selection_level, length(edge_signs)),
      edge_signs
    ),
    proportion = select_sparsity_thresholds(
      associations = associations,
      proportion = selection_level
    ),
    stop(
      paste(
        "`selection_criterion` must be one of",
        "\"p_value\", \"absolute\", or \"proportion\"."
      )
    )
  )

  mask <- edge_mask_from_cutoffs(
    associations = associations,
    cutoffs = thresholds
  )

  list(
    associations = associations,
    thresholds = thresholds,
    mask = mask
  )
}

select_edge_mask <- function(
  conmat,
  behav,
  selection_method = c("pearson", "spearman"),
  selection_criterion = c("p_value", "absolute", "proportion"),
  selection_level = 0.01
) {
  run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_method = selection_method,
    selection_criterion = selection_criterion,
    selection_level = selection_level
  )$mask
}

train_model <- function(
  conmat,
  behav,
  edge_selection,
  construction_spec,
  model_spec
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
  network_summaries <- compute_network_summaries(conmat, edge_weights)
  prediction_streams <- prediction_streams_for_polarity(
    construction_spec$polarity
  )

  outcome_models <- lapply(prediction_streams, function(prediction_stream) {
    fit_stream_model(
      network_summaries = network_summaries,
      behav = behav,
      construction_polarity = construction_spec$polarity,
      prediction_stream = prediction_stream,
      model_spec = model_spec
    )
  })
  names(outcome_models) <- prediction_streams

  list(
    standardize_edges = construction_spec$standardize_edges,
    center = center,
    scale = scale,
    edges = edge_selection$mask,
    edge_weights = edge_weights,
    weight_scale = construction_spec$weight_scale,
    selection_thresholds = edge_selection$thresholds,
    construction_polarity = construction_spec$polarity,
    prediction_streams = prediction_streams,
    outcome_models = outcome_models
  )
}

predict_model <- function(model, conmat_new) {
  if (model$standardize_edges) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  network_summaries <- compute_network_summaries(conmat_new, model$edge_weights)
  prediction_streams <- model$prediction_streams

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_streams),
    dimnames = list(NULL, prediction_streams)
  )
  for (prediction_stream in prediction_streams) {
    pred[, prediction_stream] <- predict_stream_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      network_summaries = network_summaries,
      construction_polarity = model$construction_polarity,
      prediction_stream = prediction_stream
    )
  }

  pred
}

edge_mask_from_cutoffs <- function(associations, cutoffs) {
  matrix(
    c(
      !is.na(associations) & associations >= cutoffs[["positive"]],
      !is.na(associations) & (-associations) >= cutoffs[["negative"]]
    ),
    ncol = 2,
    dimnames = list(NULL, edge_signs)
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

  stats::setNames(c(positive_cutoff, negative_cutoff), edge_signs)
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

fit_stream_model <- function(
  network_summaries,
  behav,
  construction_polarity,
  prediction_stream,
  model_spec
) {
  features <- stream_features(
    network_summaries = network_summaries,
    construction_polarity = construction_polarity,
    prediction_stream = prediction_stream
  )

  fit_outcome_model(
    features = features,
    behav = behav,
    model_spec = model_spec
  )
}

predict_stream_model <- function(
  fitted_model,
  network_summaries,
  construction_polarity,
  prediction_stream
) {
  features <- stream_features(
    network_summaries = network_summaries,
    construction_polarity = construction_polarity,
    prediction_stream = prediction_stream
  )

  predict_outcome_model(
    fitted_model = fitted_model,
    features = features
  )
}

stream_features <- function(
  network_summaries,
  construction_polarity,
  prediction_stream
) {
  switch(
    construction_polarity,
    separate = separate_stream_features(
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
        "`construction_polarity` must be either \"separate\" or \"net\"."
      )
    )
  )
}

separate_stream_features <- function(network_summaries, prediction_stream) {
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
        "`construction_polarity = \"separate\"`."
      ),
      call. = FALSE
    )
  }

  network_summaries[, columns, drop = FALSE]
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

prediction_streams_for_polarity <- function(construction_polarity) {
  switch(
    construction_polarity,
    separate = c("joint", edge_signs),
    net = "net",
    stop(
      paste(
        "`construction_polarity` must be either \"separate\" or \"net\"."
      )
    )
  )
}

edge_signs <- c("positive", "negative")
summary_column_names <- stats::setNames(
  paste0(edge_signs, "_summary"),
  edge_signs
)
summary_columns <- unname(summary_column_names)

critical_r <- function(n, alpha) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha > 1
  ) {
    stop("`alpha` must be a single number in (0, 1].", call. = FALSE)
  }

  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

fscale <- function(x, center, scale) {
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}
