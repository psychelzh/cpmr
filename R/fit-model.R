run_edge_selection <- function(
  conmat,
  behav,
  selection_spec
) {
  selection_spec <- validate_selection_spec(selection_spec)

  switch(
    selection_spec$type,
    cor = run_correlation_edge_selection(
      conmat = conmat,
      behav = behav,
      method = selection_spec$method,
      criterion = selection_spec$criterion,
      level = selection_spec$level
    )
  )
}

run_correlation_edge_selection <- function(
  conmat,
  behav,
  method = c("pearson", "spearman"),
  criterion = c("p_value", "absolute", "proportion"),
  level = 0.01
) {
  method <- match.arg(method)
  criterion <- match.arg(criterion)
  level <- validate_selection_level(
    level,
    criterion = criterion,
    arg = "`level`"
  )
  associations <- drop(stats::cor(
    conmat,
    behav,
    method = method
  ))
  associations[!is.finite(associations)] <- NA_real_

  thresholds <- switch(
    criterion,
    p_value = stats::setNames(
      rep(critical_r(nrow(conmat), level), length(edge_signs)),
      edge_signs
    ),
    absolute = stats::setNames(
      rep(level, length(edge_signs)),
      edge_signs
    ),
    proportion = select_sparsity_thresholds(
      associations = associations,
      proportion = level
    ),
    stop(
      paste(
        "`criterion` must be one of",
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
  method = c("pearson", "spearman"),
  criterion = c("p_value", "absolute", "proportion"),
  level = 0.01
) {
  run_correlation_edge_selection(
    conmat = conmat,
    behav = behav,
    method = method,
    criterion = criterion,
    level = level
  )$mask
}

train_model <- function(
  conmat,
  behav,
  edge_selection,
  construction_spec,
  model_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)
  model_spec <- validate_model_spec(model_spec)
  construction_model <- build_construction_model(
    conmat = conmat,
    edge_selection = edge_selection,
    construction_spec = construction_spec
  )

  outcome_models <- lapply(
    construction_model$prediction_streams,
    function(
      prediction_stream
    ) {
      fit_stream_model(
        construction_model = construction_model,
        behav = behav,
        prediction_stream = prediction_stream,
        model_spec = model_spec
      )
    }
  )
  names(outcome_models) <- construction_model$prediction_streams

  utils::modifyList(
    construction_model,
    list(
      edges = edge_selection$mask,
      selection_thresholds = edge_selection$thresholds,
      outcome_models = outcome_models
    )
  )
}

predict_model <- function(model, conmat_new) {
  prediction_streams <- model$prediction_streams

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_streams),
    dimnames = list(NULL, prediction_streams)
  )
  for (prediction_stream in prediction_streams) {
    pred[, prediction_stream] <- predict_stream_model(
      fitted_model = model$outcome_models[[prediction_stream]],
      construction_model = model,
      conmat_new = conmat_new,
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
  construction_model,
  behav,
  prediction_stream,
  model_spec
) {
  features <- construction_stream_features(
    construction_model = construction_model,
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
  construction_model,
  conmat_new,
  prediction_stream
) {
  features <- construction_stream_features(
    construction_model = construction_model,
    conmat_new = conmat_new,
    prediction_stream = prediction_stream
  )

  predict_outcome_model(
    fitted_model = fitted_model,
    features = features
  )
}

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
    prediction_streams = prediction_streams_for_polarity(
      construction_spec$polarity
    ),
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

  stream_features(
    network_summaries = network_summaries,
    construction_polarity = construction_model$construction$polarity,
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

construction_prediction_streams <- function(construction_spec) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = prediction_streams_for_polarity(construction_spec$polarity)
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
