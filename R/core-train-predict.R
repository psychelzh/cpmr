core_fscale <- function(x, center, scale) {
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}

core_train_model <- function(conmat, behav, edges, bias_correct) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- core_fscale(conmat, center, scale)
  }

  x <- matrix(
    1,
    nrow = dim(conmat)[1],
    ncol = length(edge_signs) + 1,
    dimnames = list(NULL, c("(Intercept)", edge_signs))
  )
  for (edge_sign in edge_signs) {
    x[, edge_sign] <- Rfast::rowsums(
      conmat[, edges[, edge_sign], drop = FALSE]
    )
  }

  models <- lapply(prediction_networks, function(network_name) {
    cur_x <- if (network_name == "both") {
      x
    } else {
      x[, c("(Intercept)", network_name)]
    }
    stats::.lm.fit(cur_x, behav)$coefficients
  })
  names(models) <- prediction_networks

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edges,
    models = models
  )
}

core_predict_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- core_fscale(conmat_new, model$center, model$scale)
  }

  x_new <- matrix(
    1,
    nrow = dim(conmat_new)[1],
    ncol = length(edge_signs) + 1,
    dimnames = list(NULL, c("(Intercept)", edge_signs))
  )
  for (edge_sign in edge_signs) {
    x_new[, edge_sign] <- Rfast::rowsums(
      conmat_new[, model$edges[, edge_sign], drop = FALSE]
    )
  }

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_networks),
    dimnames = list(NULL, prediction_networks)
  )
  for (network_name in prediction_networks) {
    cur_x_new <- if (network_name == "both") {
      x_new
    } else {
      x_new[, c("(Intercept)", network_name)]
    }
    pred[, network_name] <- cur_x_new %*% model$models[[network_name]]
  }

  pred
}
