select_edges <- function(conmat, behav, method, level) {
  r_mat <- stats::cor(conmat, behav)
  r_crit <- switch(
    method,
    alpha = {
      thresh <- critical_r(nrow(conmat), level)
      c(-thresh, thresh)
    },
    sparsity = {
      k <- round(level * length(r_mat))
      thresh <- c(
        Rfast::nth(r_mat, k),
        Rfast::nth(r_mat, k, descending = TRUE)
      )
      if (thresh[[1]] > 0 || thresh[[2]] < 0) {
        warning("Not enough positive or negative correlation values.") # nocov
      }
      thresh
    },
    stop("Invalid threshold method.")
  )
  matrix(
    c(r_mat >= r_crit[2], r_mat <= r_crit[1]),
    ncol = 2,
    dimnames = list(NULL, edge_types)
  )
}

train_model <- function(conmat, behav, edges, bias_correct) {
  center <- NULL
  scale <- NULL
  if (bias_correct) {
    center <- Rfast::colmeans(conmat)
    scale <- Rfast::colVars(conmat, std = TRUE)
    conmat <- fscale(conmat, center, scale)
  }

  x <- matrix(
    1,
    nrow = dim(conmat)[1],
    ncol = length(edge_types) + 1,
    dimnames = list(NULL, c("(Intercept)", edge_types))
  )
  for (edge_type in edge_types) {
    x[, edge_type] <- Rfast::rowsums(
      conmat[, edges[, edge_type], drop = FALSE]
    )
  }

  models <- lapply(prediction_types, function(prediction_type) {
    cur_x <- if (prediction_type == "both") {
      x
    } else {
      x[, c("(Intercept)", prediction_type)]
    }
    stats::.lm.fit(cur_x, behav)$coefficients
  })
  names(models) <- prediction_types

  list(
    bias_correct = bias_correct,
    center = center,
    scale = scale,
    edges = edges,
    models = models
  )
}

predict_model <- function(model, conmat_new) {
  if (model$bias_correct) {
    conmat_new <- fscale(conmat_new, model$center, model$scale)
  }

  x_new <- matrix(
    1,
    nrow = dim(conmat_new)[1],
    ncol = length(edge_types) + 1,
    dimnames = list(NULL, c("(Intercept)", edge_types))
  )
  for (edge_type in edge_types) {
    x_new[, edge_type] <- Rfast::rowsums(
      conmat_new[, model$edges[, edge_type], drop = FALSE]
    )
  }

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(prediction_types),
    dimnames = list(NULL, prediction_types)
  )
  for (prediction_type in prediction_types) {
    cur_x_new <- if (prediction_type == "both") {
      x_new
    } else {
      x_new[, c("(Intercept)", prediction_type)]
    }
    pred[, prediction_type] <- cur_x_new %*% model$models[[prediction_type]]
  }

  pred
}

edge_types <- c("pos", "neg")
prediction_types <- c("both", edge_types)

critical_r <- function(n, alpha) {
  df <- n - 2
  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}

fscale <- function(x, center, scale) {
  Rfast::eachrow(Rfast::eachrow(x, center, "-"), scale, "/")
}
