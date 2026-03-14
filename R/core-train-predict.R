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
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x[, corr_type] <- Rfast::rowsums(
      conmat[, edges[, corr_type], drop = FALSE]
    )
  }

  models <- lapply(inc_edges, function(inc_edge) {
    cur_x <- if (inc_edge == "both") {
      x
    } else {
      x[, c("(Intercept)", inc_edge)]
    }
    stats::.lm.fit(cur_x, behav)$coefficients
  })
  names(models) <- inc_edges

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
    ncol = length(corr_types) + 1,
    dimnames = list(NULL, c("(Intercept)", corr_types))
  )
  for (corr_type in corr_types) {
    x_new[, corr_type] <- Rfast::rowsums(
      conmat_new[, model$edges[, corr_type], drop = FALSE]
    )
  }

  pred <- matrix(
    nrow = dim(conmat_new)[1],
    ncol = length(inc_edges),
    dimnames = list(NULL, inc_edges)
  )
  for (inc_edge in inc_edges) {
    cur_x_new <- if (inc_edge == "both") {
      x_new
    } else {
      x_new[, c("(Intercept)", inc_edge)]
    }
    pred[, inc_edge] <- cur_x_new %*% model$models[[inc_edge]]
  }

  pred
}
