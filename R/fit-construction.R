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
    summary = {
      center <- NULL
      scale <- NULL
      if (construction_spec$standardize_edges) {
        center <- Rfast::colmeans(conmat)
        scale <- Rfast::colVars(conmat, std = TRUE)
        conmat <- fscale(conmat, center, scale)
      }

      edge_mask <- edge_selection$mask
      weight_scale <- construction_spec$weight_scale
      edge_weights <- NULL
      if (weight_scale != 0) {
        edge_weights <- edge_weights_summary(
          associations = edge_selection$associations,
          cutoffs = edge_selection$thresholds,
          mask = edge_mask,
          weight_scale = weight_scale
        )
      }

      list(
        construction = construction_spec,
        center = center,
        scale = scale,
        edge_mask = edge_mask,
        edge_weights = edge_weights,
        summaries = feature_matrix_summary(
          conmat = conmat,
          edge_mask = edge_mask,
          edge_weights = edge_weights
        )
      )
    },
    stop(
      "`construction` must be a supported CPM construction spec.",
      call. = FALSE
    )
  )
}

construction_features <- function(construction_state) {
  switch(
    construction_state$construction$type,
    summary = {
      summaries <- construction_state$summaries

      switch(
        construction_state$construction$sign_mode,
        net = list(
          net = matrix(
            summaries[, "positive_summary"] -
              summaries[, "negative_summary"],
            ncol = 1,
            dimnames = list(NULL, "net_summary")
          )
        ),
        separate = list(
          joint = summaries,
          positive = summaries[, "positive_summary", drop = FALSE],
          negative = summaries[, "negative_summary", drop = FALSE]
        ),
        stop(
          "`construction$sign_mode` must be a supported summary construction mode.",
          call. = FALSE
        )
      )
    },
    stop(
      "`construction` must be a supported CPM construction spec.",
      call. = FALSE
    )
  )
}

edge_weights_summary <- function(
  associations,
  cutoffs,
  mask,
  weight_scale = 0
) {
  if (weight_scale == 0) {
    return(matrix(
      as.numeric(mask),
      ncol = length(edge_signs),
      dimnames = list(NULL, edge_signs)
    ))
  }

  positive <- rep(0, length(associations))
  negative <- rep(0, length(associations))

  if (is.finite(cutoffs[["positive"]])) {
    valid <- !is.na(associations) & associations > 0
    positive[valid] <- stats::plogis(
      (associations[valid] - cutoffs[["positive"]]) / weight_scale
    )
  }
  if (is.finite(cutoffs[["negative"]])) {
    valid <- !is.na(associations) & associations < 0
    negative[valid] <- stats::plogis(
      ((-associations[valid]) - cutoffs[["negative"]]) / weight_scale
    )
  }

  cbind(
    positive = positive,
    negative = negative
  )
}

feature_matrix_summary <- function(
  conmat,
  edge_mask,
  edge_weights = NULL
) {
  summaries <- matrix(
    0,
    nrow = nrow(conmat),
    ncol = length(summary_columns),
    dimnames = list(NULL, summary_columns)
  )
  weighted <- !is.null(edge_weights)

  for (edge_sign in edge_signs) {
    if (!weighted) {
      idx <- which(edge_mask[, edge_sign])
    } else {
      idx <- which(edge_weights[, edge_sign] != 0)
    }
    if (!length(idx)) {
      next
    }

    summaries[, summary_column_names[[edge_sign]]] <- if (!weighted) {
      Rfast::rowsums(conmat[, idx, drop = FALSE])
    } else {
      drop(conmat[, idx, drop = FALSE] %*% edge_weights[idx, edge_sign])
    }
  }

  summaries
}

fscale <- function(conmat, center, scale) {
  Rfast::eachrow(Rfast::eachrow(conmat, center, "-"), scale, "/")
}
