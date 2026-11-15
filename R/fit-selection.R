run_edge_selection <- function(
  conmat,
  behav,
  selection_spec,
  covariate_rank = 0L
) {
  switch(
    selection_spec$type,
    cor = {
      p_value_cutoff <- if (selection_spec$criterion == "p_value") {
        critical_r(nrow(conmat), selection_spec$level, covariate_rank)
      }
      associations <- drop(stats::cor(
        conmat,
        behav,
        method = selection_spec$method
      ))
      associations[!is.finite(associations)] <- NA_real_

      thresholds <- switch(
        selection_spec$criterion,
        p_value = stats::setNames(
          rep(
            p_value_cutoff,
            length(edge_signs)
          ),
          edge_signs
        ),
        absolute = stats::setNames(
          rep(selection_spec$level, length(edge_signs)),
          edge_signs
        ),
        proportion = select_sparsity_thresholds(
          associations = associations,
          proportion = selection_spec$level
        ),
        stop(
          paste(
            "`criterion` must be one of",
            "\"p_value\", \"absolute\", or \"proportion\"."
          )
        )
      )

      list(
        associations = associations,
        thresholds = thresholds,
        mask = edge_mask_from_cutoffs(
          associations = associations,
          cutoffs = thresholds
        )
      )
    },
    stop("`selection` must be a supported CPM selection spec.", call. = FALSE)
  )
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

critical_r <- function(n, alpha, covariate_rank = 0L) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  df <- n - 2 - covariate_rank
  if (df <= 0L) {
    stop(
      "P-value edge selection requires positive residual degrees of freedom after covariate adjustment.",
      call. = FALSE
    )
  }
  if (alpha == 0) {
    return(Inf)
  }

  ct <- stats::qt(alpha / 2, df, lower.tail = FALSE)
  sqrt((ct^2) / ((ct^2) + df))
}
