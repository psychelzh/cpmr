resolve_resample_folds <- function(resamples, include_cases) {
  if (length(include_cases) < 2L) {
    stop(
      "At least 2 complete-case observations are required for resampling.",
      call. = FALSE
    )
  }

  if (is.null(resamples)) {
    n_folds <- length(include_cases)
    folds <- make_kfold_assessment_folds(include_cases, n_folds)
  } else if (is.list(resamples)) {
    folds <- normalize_manual_resamples(resamples, include_cases)
  } else {
    n_folds <- normalize_resample_count(resamples)
    if (n_folds > length(include_cases)) {
      stop(
        paste0(
          "`resamples` as a fold count must be less than or equal to ",
          "complete-case observations."
        )
      )
    }
    folds <- make_kfold_assessment_folds(include_cases, n_folds)
  }

  folds
}

make_kfold_assessment_folds <- function(x, k) {
  split(sample(x), cut(seq_along(x), breaks = k, labels = FALSE))
}

normalize_manual_resamples <- function(resamples, include_cases) {
  if (!is.list(resamples) || length(resamples) == 0L) {
    stop("`resamples` must be a non-empty list of assessment indices.")
  }
  if (length(resamples) < 2L) {
    stop("`resamples` must contain at least 2 assessment sets.")
  }

  normalized <- lapply(resamples, function(idx) {
    if (!is.numeric(idx) || anyNA(idx) || any(!is.finite(idx))) {
      stop("Each element in `resamples` must contain finite numeric indices.")
    }
    if (any(idx %% 1 != 0)) {
      stop("Each element in `resamples` must contain integer-valued indices.")
    }

    idx <- as.integer(idx)
    if (any(idx <= 0L)) {
      stop("Each element in `resamples` must contain positive indices.")
    }
    if (anyDuplicated(idx)) {
      stop("Each element in `resamples` must not contain duplicates.")
    }

    idx
  })

  all_assessment <- unlist(normalized, use.names = FALSE)
  include_cases <- sort(unique(include_cases))

  if (!all(all_assessment %in% include_cases)) {
    stop("All `resamples` indices must be contained in complete-case rows.")
  }
  if (length(all_assessment) != length(unique(all_assessment))) {
    stop("`resamples` indices must not overlap across folds.")
  }
  if (!identical(sort(all_assessment), include_cases)) {
    stop("`resamples` indices must cover all complete-case rows exactly once.")
  }

  normalized
}

normalize_resample_count <- function(resamples) {
  if (
    !is.numeric(resamples) ||
      length(resamples) != 1L ||
      is.na(resamples) ||
      !is.finite(resamples) ||
      resamples < 2 ||
      resamples %% 1 != 0
  ) {
    stop(
      paste0(
        "`resamples` must be NULL, a single integer greater than or equal ",
        "to 2, or a non-empty list of assessment indices."
      )
    )
  }

  as.integer(resamples)
}

warn_large_edge_storage <- function(n_edges, n_folds, return_edges) {
  if (return_edges != "all") {
    return(invisible())
  }

  estimated_bytes <- as.double(n_edges) * length(edge_signs) * n_folds * 4
  threshold_bytes <- 10 * 1024^2
  if (estimated_bytes > threshold_bytes) {
    warning(
      sprintf(
        paste0(
          "Storing fold-wise edges (`return_edges = \"all\"`) may consume ",
          "large memory (~%.1f MB). Consider `return_edges = \"sum\"` or ",
          "`return_edges = \"none\"` when edge storage is optional."
        ),
        estimated_bytes / 1024^2
      )
    )
  }

  invisible()
}

init_edge_storage <- function(return_edges, conmat, n_folds) {
  switch(
    return_edges,
    all = array(
      dim = c(dim(conmat)[2], length(edge_signs), n_folds),
      dimnames = list(NULL, edge_signs, NULL)
    ),
    sum = array(
      0,
      dim = c(dim(conmat)[2], length(edge_signs)),
      dimnames = list(NULL, edge_signs)
    ),
    none = NULL
  )
}

update_edge_storage <- function(edges, return_edges, fold, edge_mask) {
  switch(
    return_edges,
    all = {
      edges[,, fold] <- edge_mask
      edges
    },
    sum = edges + edge_mask,
    none = edges
  )
}
