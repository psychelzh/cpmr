resolve_assessment_folds <- function(resamples, include_cases) {
  if (length(include_cases) < 2L) {
    stop(
      "CPM requires at least 2 complete-case observations to define assessment folds.",
      call. = FALSE
    )
  }

  if (is.null(resamples)) {
    n_folds <- length(include_cases)
    folds <- make_kfold_assessment_folds(include_cases, n_folds)
  } else if (is.list(resamples)) {
    folds <- normalize_manual_folds(resamples, include_cases)
  } else {
    n_folds <- normalize_fold_count(resamples)
    if (n_folds > length(include_cases)) {
      stop(
        paste0(
          "The requested fold count must be less than or equal to the ",
          "number of complete-case observations. The default is 10; ",
          "specify a smaller `resamples` value or use `resamples = NULL`."
        ),
        call. = FALSE
      )
    }
    folds <- make_kfold_assessment_folds(include_cases, n_folds)
  }

  if (any(length(include_cases) - lengths(folds) < 3L)) {
    stop(
      "Each assessment fold must leave at least 3 complete-case observations for training.",
      call. = FALSE
    )
  }

  folds
}

make_kfold_assessment_folds <- function(x, k) {
  split(sample(x), cut(seq_along(x), breaks = k, labels = FALSE))
}

normalize_manual_folds <- function(resamples, include_cases) {
  if (!is.list(resamples) || length(resamples) == 0L) {
    stop("`resamples` must be a non-empty list of assessment indices.")
  }
  if (length(resamples) < 2L) {
    stop("`resamples` must contain at least 2 assessment sets.")
  }

  normalized <- lapply(resamples, function(idx) {
    if (length(idx) == 0L) {
      stop("Each element in `resamples` must contain assessment indices.")
    }
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

normalize_fold_count <- function(resamples) {
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
