#' Collect stored CPM edge masks
#'
#' @param x An object with stored CPM edges.
#' @param type How to summarize resample-level edge extracts. `type = "sum"`
#'   counts how often each edge was selected, `type = "prop"` converts those
#'   counts to fold-wise proportions, and `type = "all"` returns the raw edge
#'   masks from each fold. Ignored for single fitted CPM objects.
#' @param selected_only Should outputs keep only selected edges? Ignored for
#'   single fitted CPM objects.
#' @param ... Unused.
#'
#' @return Stored CPM edge masks.
#' @export
collect_edges <- function(
  x,
  type = c("sum", "prop", "all"),
  selected_only = TRUE,
  ...
) {
  UseMethod("collect_edges")
}

#' @export
collect_edges.cpm_fit <- function(x, ...) {
  x$edges
}

#' @export
collect_edges.cpm_edge_extract <- function(x, ...) {
  x$edges
}

#' @export
collect_edges.model_fit <- function(x, ...) {
  if (!inherits(x$fit, "cpm_fit")) {
    stop(
      "`collect_edges()` only supports `model_fit` objects created by the cpmr engine."
    )
  }

  collect_edges(x$fit, ...)
}

#' @export
collect_edges.workflow <- function(x, ...) {
  rlang::check_installed("workflows")
  collect_edges(workflows::extract_fit_parsnip(x), ...)
}

#' @export
collect_edges.resample_results <- function(
  x,
  type = c("sum", "prop", "all"),
  selected_only = TRUE,
  ...
) {
  type <- match.arg(type)
  selected_only <- validate_selected_only(selected_only)

  extracts <- collect_cpm_edge_extracts(x)
  resample_id_cols <- cpm_resample_id_cols(extracts)
  expanded <- expand_cpm_edge_extracts(
    extracts = extracts,
    meta_cols = character(),
    resample_id_cols = resample_id_cols,
    type = type,
    selected_only = selected_only
  )

  if (type == "all") {
    expanded
  } else {
    summarize_cpm_edge_extracts(
      expanded = expanded,
      meta_cols = character(),
      resample_id_cols = resample_id_cols,
      type = type,
      selected_only = selected_only
    )
  }
}

#' @export
collect_edges.tune_results <- function(
  x,
  type = c("sum", "prop", "all"),
  selected_only = TRUE,
  ...
) {
  type <- match.arg(type)
  selected_only <- validate_selected_only(selected_only)

  extracts <- collect_cpm_edge_extracts(x)
  resample_id_cols <- cpm_resample_id_cols(extracts)
  meta_cols <- setdiff(names(extracts), c(resample_id_cols, ".extracts"))
  expanded <- expand_cpm_edge_extracts(
    extracts = extracts,
    meta_cols = meta_cols,
    resample_id_cols = resample_id_cols,
    type = type,
    selected_only = selected_only
  )

  if (type == "all") {
    expanded
  } else {
    summarize_cpm_edge_extracts(
      expanded = expanded,
      meta_cols = meta_cols,
      resample_id_cols = resample_id_cols,
      type = type,
      selected_only = selected_only
    )
  }
}

#' @export
collect_edges.default <- function(x, ...) {
  stop(
    paste(
      "`collect_edges()` supports fitted CPM model objects and tidymodels",
      "resample results created with `extract_cpm_edges()`."
    )
  )
}

validate_selected_only <- function(selected_only) {
  if (
    !is.logical(selected_only) ||
      length(selected_only) != 1L ||
      is.na(selected_only)
  ) {
    stop("`selected_only` must be `TRUE` or `FALSE`.")
  }

  selected_only
}

collect_cpm_edge_extracts <- function(x) {
  rlang::check_installed("tune")

  extracts <- tryCatch(
    tune::collect_extracts(x),
    error = function(cnd) {
      stop(cpm_edge_extracts_error_message(), call. = FALSE)
    }
  )
  if (!nrow(extracts) || !".extracts" %in% names(extracts)) {
    stop(cpm_edge_extracts_error_message())
  }

  is_cpm_extract <- vapply(
    extracts$.extracts,
    function(item) inherits(item, "cpm_edge_extract"),
    logical(1)
  )
  if (!all(is_cpm_extract)) {
    stop(cpm_edge_extracts_error_message())
  }

  extracts
}

cpm_resample_id_cols <- function(extracts) {
  grep("^id[0-9]*$", names(extracts), value = TRUE)
}

expand_cpm_edge_extracts <- function(
  extracts,
  meta_cols,
  resample_id_cols,
  type,
  selected_only
) {
  rows <- lapply(seq_len(nrow(extracts)), function(i) {
    payload <- extracts$.extracts[[i]]
    out <- tibble::tibble(
      predictor = payload$predictors,
      pos = payload$edges[, "pos"],
      neg = payload$edges[, "neg"]
    )

    if (type == "all" && selected_only) {
      out <- out[out$pos | out$neg, , drop = FALSE]
    }

    header_cols <- c(meta_cols, resample_id_cols)
    if (length(header_cols)) {
      header <- extracts[i, header_cols, drop = FALSE]
      if (!nrow(out)) {
        header <- header[0, , drop = FALSE]
      } else {
        header <- header[rep(1L, nrow(out)), , drop = FALSE]
      }
      out <- tibble::as_tibble(cbind(header, out))
    }

    out
  })

  if (!length(rows)) {
    return(tibble::tibble())
  }

  tibble::as_tibble(do.call(rbind, rows))
}

summarize_cpm_edge_extracts <- function(
  expanded,
  meta_cols,
  resample_id_cols,
  type,
  selected_only
) {
  if (!nrow(expanded)) {
    out <- unique(expanded[, c(meta_cols, "predictor"), drop = FALSE])
    out$pos <- numeric(0)
    out$neg <- numeric(0)
    out$n_folds <- integer(0)
    return(tibble::as_tibble(out))
  }

  group_cols <- c(meta_cols, "predictor")
  group_key <- do.call(
    interaction,
    c(expanded[group_cols], list(drop = TRUE, lex.order = TRUE))
  )
  groups <- split(seq_len(nrow(expanded)), group_key)

  rows <- lapply(groups, function(idx) {
    first <- expanded[idx[1], group_cols, drop = FALSE]
    fold_key <- if (length(resample_id_cols)) {
      do.call(
        interaction,
        c(
          expanded[idx, resample_id_cols, drop = FALSE],
          list(drop = TRUE, lex.order = TRUE)
        )
      )
    } else {
      seq_along(idx)
    }
    data.frame(
      first,
      pos = sum(expanded$pos[idx]),
      neg = sum(expanded$neg[idx]),
      n_folds = length(unique(fold_key)),
      row.names = NULL
    )
  })

  out <- tibble::as_tibble(do.call(rbind, rows))
  if (type == "prop") {
    out$pos <- out$pos / out$n_folds
    out$neg <- out$neg / out$n_folds
  }
  if (selected_only) {
    out <- out[(out$pos > 0) | (out$neg > 0), , drop = FALSE]
  }

  out
}

cpm_edge_extracts_error_message <- function() {
  paste(
    "`collect_edges()` requires resample results created with",
    "`control_resamples(extract = extract_cpm_edges)` or",
    "`control_grid(extract = extract_cpm_edges)`."
  )
}
