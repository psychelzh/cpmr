#' Define CPM correlation-based edge-selection settings
#'
#' Build the selection portion of a `cpm_spec()`. This helper configures the
#' current correlation-based edge-selection path by choosing the correlation
#' method used to relate each edge to the outcome and how the associated `level`
#' is interpreted.
#'
#' @param method Correlation method used during edge selection. `"pearson"` is
#'   the default CPM path; `"spearman"` can be useful when a monotonic but
#'   non-linear relationship is expected.
#' @param criterion How `level` is interpreted. `"p_value"` thresholds the
#'   absolute correlation by the critical value implied by `level`.
#'   `"absolute"` treats `level` as a direct absolute-correlation cutoff.
#'   `"proportion"` retains a per-sign proportion of positive and negative edges
#'   separately. Because edge counts are discrete and ties at the cutoff are
#'   retained, the realized proportion may be slightly larger than the requested
#'   `level`.
#' @param level Numeric level associated with `criterion`.
#'
#' @examples
#' cpm_selection_cor()
#' cpm_selection_cor(method = "spearman", criterion = "absolute", level = 0.1)
#' @export
cpm_selection_cor <- function(
  method = c("pearson", "spearman"),
  criterion = c("p_value", "absolute", "proportion"),
  level = 0.01
) {
  method <- match.arg(method)
  criterion <- match.arg(criterion)
  validate_selection_level(level)

  structure(
    list(
      type = "cor",
      method = method,
      criterion = criterion,
      level = level
    ),
    class = "cpm_selection_spec"
  )
}

#' @export
print.cpm_selection_spec <- function(x, ...) {
  cat("CPM selection (correlation):\n")
  cat(sprintf("  Method:    %s\n", x$method))
  cat(sprintf("  Criterion: %s\n", x$criterion))
  cat(sprintf("  Level:     %s\n", format_threshold_level(x$level)))
  invisible(x)
}

#' Define CPM network-strength construction settings
#'
#' Build the feature-construction portion of a `cpm_spec()`. This helper
#' configures the current network-strength construction path by deciding how
#' positive and negative screened edge sets are represented, how screened edges
#' are weighted, and whether edge standardization is applied before constructing
#' subject-level predictors.
#'
#' @param polarity How positive and negative screened information is represented.
#'   `"separate"` constructs `positive_strength` and `negative_strength`, fits a
#'   `joint` stream from both together, and also returns `positive` and
#'   `negative` single-strength diagnostic streams. `"net"` constructs one
#'   `net_strength = positive_strength - negative_strength` feature and returns
#'   a single `net` stream.
#' @param weighting Edge-weighting helper created by [cpm_weighting()].
#' @param standardize_edges Logical value indicating if edge strengths should be
#'   standardized within each training set before CPM feature construction. If
#'   `TRUE`, each edge is centered and scaled to unit variance using the
#'   training data, and the same transformation is applied again at prediction
#'   time. This follows the fold-local edge z-scoring approach described by
#'   Rapuano et al. (2020). Defaults to `FALSE` so the default construction
#'   stays close to the classic CPM network-strength workflow; set it to `TRUE`
#'   when you want this additional preprocessing step explicitly.
#'
#' @examples
#' cpm_construction_strength()
#' cpm_construction_strength(
#'   polarity = "net",
#'   weighting = cpm_weighting("sigmoid", scale = 0.03)
#' )
#' @export
cpm_construction_strength <- function(
  polarity = c("separate", "net"),
  weighting = cpm_weighting(),
  standardize_edges = FALSE
) {
  polarity <- match.arg(polarity)
  validate_cpm_component(
    weighting,
    class = "cpm_weighting_spec",
    message = "`weighting` must be created by `cpm_weighting()`."
  )
  validate_standardize_edges(standardize_edges)

  structure(
    list(
      type = "strength",
      polarity = polarity,
      weighting = weighting,
      standardize_edges = standardize_edges
    ),
    class = "cpm_construction_spec"
  )
}

#' @export
print.cpm_construction_spec <- function(x, ...) {
  cat("CPM construction (network strength):\n")
  cat(sprintf("  Polarity:             %s\n", x$polarity))
  cat(sprintf("  Edge weighting:       %s\n", x$weighting$method))
  cat(sprintf(
    "  Weighting scale:      %s\n",
    format_threshold_level(x$weighting$scale)
  ))
  cat(sprintf(
    "  Edge standardization: %s\n",
    format_edge_standardization(x$standardize_edges)
  ))
  invisible(x)
}

#' Define CPM edge-weight settings
#'
#' Build the edge-weighting rule used during CPM feature construction.
#'
#' @param method How edge-level statistics are converted into weights before
#'   CPM feature construction. `"binary"` uses the hard thresholded edge mask.
#'   `"sigmoid"` uses a smooth sigmoid weight centered on the threshold, so
#'   edges closer to or beyond the cutoff contribute more strongly.
#' @param scale Positive scale parameter used when `method = "sigmoid"`.
#'   Smaller values make the weighting curve sharper around the threshold.
#'
#' @examples
#' cpm_weighting("binary")
#' cpm_weighting("sigmoid", scale = 0.03)
#' @export
cpm_weighting <- function(
  method = c("binary", "sigmoid"),
  scale = 0.05
) {
  method <- match.arg(method)
  validate_weighting_scale(scale)

  structure(
    list(
      method = method,
      scale = scale
    ),
    class = "cpm_weighting_spec"
  )
}

#' Define the outcome model used after CPM feature construction
#'
#' Build the second-stage model used after CPM has converted selected edges
#' into subject-level predictors. `cpm_model_lm()` fits an intercept-inclusive
#' linear regression with the CPM-derived features for each prediction stream.
#'
#' @examples
#' cpm_model_lm()
#' @export
cpm_model_lm <- function() {
  structure(
    list(type = "lm"),
    class = "cpm_model_spec"
  )
}

cpm_model_from_params <- function(model_type) {
  switch(
    model_type,
    lm = cpm_model_lm(),
    stop("`model` must be a supported CPM outcome model.", call. = FALSE)
  )
}

validate_cpm_component <- function(x, class, message) {
  if (!inherits(x, class)) {
    stop(message, call. = FALSE)
  }

  invisible(x)
}

validate_selection_level <- function(level) {
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level) ||
      level < 0 ||
      level > 1
  ) {
    stop("`level` must be a single number between 0 and 1.", call. = FALSE)
  }

  invisible(level)
}

validate_weighting_scale <- function(scale) {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      !is.finite(scale) ||
      scale <= 0
  ) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }

  invisible(scale)
}

validate_standardize_edges <- function(standardize_edges) {
  if (
    !is.logical(standardize_edges) ||
      length(standardize_edges) != 1L ||
      is.na(standardize_edges)
  ) {
    stop("`standardize_edges` must be either TRUE or FALSE.", call. = FALSE)
  }

  invisible(standardize_edges)
}

format_model_type <- function(model_type) {
  switch(
    model_type,
    lm = "linear regression",
    model_type
  )
}

flatten_cpm_params <- function(params) {
  extras <- params[setdiff(
    names(params),
    c("selection", "construction", "model")
  )]

  c(
    extras,
    list(
      selection_type = params$selection$type,
      selection_method = params$selection$method,
      selection_criterion = params$selection$criterion,
      selection_level = params$selection$level,
      construction_type = params$construction$type,
      construction_polarity = params$construction$polarity,
      edge_weighting = params$construction$weighting$method,
      weighting_scale = params$construction$weighting$scale,
      standardize_edges = params$construction$standardize_edges,
      model_type = params$model$type
    )
  )
}
