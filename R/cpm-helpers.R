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

#' Define CPM summary-construction settings
#'
#' Build the feature-construction portion of a `cpm_spec()`. This helper
#' configures the current summary-construction path by deciding how positive and
#' negative screened edge sets are represented, whether screened edges are
#' additionally weighted before summary construction, and whether edge
#' standardization is applied before constructing subject-level predictors.
#'
#' @param polarity How positive and negative screened information is represented.
#'   `"separate"` constructs `positive_summary` and `negative_summary`, fits a
#'   `joint` stream from both together, and also returns `positive` and
#'   `negative` single-summary diagnostic streams. `"net"` constructs one
#'   `net_summary = positive_summary - negative_summary` feature and returns
#'   a single `net` stream.
#' @param weight_scale Non-negative scale controlling optional sigmoid-style
#'   edge weighting before summary construction. `0` disables additional
#'   weighting and keeps the classic hard-threshold CPM summary. Values greater
#'   than `0` apply the current sigmoid weighting scheme, with larger values
#'   producing a smoother transition around the selection cutoff.
#' @param standardize_edges Logical value indicating if edge values should be
#'   standardized within each training set before CPM feature construction. If
#'   `TRUE`, each edge is centered and scaled to unit variance using the
#'   training data, and the same transformation is applied again at prediction
#'   time. This follows the fold-local edge z-scoring approach described by
#'   Rapuano et al. (2020). Defaults to `FALSE` so the default construction
#'   stays close to the classic CPM summary workflow; set it to `TRUE`
#'   when you want this additional preprocessing step explicitly.
#'
#' @examples
#' cpm_construction_summary()
#' cpm_construction_summary(
#'   polarity = "net",
#'   weight_scale = 0.03
#' )
#' @export
cpm_construction_summary <- function(
  polarity = c("separate", "net"),
  weight_scale = 0,
  standardize_edges = FALSE
) {
  polarity <- match.arg(polarity)
  validate_weight_scale(weight_scale)
  validate_standardize_edges(standardize_edges)

  structure(
    list(
      type = "summary",
      polarity = polarity,
      weight_scale = weight_scale,
      standardize_edges = standardize_edges
    ),
    class = "cpm_construction_spec"
  )
}

#' @export
print.cpm_construction_spec <- function(x, ...) {
  cat("CPM construction (summary):\n")
  cat(sprintf("  Polarity:             %s\n", x$polarity))
  cat(sprintf(
    "  Edge weighting:       %s\n",
    format_weighting_label(x$weight_scale)
  ))
  cat(sprintf(
    "  Weight scale:         %s\n",
    format_weight_scale(x$weight_scale)
  ))
  cat(sprintf(
    "  Edge standardization: %s\n",
    format_edge_standardization(x$standardize_edges)
  ))
  invisible(x)
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

validate_weight_scale <- function(scale) {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      !is.finite(scale) ||
      scale < 0
  ) {
    stop(
      "`weight_scale` must be a single non-negative number.",
      call. = FALSE
    )
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

format_weighting_label <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", "sigmoid")
}

format_weight_scale <- function(weight_scale) {
  ifelse(weight_scale == 0, "none", format_threshold_level(weight_scale))
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
      weight_scale = params$construction$weight_scale,
      standardize_edges = params$construction$standardize_edges,
      model_type = params$model$type
    )
  )
}
